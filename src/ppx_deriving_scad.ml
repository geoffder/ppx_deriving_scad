open! Ppxlib
open! Base
open! Ast_builder.Default

let attrs_to_string =
  List.fold ~init:"" ~f:(fun acc { attr_name = { txt; _ }; _ } -> acc ^ " " ^ txt)

(* standin example. I'll want to use attributes to indicate unit vectors for instance,
   preventing them from being translated *)
let unit_attr =
  Attribute.declare
    "unit_vec"
    Attribute.Context.label_declaration
    (* Ast_pattern.(single_expr_payload __) *)
    Ast_pattern.(pstr nil)
    (* (fun expr -> expr) *)
    (* (fun _expr -> "don't translate this") *)
    ()

(* let attributes = [ Attribute.T default_attribute ] *)

type transform =
  | Translate
  | Scale
  | Rotate

let transform_to_name is_unit transform =
  match is_unit, transform with
  | _, Translate -> "translate"
  | _, Scale     -> "scale"
  | _, Rotate    -> "rotate"

let ld_to_fun_id (ld : label_declaration) name =
  let qualifiers =
    let rec last acc = function
      | [ n ]  ->
        (if String.equal n "t" then name else Printf.sprintf "%s_%s" name n) :: acc
      | h :: t -> last (h :: acc) t
      | []     -> failwith "inaccessible"
    in
    string_of_core_type ld.pld_type |> String.split ~on:'.' |> last [] |> List.rev
  in
  match qualifiers with
  | h :: t -> List.fold_left ~init:(lident h) ~f:(fun li m -> Longident.Ldot (li, m)) t
  | []     -> failwith "inaccessible"

let record_entry ~name ~params (ld : label_declaration) =
  let loc = ld.pld_loc in
  let id = ld_to_fun_id ld name
  and params =
    List.fold
      ~init:
        [ ( Nolabel
          , pexp_field
              ~loc
              (pexp_ident ~loc { loc; txt = lident "t" })
              { loc; txt = lident ld.pld_name.txt } )
        ]
      ~f:(fun ps p -> (Nolabel, pexp_ident ~loc { loc; txt = lident p }) :: ps)
      params
  in
  ( { loc; txt = lident ld.pld_name.txt }
  , pexp_apply ~loc (pexp_ident ~loc { loc; txt = id }) params )

let build_fun ~loc ~params expr =
  let f expr txt = pexp_fun ~loc Nolabel None (ppat_var ~loc { loc; txt }) expr in
  List.fold ~init:expr ~f (List.rev params)

let record_transformer ~loc ~name ~params (td : type_declaration) fields =
  let oc = Stdio.Out_channel.create ("../../" ^ name ^ "_logs") in
  let label_attrs =
    List.fold
      ~init:""
      ~f:(fun acc (d : label_declaration) ->
        let a = Attribute.get unit_attr d in
        Printf.sprintf
          "%s%s -> %s // has unit? - %s\n"
          acc
          d.pld_name.txt
          (attrs_to_string d.pld_attributes)
          (Bool.to_string (Option.is_some a)) )
      fields
  in
  Stdio.Out_channel.fprintf oc "%s attributes:\n%s" name label_attrs;
  Stdio.Out_channel.close oc;
  let name =
    if String.equal td.ptype_name.txt "t"
    then name
    else Printf.sprintf "%s_%s" name td.ptype_name.txt
  and f = record_entry ~name ~params in
  pstr_value
    ~loc
    Nonrecursive
    [ { pvb_pat = ppat_var ~loc { loc; txt = name }
      ; pvb_expr =
          build_fun
            ~loc
            ~params
            (pexp_fun
               ~loc
               Nolabel
               None
               (ppat_var ~loc { loc; txt = "t" })
               (pexp_record ~loc (List.map ~f fields) None) )
      ; pvb_attributes = []
      ; pvb_loc = loc
      }
    ]

let translator loc = record_transformer ~loc ~name:"translate" ~params:[ "p" ]
let scaler loc = record_transformer ~loc ~name:"scale" ~params:[ "s" ]

let accessor_intf ~ptype_name (ld : label_declaration) =
  let loc = ld.pld_loc in
  psig_value
    ~loc
    { pval_name = ld.pld_name
    ; pval_type =
        ptyp_arrow
          ~loc
          Nolabel
          (ptyp_constr ~loc { loc; txt = lident ptype_name.txt } [])
          ld.pld_type
    ; pval_attributes = []
    ; pval_loc = loc
    ; pval_prim = []
    }

let transformer_impl ~ctxt (_rec_flag, type_declarations) =
  let oc = Stdio.Out_channel.create "../../transformer_logs" in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
      Location.raise_errorf ~loc "Cannot derive translators for non record types (yet)"
    | { ptype_kind = Ptype_record fields; ptype_name; ptype_attributes; _ } ->
      Stdio.Out_channel.fprintf
        oc
        "%s attributes: %s\n"
        ptype_name.txt
        (attrs_to_string ptype_attributes);
      [ translator loc td fields; scaler loc td fields ]
  in
  let a = List.concat_map ~f type_declarations in
  Stdio.Out_channel.close oc;
  a

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
        Location.raise_errorf ~loc "Cannot derive accessors for non record types"
      | { ptype_kind = Ptype_record fields; ptype_name; _ } ->
        List.map fields ~f:(accessor_intf ~ptype_name) )
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg transformer_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let my_deriver =
  Deriving.add ~str_type_decl:impl_generator ~sig_type_decl:intf_generator "my_deriver"
