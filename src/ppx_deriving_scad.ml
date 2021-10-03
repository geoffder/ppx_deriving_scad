open! Ppxlib
open! Base
open! Ast_builder.Default

let attrs_to_string =
  List.fold ~init:"" ~f:(fun acc { attr_name = { txt; _ }; _ } -> acc ^ " " ^ txt)

(* standin example. I'll want to use attributes to indicate unit vectors for instance,
   preventing them from being translated *)
let unit_attr =
  Attribute.declare
    "unit"
    Attribute.Context.label_declaration
    (* Ast_pattern.(single_expr_payload __) *)
    Ast_pattern.(pstr nil)
    (* (fun expr -> expr) *)
    (* (fun _expr -> "don't translate this") *)
    ()

(* let attributes = [ Attribute.T default_attribute ] *)

(* TODO: This works, but rethink it. Need to cleanly represent the transforms with
   types while allowing seamless adaptation to attributes (such as unit). Both name
   and params need to change when translation functions are avoided for example.
   Also, with inclusion of and @ignore attribute, should consider moving is_unit
   (and ignore along with it) out of the janky names_params function here. *)

type transform =
  | Translate
  | Scale
  | Rotate
  | RotateAbout

let transform_to_string = function
  | Translate   -> "translate"
  | Scale       -> "scale"
  | Rotate      -> "rotate"
  | RotateAbout -> "rotate_about_pt"

let transform_to_params = function
  | Translate   -> [ "p" ]
  | Scale       -> [ "s" ]
  | Rotate      -> [ "r" ]
  | RotateAbout -> [ "r"; "p" ]

let transform_to_name_params is_unit transform =
  match is_unit, transform with
  | false, Translate   -> Some ("translate", [ "p" ])
  | true, Translate    -> None
  | _, Scale           -> Some ("scale", [ "s" ])
  | _, Rotate          -> Some ("rotate", [ "r" ])
  | false, RotateAbout -> Some ("rotate_about_pt", [ "r"; "p" ])
  | true, RotateAbout  -> Some ("rotate", [ "r" ])

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

let record_entry ~transform (ld : label_declaration) =
  let loc = ld.pld_loc in
  let is_unit = Option.is_some @@ Attribute.get unit_attr ld
  and field_id = { loc; txt = lident ld.pld_name.txt } in
  let field_expr = pexp_field ~loc (pexp_ident ~loc { loc; txt = lident "t" }) field_id in
  match transform_to_name_params is_unit transform with
  | Some (name, params) ->
    let id = ld_to_fun_id ld name
    and params =
      List.fold
        ~init:[ Nolabel, field_expr ]
        ~f:(fun ps p -> (Nolabel, pexp_ident ~loc { loc; txt = lident p }) :: ps)
        (List.rev params)
    in
    field_id, pexp_apply ~loc (pexp_ident ~loc { loc; txt = id }) params
  | None                -> field_id, field_expr

let build_fun ~loc ~params expr =
  let f expr txt = pexp_fun ~loc Nolabel None (ppat_var ~loc { loc; txt }) expr in
  List.fold ~init:expr ~f (List.rev params)

let record_transformer ~loc ~transform (td : type_declaration) fields =
  let name = transform_to_string transform in
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
  and f = record_entry ~transform in
  pstr_value
    ~loc
    Nonrecursive
    [ { pvb_pat = ppat_var ~loc { loc; txt = name }
      ; pvb_expr =
          build_fun
            ~loc
            ~params:(transform_to_params transform)
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

let translator loc = record_transformer ~loc ~transform:Translate
let scaler loc = record_transformer ~loc ~transform:Scale
let rotater loc = record_transformer ~loc ~transform:Rotate
let rotate_abouter loc = record_transformer ~loc ~transform:RotateAbout

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
      [ translator loc td fields
      ; scaler loc td fields
      ; rotater loc td fields
      ; rotate_abouter loc td fields
      ]
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
