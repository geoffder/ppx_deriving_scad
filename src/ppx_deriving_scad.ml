open! Ppxlib
module List = ListLabels
open! Ast_builder.Default

(* standin example. I'll want to use attributes to indicate unit vectors for instance,
   preventing them from being translated *)
let default_attribute =
  Attribute.declare
    "ppx_deriving_scad.unit"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let attributes = [ Attribute.T default_attribute ]

module Vec3 = struct
  type t = float * float * float

  let translate (px, py, pz) (tx, ty, tz) = px +. tx, py +. ty, pz +. tz
end

type vec = { a : Vec3.t }

let translator loc (td : type_declaration) fields =
  let names =
    let qualified = string_of_core_type (List.hd fields).pld_type in
    let mods, name =
      let rec last acc = function
        | [ n ]  -> acc, n
        | h :: t -> last (h :: acc) t
        | []     -> [], ""
      in
      String.split_on_char '.' qualified |> last []
    in
    let name' =
      if String.equal name "t" then "translate" else Printf.sprintf "%s_translate" name
    in
    List.rev (name' :: mods)
  in
  let id =
    match names with
    | h :: t -> List.fold_left ~init:(lident h) ~f:(fun li m -> Longident.Ldot (li, m)) t
    | []     -> failwith "inaccesible"
  in
  let field_trans (ld : label_declaration) =
    let loc = ld.pld_loc in
    ( { loc; txt = lident ld.pld_name.txt }
    , pexp_apply
        ~loc
        (pexp_ident ~loc { loc; txt = id })
        [ Nolabel, pexp_ident ~loc { loc; txt = lident "p" }
        ; ( Nolabel
          , pexp_field
              ~loc
              (pexp_ident ~loc { loc; txt = lident "t" })
              { loc; txt = lident ld.pld_name.txt } )
        ] )
  in
  let name =
    if String.equal td.ptype_name.txt "t"
    then "translate"
    else Printf.sprintf "%s_translate" td.ptype_name.txt
  in
  pstr_value
    ~loc
    Nonrecursive
    [ { pvb_pat = ppat_var ~loc { loc; txt = name }
      ; pvb_expr =
          pexp_fun
            ~loc
            Nolabel
            None
            (ppat_var ~loc { loc; txt = "p" })
            (pexp_fun
               ~loc
               Nolabel
               None
               (ppat_var ~loc { loc; txt = "t" })
               (pexp_record ~loc (List.map ~f:field_trans fields) None) )
      ; pvb_attributes = []
      ; pvb_loc = loc
      }
    ]

(* TODO: can I access a field in x using the name of the field with metaquot? *)
(* This spits out what is on the other side of the equals at the moment. So I get
     "no such pexp_field value" etc. How can I make this into an expression bound
     to the left side. *)
(* [%stri
 *   let [%p Ast.pvar ld.pld_name.txt] =
 *     [%expr pexp_field ~loc x { loc; txt = lident ld.pld_name.txt }]] *)

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

let translator_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
      Location.raise_errorf ~loc "Cannot derive translators for non record types (yet)"
    | { ptype_kind = Ptype_record fields; _ } -> [ translator loc td fields ]
  in
  List.concat_map ~f type_declarations

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
        Location.raise_errorf ~loc "Cannot derive accessors for non record types"
      | { ptype_kind = Ptype_record fields; ptype_name; _ } ->
        List.map fields ~f:(accessor_intf ~ptype_name) )
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg translator_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let my_deriver =
  Deriving.add ~str_type_decl:impl_generator ~sig_type_decl:intf_generator "my_deriver"
