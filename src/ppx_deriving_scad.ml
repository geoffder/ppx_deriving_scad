open! Ppxlib
open! Base
open! Ast_builder.Default

let unit_attr =
  Attribute.declare
    "scad.unit"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let ignore_attr =
  Attribute.declare
    "scad.ignore"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

type transform =
  | Translate
  | Scale
  | Rotate
  | RotateAbout
  | Quaternion
  | QuaternionAbout
  | Mirror

let transforms =
  [ Translate; Scale; Rotate; RotateAbout; Quaternion; QuaternionAbout; Mirror ]

let transform_to_string = function
  | Translate       -> "translate"
  | Scale           -> "scale"
  | Rotate          -> "rotate"
  | RotateAbout     -> "rotate_about_pt"
  | Quaternion      -> "quaternion"
  | QuaternionAbout -> "quaternion_about_pt"
  | Mirror          -> "mirror"

let transform_to_rev_params = function
  | Translate       -> [ "p" ]
  | Scale           -> [ "s" ]
  | Rotate          -> [ "r" ]
  | RotateAbout     -> [ "p"; "r" ]
  | Quaternion      -> [ "q" ]
  | QuaternionAbout -> [ "p"; "q" ]
  | Mirror          -> [ "ax" ]

let transform_drop_about = function
  | RotateAbout     -> Rotate
  | QuaternionAbout -> Quaternion
  | trans           -> trans

let transform_to_names is_unit transform =
  match is_unit, transform with
  | true, (Translate | Scale) -> None
  | false, trans              -> Some
                                   ( transform_to_string trans
                                   , transform_to_rev_params trans )
  | true, trans               ->
    let trans' = transform_drop_about trans in
    Some (transform_to_string trans', transform_to_rev_params trans')

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
  and ignored = Option.is_some @@ Attribute.get ignore_attr ld
  and field_id = { loc; txt = lident ld.pld_name.txt } in
  let field_expr = pexp_field ~loc (pexp_ident ~loc { loc; txt = lident "t" }) field_id in
  match Option.(transform_to_names is_unit transform >>= some_if (not ignored)) with
  | Some (name, params) ->
    let id = ld_to_fun_id ld name
    and params =
      List.fold
        ~init:[ Nolabel, field_expr ]
        ~f:(fun ps p -> (Nolabel, pexp_ident ~loc { loc; txt = lident p }) :: ps)
        params
    in
    field_id, pexp_apply ~loc (pexp_ident ~loc { loc; txt = id }) params
  | None                -> field_id, field_expr

let build_fun ~loc ~params expr =
  let f expr txt = pexp_fun ~loc Nolabel None (ppat_var ~loc { loc; txt }) expr in
  List.fold ~init:expr ~f params

let record_transformer ~loc ~transform (td : type_declaration) fields =
  let name =
    let func_name = transform_to_string transform in
    if String.equal td.ptype_name.txt "t"
    then func_name
    else Printf.sprintf "%s_%s" func_name td.ptype_name.txt
  and f = record_entry ~transform in
  pstr_value
    ~loc
    Nonrecursive
    [ { pvb_pat = ppat_var ~loc { loc; txt = name }
      ; pvb_expr =
          build_fun
            ~loc
            ~params:(transform_to_rev_params transform)
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

let transformer_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
      Location.raise_errorf
        ~loc
        "Deriving scad transformers for non-record types is not supported."
    | { ptype_kind = Ptype_record fields; _ } ->
      List.map
        ~f:(fun transform -> record_transformer ~loc ~transform td fields)
        transforms
  in
  List.concat_map ~f type_declarations

let module_type_arrow ~loc name =
  ptyp_arrow
    ~loc
    Nolabel
    (ptyp_constr ~loc { loc; txt = Longident.Ldot (lident name, "t") } [])

let transformer_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
      Location.raise_errorf
        ~loc
        "Deriving scad transformers for non-record types is not supported."
    | { ptype_kind = Ptype_record _; ptype_name; ptype_params; _ } ->
      let f transform =
        let name =
          let func_name = transform_to_string transform in
          if String.equal td.ptype_name.txt "t"
          then func_name
          else Printf.sprintf "%s_%s" func_name td.ptype_name.txt
        and last_arrow =
          let typ =
            ptyp_constr
              ~loc
              { loc; txt = lident ptype_name.txt }
              (List.map ~f:fst ptype_params)
          in
          ptyp_arrow ~loc Nolabel typ typ
        in
        let pval_type =
          match transform with
          | RotateAbout     ->
            let arrow = module_type_arrow ~loc "Vec3" in
            arrow @@ arrow last_arrow
          | Quaternion      -> module_type_arrow ~loc "Quaternion" @@ last_arrow
          | QuaternionAbout ->
            module_type_arrow ~loc "Quaternion"
            @@ module_type_arrow ~loc "Vec3"
            @@ last_arrow
          | _               -> module_type_arrow ~loc "Vec3" @@ last_arrow
        in
        psig_value
          ~loc
          { pval_name = { loc; txt = name }
          ; pval_type
          ; pval_attributes = []
          ; pval_loc = loc
          ; pval_prim = []
          }
      in
      List.map ~f transforms
  in
  List.concat_map ~f type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg transformer_impl
let intf_generator = Deriving.Generator.V2.make_noarg transformer_intf
let scad = Deriving.add ~str_type_decl:impl_generator ~sig_type_decl:intf_generator "scad"
