open! Ppxlib
open! Base
open! Ast_builder.Default

(* TODO:
   - support for 2d and 3d types in gadt version of scad-ml?
   - support for non-record types. Would be nice to have for naked Maps
     (Columns.t being the first one from the bottom up in dometyl)
*)

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

let map_attr =
  Attribute.declare
    "scad.map"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let mapf_attr =
  Attribute.declare
    "scad.mapf"
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

let option_map ~loc expr =
  [%expr
    function
    | Some opt -> Some ([%e expr] opt)
    | None     -> None]

let result_map ~loc expr =
  [%expr
    function
    | Ok ok -> Ok ([%e expr] ok)
    | err   -> err]

let list_map ~loc expr =
  [%expr
    let rec aux acc = function
      | h :: t -> aux ([%e expr] h :: acc) t
      | []     -> acc
    in
    aux []]

let is_map s = Str.string_match (Str.regexp "Map") s 0

let rec is_lid_map = function
  | Lident s      -> is_map s
  | Ldot (p, _)   -> is_lid_map p
  | Lapply (a, b) -> is_lid_map a || is_lid_map b

let lid_contains lid name =
  let str s = Str.string_match (Str.regexp name) s 0 in
  let rec aux = function
    | Lident s      -> str s
    | Ldot (p, _)   -> aux p
    | Lapply (a, b) -> aux a || aux b
  in
  aux lid

let fun_id name lid =
  let maybe_suffix s =
    if String.equal s "t" then name else Printf.sprintf "%s_%s" name s
  in
  match lid with
  | Lident s    -> Lident (maybe_suffix s)
  | Ldot (p, s) -> Ldot (p, maybe_suffix s)
  | Lapply _    -> assert false

let rec is_jane_map = function
  | Lident s      -> String.equal "Map" s
  | Ldot (p, _)   -> is_jane_map p
  | Lapply (a, b) -> is_jane_map a || is_jane_map b

let map ~lid ~jane ~loc expr =
  let lid = if jane && is_jane_map lid then Longident.Ldot (lident "Map", "t") else lid in
  let id = pexp_ident ~loc @@ { loc; txt = fun_id "map" lid } in
  if jane then [%expr [%e id] ~f:[%e expr]] else [%expr [%e id] [%e expr]]

let transform_expr ~transform (ld : label_declaration) =
  let loc = ld.pld_loc
  and is_unit = Option.is_some @@ Attribute.get unit_attr ld
  and ignored = Option.is_some @@ Attribute.get ignore_attr ld
  and mappable =
    if Option.is_some @@ Attribute.get map_attr ld
    then Some false
    else if Option.is_some @@ Attribute.get mapf_attr ld
    then Some true
    else None
  and is_constr = function
    | { ptyp_desc = Ptyp_constr _; _ } -> true
    | _ -> false
  in
  let f (name, params) =
    let inner_expr name lid =
      let params =
        List.fold
          ~init:[]
          ~f:(fun ps p -> (Nolabel, pexp_ident ~loc { loc; txt = lident p }) :: ps)
          params
      and txt = fun_id name lid in
      pexp_apply ~loc (pexp_ident ~loc { loc; txt }) params
    in
    let rec exprs_of_typ funcs next =
      match next with
      | [%type: [%t? typ] option] | [%type: [%t? typ] Option.t] ->
        exprs_of_typ (option_map :: funcs) typ
      | [%type: [%t? typ] list] | [%type: [%t? typ] List.t] ->
        exprs_of_typ (list_map :: funcs) typ
      | [%type: ([%t? typ], [%t? _]) result] | [%type: ([%t? typ], [%t? _]) Result.t] ->
        exprs_of_typ (result_map :: funcs) typ
      | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, []); _ } ->
        inner_expr name lid, funcs
      | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, (arg :: _ as args)); _ } ->
        if List.for_all ~f:(Fn.non is_constr) args
        then inner_expr name lid, funcs
        else (
          let funcs' =
            match mappable with
            | Some jane -> map ~lid ~jane :: funcs
            | None      -> funcs
          in
          exprs_of_typ funcs' arg )
      | { ptyp_desc = Ptyp_poly (_, typ); _ } ->
        Location.raise_errorf ~loc "Fail on poly: %s" (string_of_core_type typ)
      | { ptyp_desc = Ptyp_var name; _ } ->
        Location.raise_errorf ~loc "Fail on Ptyp_var: %s" name
      | _ -> Location.raise_errorf ~loc "exprs_of_typ failure"
    in
    let expr, maps = exprs_of_typ [] ld.pld_type in
    List.fold ~f:(fun expr m -> [%expr [%e m ~loc expr]]) ~init:expr maps
  in
  Option.(
    value_map
      ~f
      ~default:[%expr fun a -> a]
      (transform_to_names is_unit transform >>= some_if (not ignored)))

let record_entry ~transform (ld : label_declaration) =
  let loc = ld.pld_loc in
  let field_id = { loc; txt = lident ld.pld_name.txt } in
  let field_expr = pexp_field ~loc (pexp_ident ~loc { loc; txt = lident "t" }) field_id in
  let expr = transform_expr ~transform ld in
  field_id, [%expr [%e expr] [%e field_expr]]

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

let scad_type_arrow ~loc name =
  let txt = Longident.Ldot (Longident.Ldot (lident "Scad_ml", name), "t") in
  ptyp_arrow ~loc Nolabel (ptyp_constr ~loc { loc; txt } [])

let transformer_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
      Location.raise_errorf
        ~loc
        "Deriving scad transformers for non-record types is not supported."
    | { ptype_kind = Ptype_record _; ptype_name; ptype_params; _ } ->
      let gen_sig transform =
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
            let arrow = scad_type_arrow ~loc "Vec3" in
            arrow @@ arrow last_arrow
          | Quaternion      -> scad_type_arrow ~loc "Quaternion" @@ last_arrow
          | QuaternionAbout ->
            scad_type_arrow ~loc "Quaternion" @@ scad_type_arrow ~loc "Vec3" @@ last_arrow
          | _               -> scad_type_arrow ~loc "Vec3" @@ last_arrow
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
      List.map ~f:gen_sig transforms
  in
  List.concat_map ~f type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg transformer_impl
let intf_generator = Deriving.Generator.V2.make_noarg transformer_intf
let scad = Deriving.add ~str_type_decl:impl_generator ~sig_type_decl:intf_generator "scad"
