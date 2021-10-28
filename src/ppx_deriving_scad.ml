open! Ppxlib
open! Base
open! Ast_builder.Default

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
  | Translate -> "translate"
  | Scale -> "scale"
  | Rotate -> "rotate"
  | RotateAbout -> "rotate_about_pt"
  | Quaternion -> "quaternion"
  | QuaternionAbout -> "quaternion_about_pt"
  | Mirror -> "mirror"

let transform_to_rev_params = function
  | Translate -> [ "p" ]
  | Scale -> [ "s" ]
  | Rotate -> [ "r" ]
  | RotateAbout -> [ "p"; "r" ]
  | Quaternion -> [ "q" ]
  | QuaternionAbout -> [ "p"; "q" ]
  | Mirror -> [ "ax" ]

let transform_drop_about = function
  | RotateAbout -> Rotate
  | QuaternionAbout -> Quaternion
  | trans -> trans

let transform_to_names is_unit transform =
  match (is_unit, transform) with
  | true, (Translate | Scale) -> None
  | false, trans ->
      Some (transform_to_string trans, transform_to_rev_params trans)
  | true, trans ->
      let trans' = transform_drop_about trans in
      Some (transform_to_string trans', transform_to_rev_params trans')

let option_map ~loc expr =
  [%expr function Some opt -> Some ([%e expr] opt) | None -> None]

let result_map ~loc expr =
  [%expr function Ok ok -> Ok ([%e expr] ok) | err -> err]

let list_map ~loc expr =
  [%expr
    let rec aux acc = function
      | h :: t -> aux ([%e expr] h :: acc) t
      | [] -> acc
    in
    aux []]

let fun_id name lid =
  let maybe_suffix s =
    if String.equal s "t" then name else Printf.sprintf "%s_%s" name s
  in
  match lid with
  | Lident s -> Lident (maybe_suffix s)
  | Ldot (p, s) -> Ldot (p, maybe_suffix s)
  | Lapply _ -> assert false

let rec is_jane_map = function
  | Lident s -> String.equal "Map" s
  | Ldot (p, _) -> is_jane_map p
  | Lapply (a, b) -> is_jane_map a || is_jane_map b

let map ~lid ~jane ~loc expr =
  let lid =
    if jane && is_jane_map lid then Longident.Ldot (lident "Map", "t") else lid
  in
  let id = pexp_ident ~loc @@ { loc; txt = fun_id "map" lid } in
  if jane then [%expr [%e id] ~f:[%e expr]] else [%expr [%e id] [%e expr]]

let transform_expr ~loc ~jane ~transform ~kind (ct : core_type) =
  let is_unit = Option.is_some @@ Attr.get_unit kind
  and ignored = Option.is_some @@ Attr.get_ignore kind
  and jane =
    (jane || (Option.is_some @@ Attr.get_mapf kind))
    && (not @@ Option.is_some @@ Attr.get_map kind)
  and is_constr = function
    | { ptyp_desc = Ptyp_constr _; _ } -> true
    | _ -> false
  in
  let f (name, params) =
    let inner_expr name lid =
      let params =
        List.fold
          ~init:[]
          ~f:(fun ps p ->
            (Nolabel, pexp_ident ~loc { loc; txt = lident p }) :: ps )
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
      | [%type: ([%t? typ], [%t? _]) result]
      | [%type: ([%t? typ], [%t? _]) Result.t] ->
          exprs_of_typ (result_map :: funcs) typ
      | [%type: [%t? _] Scad.t]
      | [%type: Scad.d2]
      | [%type: Scad.d3]
      | [%type: [%t? _] Scad_ml.Scad.t]
      | [%type: Scad_ml.Scad.d2]
      | [%type: Scad_ml.Scad.d3] ->
          let lid = Longident.(Ldot (Ldot (lident "Scad_ml", "Scad"), "t")) in
          (inner_expr name lid, funcs)
      | { ptyp_desc = Ptyp_tuple cts; _ } ->
          let tup_expr =
            let argn n = Printf.sprintf "arg%i" n in
            let args =
              let arg_var i _ = ppat_var ~loc { loc; txt = argn i } in
              ppat_tuple ~loc (List.mapi ~f:arg_var cts)
            and sub_exprs =
              let f i c =
                let apply expr m =
                  [%expr [%e m ~loc expr] [%e evar ~loc (argn i)]]
                and expr, maps = exprs_of_typ [] c in
                List.fold ~f:apply ~init:expr maps
              in
              List.mapi ~f cts
            in
            [%expr fun [%p args] -> [%e pexp_tuple ~loc sub_exprs]]
          in
          (tup_expr, funcs)
      | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, []); _ } ->
          (inner_expr name lid, funcs)
      | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, (arg :: _ as args)); _ } ->
          if List.for_all ~f:(Fn.non is_constr) args then
            (inner_expr name lid, funcs)
          else exprs_of_typ (map ~lid ~jane :: funcs) arg
      | ct ->
          Location.raise_errorf
            ~loc
            "Unhandled type: %s"
            (string_of_core_type ct)
    in
    let expr, maps = exprs_of_typ [] ct in
    List.fold ~f:(fun expr m -> [%expr [%e m ~loc expr]]) ~init:expr maps
  in
  Option.(
    value_map
      ~f
      ~default:[%expr fun a -> a]
      (transform_to_names is_unit transform >>= some_if (not ignored)))

let transformer ~loc ~transform (td : type_declaration) expr =
  let name =
    let func_name = transform_to_string transform in
    ppat_var
      ~loc
      { loc
      ; txt =
          ( if String.equal td.ptype_name.txt "t" then func_name
          else Printf.sprintf "%s_%s" func_name td.ptype_name.txt )
      }
  and func =
    let f expr txt =
      pexp_fun ~loc Nolabel None (ppat_var ~loc { loc; txt }) expr
    and init =
      pexp_fun ~loc Nolabel None (ppat_var ~loc { loc; txt = "t" }) expr
    in
    List.fold ~init ~f (transform_to_rev_params transform)
  in
  [%stri let [%p name] = [%e func]]

let abstract_transformer ~loc ~jane ~transform (td : type_declaration) ct =
  let expr = transform_expr ~loc ~jane ~transform ~kind:(`Type ct) ct in
  transformer ~loc ~transform td [%expr [%e expr] t]

let record_transformer ~loc ~jane ~transform (td : type_declaration) fields =
  let entry (ld : label_declaration) =
    let loc = ld.pld_loc in
    let field_id = { loc; txt = lident ld.pld_name.txt } in
    let field_expr =
      pexp_field ~loc (pexp_ident ~loc { loc; txt = lident "t" }) field_id
    in
    let expr =
      transform_expr ~loc ~jane ~transform ~kind:(`Field ld) ld.pld_type
    in
    (field_id, [%expr [%e expr] [%e field_expr]])
  in
  let expr = pexp_record ~loc (List.map ~f:entry fields) None in
  transformer ~loc ~transform td expr

let transformer_impl ~jane ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_variant _ | Ptype_open; _ } ->
        Location.raise_errorf
          ~loc
          "Deriving scad transformers for variant/open types is not supported."
    | { ptype_kind = Ptype_abstract; ptype_manifest = None; _ } ->
        Location.raise_errorf
          ~loc
          "Scad transformers cannot be derived for empty abstract types."
    | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
        List.map
          ~f:(fun transform -> abstract_transformer ~loc ~jane ~transform td ct)
          transforms
    | { ptype_kind = Ptype_record fields; _ } ->
        List.map
          ~f:(fun transform ->
            record_transformer ~loc ~jane ~transform td fields )
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
    | { ptype_kind = Ptype_variant _ | Ptype_open; _ } ->
        Location.raise_errorf
          ~loc
          "Deriving scad transformers for non-abstract/record types is not \
           supported."
    | { ptype_kind = Ptype_abstract | Ptype_record _
      ; ptype_name
      ; ptype_params
      ; _
      } ->
        let gen_sig transform =
          let name =
            let func_name = transform_to_string transform in
            if String.equal td.ptype_name.txt "t" then func_name
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
            | RotateAbout ->
                let arrow = scad_type_arrow ~loc "Vec3" in
                arrow @@ arrow last_arrow
            | Quaternion -> scad_type_arrow ~loc "Quaternion" @@ last_arrow
            | QuaternionAbout ->
                scad_type_arrow ~loc "Quaternion"
                @@ scad_type_arrow ~loc "Vec3"
                @@ last_arrow
            | _ -> scad_type_arrow ~loc "Vec3" @@ last_arrow
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

let impl_generator ~jane =
  Deriving.Generator.V2.make_noarg (transformer_impl ~jane)

let intf_generator = Deriving.Generator.V2.make_noarg transformer_intf

let scad =
  Deriving.add
    ~str_type_decl:(impl_generator ~jane:false)
    ~sig_type_decl:intf_generator
    "scad"

let scad_jane =
  Deriving.add
    ~str_type_decl:(impl_generator ~jane:true)
    ~sig_type_decl:intf_generator
    "scad_jane"
