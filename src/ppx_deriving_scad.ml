open! Ppxlib
open! Ast_builder.Default

type transform =
  | Translate
  | Rotate
  | RotateAbout
  | MultMatrix
  | Quaternion
  | QuaternionAbout
  | VectorRotateAbout
  | VectorRotate
  | Scale
  | Mirror

let transforms_2d = [ Translate; Rotate; RotateAbout; Scale; Mirror ]

let transforms_3d =
  MultMatrix
  :: Quaternion
  :: QuaternionAbout
  :: VectorRotate
  :: VectorRotateAbout
  :: transforms_2d

let transform_to_string = function
  | Translate -> "translate"
  | Scale -> "scale"
  | Rotate -> "rotate"
  | RotateAbout -> "rotate_about_pt"
  | Quaternion -> "quaternion"
  | QuaternionAbout -> "quaternion_about_pt"
  | VectorRotate -> "vector_rotate"
  | VectorRotateAbout -> "vector_rotate_about_pt"
  | Mirror -> "mirror"
  | MultMatrix -> "multmatrix"

(* underscored translation and scaling params since they may be ignored by unit *)
let transform_to_rev_params = function
  | Translate -> [ "_p" ]
  | Scale -> [ "_s" ]
  | Rotate -> [ "r" ]
  | RotateAbout -> [ "_p"; "r" ]
  | Quaternion -> [ "q" ]
  | QuaternionAbout -> [ "_p"; "q" ]
  | VectorRotate -> [ "a"; "ax" ]
  | VectorRotateAbout -> [ "_p"; "a"; "ax" ]
  | Mirror -> [ "ax" ]
  | MultMatrix -> [ "m" ]

let transform_drop_about = function
  | RotateAbout -> Rotate
  | QuaternionAbout -> Quaternion
  | trans -> trans

let transform_to_names is_unit transform =
  match is_unit, transform with
  | true, (Translate | Scale) -> None
  | false, trans -> Some (transform_to_string trans, transform_to_rev_params trans)
  | true, trans ->
    let trans' = transform_drop_about trans in
    Some (transform_to_string trans', transform_to_rev_params trans')

let transform_expr ~loc ~jane ~transform ~kind (ct : core_type) =
  let inner_expr (attrs : Attr.t) lid =
    ( if not attrs.ignored
    then
      transform_to_names attrs.unit transform
      |> Option.map (fun (name, params) ->
             let params =
               List.fold_left
                 (fun ps p -> (Nolabel, pexp_ident ~loc { loc; txt = lident p }) :: ps)
                 []
                 params
             and txt = Util.fun_id name lid in
             pexp_apply ~loc (pexp_ident ~loc { loc; txt }) params )
    else None )
    |> Option.value ~default:[%expr fun a -> a]
  and fix_id m = Longident.(Ldot (Ldot (lident "Scad_ml", m), "t")) in
  let expr_of_typ attrs ct =
    let map_exprs (expr, maps) =
      List.fold_left (fun expr m -> [%expr [%e m ~loc expr]]) expr maps
    in
    let rec aux attrs funcs next =
      let attrs = Attr.update ~loc attrs (`Type next) in
      match next with
      | [%type: [%t? typ] option] | [%type: [%t? typ] Option.t] ->
        aux attrs (Util.option_map_expr :: funcs) typ
      | [%type: [%t? typ] list] | [%type: [%t? typ] List.t] ->
        aux attrs (Util.list_map_expr :: funcs) typ
      | [%type: ([%t? typ], [%t? _]) result] | [%type: ([%t? typ], [%t? _]) Result.t] ->
        aux attrs (Util.result_map_expr :: funcs) typ
      | [%type: ([%t? _], [%t? _]) Scad.t]
      | [%type: Scad.d2]
      | [%type: Scad.d3]
      | [%type: ([%t? _], [%t? _]) Scad_ml.Scad.t]
      | [%type: Scad_ml.Scad.d2]
      | [%type: Scad_ml.Scad.d3] -> inner_expr attrs (fix_id "Scad"), funcs
      | [%type: Scad.v2] | [%type: Scad_ml.Scad.v2] ->
        inner_expr attrs (fix_id "Vec2"), funcs
      | [%type: Scad.v3] | [%type: Scad_ml.Scad.v3] ->
        inner_expr attrs (fix_id "Vec3"), funcs
      | { ptyp_desc = Ptyp_tuple cts; _ } ->
        let tup_expr =
          let argn n = Printf.sprintf "arg%i" n in
          let args =
            let arg_var i _ = ppat_var ~loc { loc; txt = argn i } in
            ppat_tuple ~loc (List.mapi arg_var cts)
          and sub_exprs =
            let f i c =
              let expr = map_exprs (aux attrs [] c) in
              [%expr [%e expr] [%e evar ~loc (argn i)]]
            in
            List.mapi f cts
          in
          [%expr fun [%p args] -> [%e pexp_tuple ~loc sub_exprs]]
        in
        tup_expr, funcs
      | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, []); _ } ->
        inner_expr attrs lid, funcs
      | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, (arg :: _ as args)); _ } ->
        if List.for_all (Fun.negate Util.is_constr) args
        then inner_expr attrs lid, funcs
        else aux attrs (Util.map_expr ~lid ~jane :: funcs) arg
      | ct -> Location.raise_errorf ~loc "Unhandled type: %s" (string_of_core_type ct)
    in
    map_exprs (aux attrs [] ct)
  in
  let attrs = Attr.(update ~loc { unit = false; ignored = false; jane } kind) in
  if attrs.ignored then [%expr fun a -> a] else expr_of_typ attrs ct

let transformer ~loc ~transform (td : type_declaration) expr =
  let name =
    let func_name = transform_to_string transform in
    ppat_var
      ~loc
      { loc
      ; txt =
          ( if String.equal td.ptype_name.txt "t"
          then func_name
          else Printf.sprintf "%s_%s" func_name td.ptype_name.txt )
      }
  and func =
    let f expr txt = pexp_fun ~loc Nolabel None (ppat_var ~loc { loc; txt }) expr
    and init = pexp_fun ~loc Nolabel None (ppat_var ~loc { loc; txt = "t" }) expr in
    List.fold_left f init (transform_to_rev_params transform)
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
    let expr = transform_expr ~loc ~jane ~transform ~kind:(`Field ld) ld.pld_type in
    field_id, [%expr [%e expr] [%e field_expr]]
  in
  let expr = pexp_record ~loc (List.map entry fields) None in
  transformer ~loc ~transform td expr

let transformer_impl ~jane ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let transforms = function
    | Dim.D2 | Poly _ -> transforms_2d
    | D3 -> transforms_3d
  in
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
        (fun transform -> abstract_transformer ~loc ~jane ~transform td ct)
        (transforms @@ Dim.decide_type ~loc ct)
    | { ptype_kind = Ptype_record fields; _ } ->
      List.map
        (fun transform -> record_transformer ~loc ~jane ~transform td fields)
        (transforms @@ Dim.decide_record ~loc fields)
  in
  List.concat_map f type_declarations

let scad_type_arrow ~loc name =
  let txt = Longident.Ldot (Longident.Ldot (lident "Scad_ml", name), "t") in
  ptyp_arrow ~loc Nolabel (ptyp_constr ~loc { loc; txt } [])

let float_type_arrow ~loc =
  ptyp_arrow ~loc Nolabel (ptyp_constr ~loc { loc; txt = lident "float" } [])

let var_type_arrow ~loc v = ptyp_arrow ~loc Nolabel (ptyp_var ~loc v)

let transformer_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f ({ ptype_kind; ptype_name; ptype_params; ptype_manifest; _ } as td) =
    let dim =
      match ptype_kind, ptype_manifest with
      | (Ptype_variant _ | Ptype_open), _ ->
        Location.raise_errorf
          ~loc
          "Deriving scad transformers for non-abstract/record types is not supported."
      | Ptype_abstract, Some ct -> Dim.decide_type ~loc ct
      | Ptype_abstract, None ->
        Location.raise_errorf
          ~loc
          "Scad transformers cannot be derived for empty abstract types."
      | Ptype_record fields, _ -> Dim.decide_record ~loc fields
    in
    let space_arrow, rot_arrow, transforms =
      match dim with
      | D2 -> scad_type_arrow ~loc "Vec2", float_type_arrow ~loc, transforms_2d
      | D3 ->
        let v3_arrow = scad_type_arrow ~loc "Vec3" in
        v3_arrow, v3_arrow, transforms_3d
      | Poly (space, rot) ->
        var_type_arrow ~loc space, var_type_arrow ~loc rot, transforms_2d
    in
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
            (List.map fst ptype_params)
        in
        ptyp_arrow ~loc Nolabel typ typ
      in
      let pval_type =
        match transform with
        | Rotate -> rot_arrow last_arrow
        | RotateAbout -> rot_arrow @@ space_arrow last_arrow
        | MultMatrix -> scad_type_arrow ~loc "MultMatrix" @@ last_arrow
        | Quaternion -> scad_type_arrow ~loc "Quaternion" @@ last_arrow
        | QuaternionAbout ->
          scad_type_arrow ~loc "Quaternion" @@ space_arrow @@ last_arrow
        | VectorRotate -> space_arrow @@ float_type_arrow ~loc @@ last_arrow
        | VectorRotateAbout ->
          space_arrow @@ float_type_arrow ~loc @@ space_arrow @@ last_arrow
        | _ -> space_arrow last_arrow
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
    List.map gen_sig transforms
  in
  List.concat_map f type_declarations

let impl_generator ~jane = Deriving.Generator.V2.make_noarg (transformer_impl ~jane)
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
