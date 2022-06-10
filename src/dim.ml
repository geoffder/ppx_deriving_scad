open! Ppxlib
open! Base
open! Ast_builder.Default

type t =
  | D2
  | D3
  | Poly of string * string

type error =
  | MixedDimensions
  | PolyCollapse
  | PolyMismatch
  | UnknownDimension

(* let equal a b = *)
(*   match a, b with *)
(*   | D2, D2 -> true *)
(*   | D3, D3 -> true *)
(*   | Poly (s1, r1), Poly (s2, r2) -> String.equal s1 s2 && String.equal r1 r2 *)
(*   | _ -> false *)

let unwrap_result ~loc res =
  let r = Location.raise_errorf ~loc in
  match res with
  | Ok (Some dim) -> dim
  | Error MixedDimensions -> r "Transformable cannot contain both 2d and 3d entities."
  | Error PolyCollapse ->
    r "Transformable cannot contain polymorphic and concrete dimensional entities."
  | Error PolyMismatch ->
    r "All polymorphic dimensional entities must share the same type variables."
  | Ok None | Error UnknownDimension ->
    r "Dimension could not be determined. Provide @scad.d2 or @scad.d3."

let dim_attr (type a) ~loc (module A : Attr.S with type t = a) (a : a) =
  match Attribute.get A.d2 a, Attribute.get A.d3 a with
  | Some (), None -> Some D2
  | None, Some () -> Some D3
  | None, None -> None
  | Some (), Some () -> Location.raise_errorf ~loc "Cannot tag with multiple dimensions."

let rec check ~loc dim = function
  | [%type: [%t? typ] option]
  | [%type: [%t? typ] Option.t]
  | [%type: [%t? typ] list]
  | [%type: [%t? typ] List.t]
  | [%type: ([%t? typ], [%t? _]) result]
  | [%type: ([%t? typ], [%t? _]) Result.t] -> check ~loc dim typ
  | [%type: v2]
  | [%type: Scad_ml.v2]
  | [%type: Vec2.t]
  | [%type: Scad_ml.Vec2.t]
  | [%type: Path2.t]
  | [%type: Scad_ml.Path2.t]
  | [%type: Poly2.t]
  | [%type: Scad_ml.Poly2.t]
  | [%type: Bezier2.t]
  | [%type: Scad_ml.Bezier2.t]
  | [%type: (Vec2.t, float) Scad.t]
  | [%type: (Vec2.t, float) Scad_ml.Scad.t]
  | [%type: Scad.d2]
  | [%type: Scad_ml.Scad.d2] ->
    ( match dim with
    | Some D3 -> Error MixedDimensions
    | Some (Poly _) -> Error PolyCollapse
    | _ -> Ok (Some D2) )
  | [%type: v3]
  | [%type: Scad_ml.v3]
  | [%type: Vec3.t]
  | [%type: Scad_ml.Vec3.t]
  | [%type: Path3.t]
  | [%type: Scad_ml.Path3.t]
  | [%type: Poly3.t]
  | [%type: Scad_ml.Poly3.t]
  | [%type: Bezier3.t]
  | [%type: Scad_ml.Bezier3.t]
  | [%type: Mesh.t]
  | [%type: Scad_ml.Mesh.t]
  | [%type: (Vec3.t, Vec3.t) Scad.t]
  | [%type: (Vec3.t, Vec3.t) Scad_ml.Scad.t]
  | [%type: Scad.d3]
  | [%type: Scad_ml.Scad.d3] ->
    ( match dim with
    | Some D2 -> Error MixedDimensions
    | Some (Poly _) -> Error PolyCollapse
    | _ -> Ok (Some D3) )
  | [%type:
      ([%t? { ptyp_desc = Ptyp_var s; _ }], [%t? { ptyp_desc = Ptyp_var r; _ }]) Scad.t]
  | [%type:
      ( [%t? { ptyp_desc = Ptyp_var s; _ }]
      , [%t? { ptyp_desc = Ptyp_var r; _ }] )
      Scad_ml.Scad.t] ->
    ( match dim with
    | Some (D2 | D3) -> Error PolyCollapse
    | Some (Poly (s', r')) as d when String.equal s s' && String.equal r r' -> Ok d
    | None -> Ok (Some (Poly (s, r)))
    | _ -> Error PolyMismatch )
  | { ptyp_desc = Ptyp_tuple (hd :: cts); _ } ->
    (* TODO: should check for ignore on elements of tuples (will be adding the
         corresponding flexibility to the impl as well. This may clarify whether
         using first class modules is the right choice, for the rest of the
         attributes, now that more places are using them.) *)
    let f dim' ct =
      if Option.is_some @@ Attr.get_ignore (`Type ct) then Ok dim' else check ~loc dim' ct
    in
    Result.bind ~f:(fun init -> List.fold_result ~init ~f cts) (f dim hd)
  | { ptyp_desc = Ptyp_constr (_, []); _ } -> Ok dim
  | { ptyp_desc = Ptyp_constr (_, (arg :: _ as args)); _ } ->
    if List.for_all ~f:(Fn.non Util.is_constr) args then Ok dim else check ~loc dim arg
  (* TODO: consider allowing type variables if they can be pegged
         to a Scad.t's 'space parameter (v2 or v3). *)
  | ct -> Location.raise_errorf ~loc "Unhandled type: %s" (string_of_core_type ct)

let decide_type ~loc ct =
  let f = function
    | None -> dim_attr ~loc (module Attr.Type) ct
    | d -> d
  in
  unwrap_result ~loc @@ Result.map ~f (check ~loc None ct)

let decide_record ~loc = function
  | [] -> Location.raise_errorf ~loc "Cannot transform empty record."
  | (hd : label_declaration) :: tl ->
    let open Result in
    let checker dim ld =
      if Option.is_some @@ Attr.get_ignore (`Field ld)
      then Ok None
      else
        check ~loc dim ld.pld_type
        >>| function
        | None -> dim_attr ~loc (module Attr.Field) ld
        | d -> d
    in
    checker None hd
    >>= (fun init -> List.fold_result ~init ~f:checker tl)
    |> unwrap_result ~loc
