open Ppxlib

module type S = sig
  type t

  val unit : (t, unit) Attribute.t

  (* val ignore : (t, unit) Attribute.t *)
  val map : (t, unit) Attribute.t
  val mapf : (t, unit) Attribute.t
  val d2 : (t, unit) Attribute.t
  val d3 : (t, unit) Attribute.t
end

module Field = struct
  type t = label_declaration

  let unit =
    Attribute.declare
      "scad.unit"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()

  let ignore =
    Attribute.declare
      "scad.ignore"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()

  let map =
    Attribute.declare
      "scad.map"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()

  let mapf =
    Attribute.declare
      "scad.mapf"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()

  let d2 =
    Attribute.declare
      "scad.d2"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()

  let d3 =
    Attribute.declare
      "scad.d3"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()
end

module Type = struct
  type t = core_type

  let unit =
    Attribute.declare "scad.unit" Attribute.Context.core_type Ast_pattern.(pstr nil) ()

  let ignore =
    Attribute.declare "scad.ignore" Attribute.Context.core_type Ast_pattern.(pstr nil) ()

  let map =
    Attribute.declare "scad.map" Attribute.Context.core_type Ast_pattern.(pstr nil) ()

  let mapf =
    Attribute.declare "scad.mapf" Attribute.Context.core_type Ast_pattern.(pstr nil) ()

  let d2 =
    Attribute.declare "scad.d2" Attribute.Context.core_type Ast_pattern.(pstr nil) ()

  let d3 =
    Attribute.declare "scad.d3" Attribute.Context.core_type Ast_pattern.(pstr nil) ()
end

let get_unit = function
  | `Type ct -> Attribute.get Type.unit ct
  | `Field ld -> Attribute.get Field.unit ld

let get_ignore = function
  | `Type _ -> None
  | `Field ld -> Attribute.get Field.ignore ld

let get_map = function
  | `Type ct -> Attribute.get Type.map ct
  | `Field ld -> Attribute.get Field.map ld

let get_mapf = function
  | `Type ct -> Attribute.get Type.mapf ct
  | `Field ld -> Attribute.get Field.mapf ld

(* let get_unit (type a) (module A : S with type t = a) (a : a) = Attribute.get A.unit a *)
(* let get_ignore (type a) (module A : S with type t = a) (a : a) = Attribute.get A.ignore a *)
(* let get_map (type a) (module A : S with type t = a) (a : a) = Attribute.get A.map a *)
(* let get_mapf (type a) (module A : S with type t = a) (a : a) = Attribute.get A.mapf a *)
