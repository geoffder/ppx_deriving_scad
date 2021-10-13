open Ppxlib

module Field = struct
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
end

module Type = struct
  let unit =
    Attribute.declare "scad.unit" Attribute.Context.core_type Ast_pattern.(pstr nil) ()

  let map =
    Attribute.declare "scad.map" Attribute.Context.core_type Ast_pattern.(pstr nil) ()

  let mapf =
    Attribute.declare "scad.mapf" Attribute.Context.core_type Ast_pattern.(pstr nil) ()
end

let get_unit = function
  | `Type td  -> Attribute.get Type.unit td
  | `Field ld -> Attribute.get Field.unit ld

let get_ignore = function
  | `Type _   -> None
  | `Field ld -> Attribute.get Field.ignore ld

let get_map = function
  | `Type td  -> Attribute.get Type.map td
  | `Field ld -> Attribute.get Field.map ld

let get_mapf = function
  | `Type td  -> Attribute.get Type.mapf td
  | `Field ld -> Attribute.get Field.mapf ld
