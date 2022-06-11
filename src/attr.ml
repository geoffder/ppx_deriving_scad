open Ppxlib

module type S = sig
  type t

  val unit : (t, unit) Attribute.t
  val ignore : (t, unit) Attribute.t
  val map : (t, unit) Attribute.t
  val mapf : (t, unit) Attribute.t
  val d2 : (t, unit) Attribute.t
  val d3 : (t, unit) Attribute.t
end

module M (T : sig
  type t

  val t : t Attribute.Context.t
end) =
struct
  type t = T.t

  let unit = Attribute.declare "scad.unit" T.t Ast_pattern.(pstr nil) ()
  let ignore = Attribute.declare "scad.ignore" T.t Ast_pattern.(pstr nil) ()
  let map = Attribute.declare "scad.map" T.t Ast_pattern.(pstr nil) ()
  let mapf = Attribute.declare "scad.mapf" T.t Ast_pattern.(pstr nil) ()
  let d2 = Attribute.declare "scad.d2" T.t Ast_pattern.(pstr nil) ()
  let d3 = Attribute.declare "scad.d3" T.t Ast_pattern.(pstr nil) ()
end

module Field : S with type t = label_declaration = M (struct
  type t = label_declaration

  let t = Attribute.Context.label_declaration
end)

module Type : S with type t = core_type = M (struct
  type t = core_type

  let t = Attribute.Context.core_type
end)

type t =
  { unit : bool
  ; ignored : bool
  ; jane : bool
  }

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

let update t kind =
  { unit = t.unit || (Option.is_some @@ get_unit kind)
  ; ignored = t.ignored || (Option.is_some @@ get_ignore kind)
  ; jane =
      (t.jane || (Option.is_some @@ get_mapf kind))
      && (not @@ Option.is_some @@ get_map kind)
  }

(* let is_unit (type a) (module A : S with type t = a) (a : a) = *)
(*   Option.is_some @@ Attribute.get A.unit a *)

(* let is_ignore (type a) (module A : S with type t = a) (a : a) = *)
(*   Option.is_some @@ Attribute.get A.ignore a *)

(* let is_map (type a) (module A : S with type t = a) (a : a) = *)
(*   Option.is_some @@ Attribute.get A.map a *)

(* let is_mapf (type a) (module A : S with type t = a) (a : a) = *)
(*   Option.is_some @@ Attribute.get A.mapf a *)

(* let update (type a) (module A : S with type t = a) t (a : a) = *)
(*   { unit = t.unit || is_unit (module A) a *)
(*   ; ignored = t.ignored || is_ignore (module A) a *)
(*   ; jane = (t.jane || is_mapf (module A) a) && (not @@ is_map (module A) a) *)
(*   } *)
