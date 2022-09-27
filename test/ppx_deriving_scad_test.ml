open Base
open Scad_ml

type vec_pair =
  { reg : V3.t
  ; unit : V3.t [@scad.unit]
  }
[@@deriving scad]

type with_ignored =
  { vector : V3.t
  ; ignored : int [@scad.ignore]
  }
[@@deriving scad]

module ScadVec : sig
  type t =
    { scad : Scad.d3
    ; vec_pair : vec_pair
    }
  [@@deriving scad]
end = struct
  type t =
    { scad : Scad.d3
    ; vec_pair : vec_pair
    }
  [@@deriving scad]
end

module type FunType = sig
  type t = float -> V3.t [@@deriving scad]
end

module PolyScads : sig
  type ('s, 'r, 'a) t =
    { a : ('s, 'r, 'a) Scad.t
    ; b : ('s, 'r, 'a) Scad.t
    }
  [@@deriving scad]
end = struct
  type ('s, 'r, 'a) t =
    { a : ('s, 'r, 'a) Scad.t
    ; b : ('s, 'r, 'a) Scad.t
    }
  [@@deriving scad]
end

module Pts : sig
  type t = { pts : V3.t list } [@@deriving scad]
end = struct
  type t = { pts : V3.t list } [@@deriving scad]
end

module PtsList : sig
  type t = (Pts.t list[@scad.d3]) [@@deriving scad]
end = struct
  type t = (Pts.t list[@scad.d3]) [@@deriving scad]
end

module OptOpt : sig
  type t = { vec : V3.t option option } [@@deriving scad]
end = struct
  type t = { vec : V3.t option option } [@@deriving scad]
end

module IntMap = Caml.Map.Make (Int)

module VecStdMap : sig
  type t = { map : V3.t IntMap.t } [@@deriving scad]
end = struct
  type t = { map : V3.t IntMap.t } [@@deriving scad]
end

module BareJaneMap : sig
  type t = V3.t Map.M(Int).t [@@deriving scad_jane]
end = struct
  type t = V3.t Map.M(Int).t [@@deriving scad_jane]
end

(* aliased to avoid generating option map expression (test map function finding) *)
module JaneOption = Option

module MixedMapConventions : sig
  type t =
    { std : V3.t IntMap.t
    ; jane : V3.t JaneOption.t
    }
  [@@deriving scad]
end = struct
  type t =
    { std : V3.t IntMap.t
    ; jane : V3.t JaneOption.t [@scad.mapf]
    }
  [@@deriving scad]
end

module BareVecList : sig
  type t = V2.t list [@@deriving scad]
end = struct
  type t = V2.t list [@@deriving scad]
end

module VecRes : sig
  type t = { res : (V3.t, string) Result.t } [@@deriving scad]
end = struct
  type t = { res : (V3.t, string) Result.t } [@@deriving scad]
end

module AmbiguousDims : sig
  type 'a p =
    { a : 'a [@scad.ignore]
    ; v : v2
    }
  [@@deriving scad]

  type 'a t = { p : 'a p [@scad.d2] } [@@deriving scad]
end = struct
  type 'a p =
    { a : 'a [@scad.ignore]
    ; v : v2
    }
  [@@deriving scad]

  type 'a t = { p : 'a p [@scad.d2] } [@@deriving scad]
end

module VecTupleOpt : sig
  type t = (V3.t JaneOption.t * V3.t option option) option [@@deriving scad]
end = struct
  type t = ((V3.t JaneOption.t[@scad.mapf]) * V3.t option option) option [@@deriving scad]
end

module Tris : sig
  type t = (V2.t * V2.t * V2.t) list [@@deriving scad]
end = struct
  type t = (V2.t * V2.t * V2.t) list [@@deriving scad]
end

let%test "rotate_about_pair" =
  let a = { reg = v3 5. 5. 0.; unit = v3 0. 1. 0. }
  and r = v3 0. 0. (Float.pi /. 2.)
  and p = v3 0. 5. 0. in
  let rot = zrot_vec_pair ~about:p r.z a in
  V3.equal rot.reg (V3.rotate ~about:p r a.reg) && V3.equal rot.unit (V3.rotate r a.unit)

let%test "unit_prevents_translate" =
  let a = { reg = v3 5. 5. 0.; unit = v3 0. 1. 0. }
  and p = v3 0. 5. 0. in
  let trans = translate_vec_pair p a in
  V3.equal trans.reg (V3.translate p a.reg) && V3.equal trans.unit a.unit

let%test "ignored" =
  let a = { vector = v3 1. 2. 3.; ignored = 0 }
  and p = v3 1. 1. 1. in
  let trans = translate_with_ignored p a in
  V3.equal trans.vector (V3.translate p a.vector) && a.ignored = trans.ignored

let%test "translate_points" =
  let a = Pts.{ pts = [ V3.zero; V3.zero; V3.zero ] }
  and p = v3 1. 1. 1. in
  let trans = Pts.translate p a in
  List.equal V3.equal (List.map ~f:(V3.add p) a.pts) trans.pts

let%test "translate_opt_opt" =
  let a = OptOpt.{ vec = Some (Some V3.zero) }
  and p = v3 1. 1. 1. in
  let trans = OptOpt.translate p a in
  V3.equal p Option.(value ~default:V3.zero @@ join trans.vec)

let%test "translate_map_vec" =
  let a = VecStdMap.{ map = IntMap.add 0 (v3 0. 0. 0.) IntMap.empty }
  and p = v3 1. 1. 1. in
  let trans = VecStdMap.translate p a in
  V3.equal p (IntMap.find 0 trans.map)

let%test "translate_bare_jane_map" =
  let p = v3 1. 1. 1. in
  let a =
    BareJaneMap.translate
      p
      (Map.add_exn (Map.empty (module Int)) ~key:0 ~data:(v3 0. 0. 0.))
  in
  V3.equal p (Map.find_exn a 0)

let%test "translate_opt_tuple" =
  let p = v3 1. 1. 1. in
  match VecTupleOpt.translate p @@ Some (Some V3.zero, None) with
  | Some (Some p', None) -> V3.approx p p'
  | _ -> false
