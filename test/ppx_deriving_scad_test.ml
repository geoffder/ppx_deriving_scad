open Base
open Scad_ml

type vec_pair =
  { reg : Vec3.t
  ; unit : Vec3.t [@scad.unit]
  }
[@@deriving scad]

type with_ignored =
  { vector : Vec3.t
  ; ignored : int [@scad.ignore]
  }
[@@deriving scad]

module ScadVec : sig
  type t =
    { scad : Scad.t
    ; vec_pair : vec_pair
    }
  [@@deriving scad]
end = struct
  type t =
    { scad : Scad.t
    ; vec_pair : vec_pair
    }
  [@@deriving scad]
end

module Pts = struct
  type t = { pts : Vec3.t list } [@@deriving scad]
end

module OptOpt = struct
  type t = { vec : Vec3.t option option } [@@deriving scad]
end

let%test "rotate_about_pair" =
  let a = { reg = 5., 5., 0.; unit = 0., 1., 0. }
  and r = 0., 0., Float.pi /. 2.
  and p = 0., 5., 0. in
  let rot = rotate_about_pt_vec_pair r p a in
  Vec3.equal rot.reg (Vec3.rotate_about_pt r p a.reg)
  && Vec3.equal rot.unit (Vec3.rotate r a.unit)

let%test "unit_prevents_translate" =
  let a = { reg = 5., 5., 0.; unit = 0., 1., 0. }
  and p = 0., 5., 0. in
  let trans = translate_vec_pair p a in
  Vec3.equal trans.reg (Vec3.translate p a.reg) && Vec3.equal trans.unit a.unit

let%test "ignored" =
  let a = { vector = 1., 2., 3.; ignored = 0 }
  and p = 1., 1., 1. in
  let trans = translate_with_ignored p a in
  Vec3.equal trans.vector (Vec3.translate p a.vector) && a.ignored = trans.ignored

let%test "translate_points" =
  let a = Pts.{ pts = [ Vec3.zero; Vec3.zero; Vec3.zero ] }
  and p = 1., 1., 1. in
  let trans = Pts.translate p a in
  List.equal Vec3.equal (List.map ~f:(Vec3.add p) a.pts) trans.pts

let%test "translate_opt_opt" =
  let a = OptOpt.{ vec = Some (Some Vec3.zero) }
  and p = 1., 1., 1. in
  let trans = OptOpt.translate p a in
  Vec3.equal p Option.(value ~default:Vec3.zero @@ join trans.vec)
