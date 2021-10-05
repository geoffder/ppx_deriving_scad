open Base
open Scad_ml

module Vec2 = struct
  type t = float * float

  let negate (x, y, z) = -.x, -.y, -.z

  let equal (ax, ay) (bx, by) =
    let open Float in
    ax = bx && ay = by

  let translate (px, py, _) (tx, ty) = px +. tx, py +. ty
  let scale (sx, sy, _) (tx, ty) = sx *. tx, sy *. ty

  let rotate (_, _, r) (x, y) =
    let s = Float.sin r in
    let c = Float.cos r in
    (c *. x) -. (s *. y), (s *. x) +. (c *. y)

  let rotate_about_pt r p t = translate p t |> rotate r |> translate (negate p)
end

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
