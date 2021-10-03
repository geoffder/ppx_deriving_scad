open Base

module Vec3 = struct
  type t = float * float * float

  let equal (ax, ay, az) (bx, by, bz) =
    let open Float in
    ax = bx && ay = by && az = bz

  let negate (x, y, z) = -.x, -.y, -.z
  let translate (px, py, pz) (tx, ty, tz) = px +. tx, py +. ty, pz +. tz
  let scale (sx, sy, sz) (tx, ty, tz) = sx *. tx, sy *. ty, sz *. tz

  let rotate_x theta (x, y, z) =
    let s = Float.sin theta in
    let c = Float.cos theta in
    let y' = (y *. c) -. (z *. s) in
    let z' = (z *. c) +. (y *. s) in
    x, y', z'

  let rotate_y theta (x, y, z) =
    let s = Float.sin theta in
    let c = Float.cos theta in
    let x' = (x *. c) +. (z *. s) in
    let z' = (z *. c) -. (x *. s) in
    x', y, z'

  let rotate_z theta (x, y, z) =
    let s = Float.sin theta in
    let c = Float.cos theta in
    let x' = (x *. c) -. (y *. s) in
    let y' = (y *. c) +. (x *. s) in
    x', y', z

  let rotate (tx, ty, tz) p = rotate_x tx p |> rotate_y ty |> rotate_z tz

  let rotate_about_pt r pivot p =
    translate p pivot |> rotate r |> translate (negate pivot)
end

module Vec2 = struct
  type t = float * float

  let negate (x, y, z) = -.x, -.y, -.z

  let equal (ax, ay) (bx, by) =
    let open Float in
    ax = bx && ay = by

  let translate (px, py, _) (tx, ty) = px +. tx, py +. ty
  let scale (sx, sy, _) (tx, ty) = sx *. tx, sy *. ty

  let rotate theta (x, y) =
    let s = Float.sin theta in
    let c = Float.cos theta in
    (c *. x) -. (s *. y), (s *. x) +. (c *. y)
end

type both_vecs =
  { v3 : Vec3.t [@unit] [@bar]
  ; v2 : Vec2.t [@foo]
  }
[@@deriving my_deriver]

type vec_pair =
  { a : Vec3.t
  ; b : Vec3.t
  }

let%test "translate_both_vecs" =
  let a = { v3 = 1., 2., 3.; v2 = 5., -1. }
  and p = 1., 0., 2. in
  let trans = translate_both_vecs p a in
  Vec3.equal trans.v3 (Vec3.translate p a.v3)
  && Vec2.equal trans.v2 (Vec2.translate p a.v2)

let%test "rotate_pair" = true
