# [@@deriving scad]
`ppx_deriving_scad` is a PPX syntax extension that generates functions for the 3d
transformation of user defined records containing types for which said
transformation functions are defined, in particular, the `Scad.t` and `Vec3.t`
types of the [Scad_ml library](https://github.com/geoffder/scad-ml).

**For example:**
```ocaml
open Scad_ml

type mark =
  { scad : Scad.t
  ; origin : Vec3.t
  }
  [@@deriving scad]
```
**Generates:**
```ocaml
val translate_mark : Vec3.t -> mark -> mark
val scale_mark : Vec3.t -> mark -> mark
val rotate_mark : Vec3.t -> mark -> mark
val rotate_about_pt_mark : Vec3.t -> Vec3.t -> mark -> mark
val quaternion_mark : Quaternion.t -> mark -> mark
val quaternion_about_pt_mark : Quaternion.t -> Vec3.t -> mark -> mark
val mirror_mark : Vec3.t -> mark -> mark
```

If the name of the type being derived is `t`, then the functions generated (and
those required to be present for the types inside of a record being derived)
will be given unqualified names. For example, applying `[@@deriving scad]` to a
lone record type `t` would give a module that adhered to the following signature.

``` ocaml
open Scad_ml

module Mark : sig
  type t =
    { scad : Scad.t
    ; origin : Vec3.t
    }

  val translate : Vec3.t -> t -> t
  val scale : Vec3.t -> t -> t
  val rotate : Vec3.t -> t -> t
  val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
  val quaternion : Quaternion.t -> t -> t
  val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> t
  val mirror : Vec3.t -> t -> t
end = struct
  type t =
    { scad : Scad.t
    ; origin : Vec3.t
    }
    [@@deriving scad]
end
```

## Attributes
### [@scad.unit]
This annotation should be applied to fields which represent unit vector. Fields
marked with this will not be subject to transformations that would cause them to
lose thier identity as such, or rotate about anything other than the world
origin. Thus:
  - translate and scale will not be applied
  - {rotate,quaternion}_about_pt will be replaced by their pivot translation
  free counterparts
**Usage:**
``` ocaml
type plane =
  { scad : Scad.t
  ; normal : Vec3.t [@scad.unit]
  } [@@deriving scad]
```
In this case the following would hold:
``` ocaml
let true =
  let plane =
    { scad = Scad.cube (10., 10., 0.001)
    ; normal = 0., 0., 1.
    }
  in
  let trans = plane_translate (5., 5., 0.) plane in
  Vec3.equal plane.normal trans.normal
```

### [@scad.ignore]
This annotation marks a field to be ignored by all generated transformations.
This is useful for ignoring whatever flags/configuration data that you want to
carry around along with your type for which the relevant functions have not been
implemented.
**Usage:**
``` ocaml
type mark =
  { scad : Scad.t
  ; origin : Vec3.t
  ; id : int [@scad.ignore]
  } [@@deriving scad]
```
