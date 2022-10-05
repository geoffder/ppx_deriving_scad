# [@@deriving scad]
`ppx_deriving_scad` is a PPX deriver that generates functions for the spatial
transformation of user defined abstract and record types containing types for
which said transformation functions are defined, in particular, the `Scad.t`,
`V3.t`, and `V2.t` types of the [Scad_ml library](https://github.com/geoffder/scad-ml).

**For example:**
```ocaml
open Scad_ml

type mark =
  { scad : Scad.d3
  ; centre : V3.t
  }
  [@@deriving scad]
```
**Generates:**
```ocaml
val translate_mark : V3.t -> mark -> mark
val xtrans_mark : float -> mark -> mark
val ytrans_mark : float -> mark -> mark
val ztrans_mark : float -> mark -> mark
val rotate_mark : ?about:V3.t -> V3.t -> mark -> mark
val xrot_mark : ?about:V3.t -> float -> mark -> mark
val yrot_mark : ?about:V3.t -> float -> mark -> mark
val zrot_mark : ?about:V3.t -> float -> mark -> mark
val axis_rotate_mark : ?about:V3.t -> V3.t -> float -> mark -> mark
val quaternion_mark : ?about:V3.t -> Quaternion.t -> mark -> mark
val scale_mark : V3.t -> mark -> mark
val mirror_mark : V3.t -> mark -> mark
val affine_mark : Affine3.t -> mark -> mark
```

If the name of the type being derived is `t`, then the functions generated (and
those required to be present for the types inside of a type/record being
derived) will be given unqualified names. For example, applying `[@@deriving scad]`
to a lone record type `t` would give a module that adhered to the
following signature.

``` ocaml
open Scad_ml

module Mark : sig
  type t =
    { scad : Scad.d3
    ; centre : V3.t
    }

  val translate : V3.t -> t -> t
  val xtrans : float -> t -> t
  val ytrans : float -> t -> t
  val ztrans : float -> t -> t
  val rotate : ?about:V3.t -> V3.t -> t -> t
  val xrot : ?about:V3.t -> float -> t -> t
  val yrot : ?about:V3.t -> float -> t -> t
  val zrot : ?about:V3.t -> float -> t -> t
  val axis_rotate : ?about:V3.t -> V3.t -> float -> t -> t
  val quaternion : ?about:V3.t -> Quaternion.t -> t -> t
  val scale : V3.t -> t -> t
  val mirror : V3.t -> t -> t
  val affine : Affine3.t -> t -> t
end = struct
  type t =
    { scad : Scad.three_d Scad.t
    ; centre : V3.t
    }
    [@@deriving scad]
end
```

## Basic mappable types and tuples
The `list`, `option`, and `result` types, as well as **tuples**, are automatically
mapped over, without any additional annotation or functions provided.
``` ocaml
module Tris : sig
  type t = (V2.t * V2.t * V2.t) list

  val translate : V2.t -> t -> t
  val xtrans : float -> t -> t
  val ytrans : float -> t -> t
  val rotate : ?about:V2.t -> float -> t -> t
  val zrot : ?about:V2.t -> float -> t -> t
  val scale : V2.t -> t -> t
  val mirror : V2.t -> t -> t
  val affine : Affine2.t -> t -> t
end = struct
  type t = (V2.t * V2.t * V2.t) list [@@deriving scad]
end
```

## Other mappable types
By default, `[@@deriving scad]` will attempt to map over constructors other than
the above basic types by using applying the `map` function of the relevant
module, or for the non-`t` named type, using the same naming conventions as
explained above.
``` ocaml
module IntMap = Map.Make (Int)
type vec_map = V3.t IntMap.t [@@deriving scad]
```
Here, `IntMap.map` will be used to apply transformations to the contained
`V3.t` elements. The expected map function should obey the convention of the
function `f` being the first *positional* argument. If you are following the
conventions of Jane Street and/or have `base`/`core` open, then you may use
`[@@deriving scad_jane]` which defaults to expecting `map` functions to accept a
keyword parameter `~f` instead. If you are deriving a record containing types
with mixed mapping conventions, you can make use of the [`[@scad.map]` and
`[@scad.mapf]`](#scadmap-and-scadmapf) attributes to specify fields that
do not match your default convention.

If the constructor type is not named `t` as in this example, then this ppx will
attempt to use a function with the suffix `_map`. For example, if the type above
was instead `V3.t int_map`, the function `int_map_map` will be expected in the
scope of the derived type.

## Intf generation
Annotating types in module sigs and `.mli` files will generate the relevant type signatures.
``` ocaml
module PolyScads : sig
  type ('s, 'r, 'a) t =
    { a : ('s, 'r, 'a) Scad.t
    ; b : ('s, 'r, 'a) Scad.t
    } [@@deriving scad]
end = struct
  type ('s, 'r, 'a) t =
    { a : ('s, 'r, 'a) Scad.t
    ; b : ('s, 'r, 'a) Scad.t
    } [@@deriving scad]
end
```

Note that this is also an example of polymorphism over the dimensionality of the
`Scad_ml.Scad.t` type. Of course, when the type could be either **2d** or **3d**, only
**2d** transformations will be available (translation, rotation, scaling, and
mirroring), as in the mappable type example.

## Attributes
### [@scad.unit]
This annotation should be applied to abstract types and fields which represent
unit vector. Types/fields marked with this will not be subject to
transformations that would cause them to lose thier identity as such, or rotate
about anything other than the world origin. Thus:

  - `translate` and `scale` will not be applied (identity function instead)
  - the `?about` parameter will not be passed to the rotation functions
    (`rotate`, `axis_rotate`, and `quaternion`)

**Usage:**
``` ocaml
type plane =
  { scad : Scad.d3
  ; normal : V3.t [@scad.unit]
  } [@@deriving scad]
```
In this case the following would hold:
``` ocaml
let true =
  let plane =
    { scad = Scad.cube (v3 10. 10. 0.001)
    ; normal = (v3 0. 0. 1.)
    }
  in
  let trans = plane_translate (v3 5. 5. 0.) plane in
  V3.equal plane.normal trans.normal
```

### [@scad.ignore]
This annotation marks a field (in a record, not applicable to abstract types) to
be ignored by all generated transformations. This is useful for ignoring
whatever flags/configuration data that you want to carry around along with your
type for which the relevant functions have not been implemented.

**Usage:**
``` ocaml
type mark =
  { scad : Scad.d3
  ; centre : V3.t
  ; id : int [@scad.ignore]
  } [@@deriving scad]
```

### [@scad.map] and [@scad.mapf]
This annotation marks a type/field for which the transformable type is contained
within a mappable type (aka functor), for which `map` is defined, and whose
parameter convention differs from the default specified by the deriver attached
to the type declaration.

  - `[@@deriving scad]` -> positional `f` expected (e.g. `map f`)
  - `[@@deriving scad_jane]` -> keyword `~f` expected (e.g. `map ~f`)

Thus, `[@scad.map]` indicates that relevant `map` functions will obey the
convention of `f` being the first *positional* argument (overiding `[@@deriving
scad_jane]`), whereas `[@scad.mapf]` indicates that a keyword argument of `~f`
is expected instead (overiding `[@@deriving scad]`). These attributes are not
required for the `list`, `option`, and `result` types, as they do not rely on
any functions in scope.

**Usage:**
``` ocaml
open Base
module IntMap = Caml.Map.Make (Int)
module JaneOption = Option (* aliased since option is special cased *)

module MixedMapConventions : sig
  type t =
    { std : v3 IntMap.t
    ; jane : v3 JaneOption.t
    }
  [@@deriving scad]
end = struct
  type t =
    { std : v3 IntMap.t
    ; jane : v3 JaneOption.t [@scad.mapf]
    }
  [@@deriving scad]
end
```

### [@scad.d2] and [@scad.d3]

When the dimensionality of a type is ambiguous (e.g. containing no fields with
concretely dimensional types from `Scad_ml` such as `Scad.d3`, or `V2.t`), these
annotations should be used to specify the correct set of functions/signatures to be
generated.

```ocaml
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
```

Here, there are no `Scad_ml` types present in `a' t` that can clue
`[@@deriving scad]` into whether it is **2d** or **3d**, so we tag on an attribute
to clear it up.
