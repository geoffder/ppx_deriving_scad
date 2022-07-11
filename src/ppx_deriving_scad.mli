(** {1 \[\@\@deriving scad\]}

    A PPX deriver that generates functions for the spatial transformation of user defined
    abstract and record types containing types for which said transformation functions are
    defined, in particular, the {!Scad_ml.Scad.t}, {!Scad_ml.Vec3.t}, and
    {!Scad_ml.Vec2.t} types of the {{:https://github.com/namachan10777/scad-ml}
    Scad_ml library}.

    {b For example:}

    {[
      open Scad_ml

      type mark =
        { scad : Scad.d3
        ; centre : Vec3.t
        }
      [@@deriving scad]
    ]}

    {b Generates:}

    {[
      val translate_mark : Vec3.t -> mark -> mark
      val rotate_mark : Vec3.t -> mark -> mark
      val rotate_about_pt_mark : Vec3.t -> Vec3.t -> mark -> mark
      val quaternion_mark : Quaternion.t -> mark -> mark
      val quaternion_about_pt_mark : Quaternion.t -> Vec3.t -> mark -> mark
      val vector_rotate_mark : Vec3.t -> float -> mark -> mark
      val vector_rotate_about_pt_mark : Vec3.t -> float -> Vec3.t -> mark -> mark
      val multmatrix_mark : MultMatrix.t -> mark -> mark
      val scale_mark : Vec3.t -> mark -> mark
      val mirror_mark : Vec3.t -> mark -> mark
    ]}

    If the name of the type being derived is [t], then the functions generated (and those
    required to be present for the types inside of a type/record being derived) will be
    given unqualified names. For example, applying [\[@@deriving scad\]] to a
    lone record type [t] would give a module that adhered to the following
    signature.

    {[
      open Scad_ml

      module Mark : sig
        type t =
          { scad : Scad.d3
          ; centre : Vec3.t
          }

        val translate : Vec3.t -> t -> t
        val rotate : Vec3.t -> t -> t
        val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
        val quaternion : Quaternion.t -> t -> t
        val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> t
        val vector_rotate : Vec3.t -> float -> t -> t
        val vector_rotate_about_pt : Vec3.t -> float -> Vec3.t -> t -> t
        val multmatrix : MultMatrix.t -> t -> t
        val scale : Vec3.t -> t -> t
        val mirror : Vec3.t -> t -> t
      end = struct
        type t =
          { scad : Scad.d3
          ; centre : Vec3.t
          }
        [@@deriving scad]
      end
    ]}

    {1 Basic monadic types and tuples}

    The [list], [option], and [result] types, as well as {b tuples}, are automatically
    mapped over, without any additional annotation or functions provided.

    {[
      module Tris : sig
        type t = (Vec2.t * Vec2.t * Vec2.t) list

        val translate : Vec2.t -> t -> t
        val rotate : float -> t -> t
        val rotate_about_pt : float -> Vec2.t -> t -> t
        val scale : Vec2.t -> t -> t
        val mirror : Vec2.t -> t -> t
      end = struct
        type t = (Vec2.t * Vec2.t * Vec2.t) list [@@deriving scad]
      end
    ]}

    {1 Other mappable types}

    By default, [\[@@deriving scad\]] will attempt to map over constructors other than the
    above basic types by using applying the [map] function of the relevant module, or for
    the non-[t] named type, using the same naming conventions as explained above.

    {[
      module IntMap = Map.Make (Int)

      type vec_map = Vec3.t IntMap.t [@@deriving scad]
    ]}

    Here, [IntMap.map] will be used to apply transformations to the contained
    {!Scad_ml.Vec3.t} elements. The expected map function should obey the convention of
    the function [f] being the first {i positional} argument. If you are following the
    conventions of JaneStreet and/or have [base]/[core] open, then you may use
    [\[@@deriving scad_jane\]] which defaults to expecting [map] functions to accept a
    keyword parameter [~f] instead. If you are deriving a record containing types with
    mixed mapping conventions, you can make use of the [\[@scad.map\]] and
    [\[@scad.mapf\]] attributes to specify fields that do not match your default
    convention.

    If the constructor type is not named [t] as in this example, then this ppx will
    attempt to use a function with the suffix [_map]. For example, if the type above was
    instead [Vec3.t int_map], the function [int_map_map] will be expected in the scope of
    the derived type.

    {1 Intf generation}

    Annotating types in module sigs and [.mli] files will generate the relevant type
    signatures.

    {[
      module PolyScads : sig
        type ('s, 'r) t =
          { a : ('s, 'r) Scad.t
          ; b : ('s, 'r) Scad.t
          } [@@deriving scad]
      end = struct
        type ('s, 'r) t =
          { a : ('s, 'r) Scad.t
          ; b : ('s, 'r) Scad.t
          } [@@deriving scad]
      end
    ]}

    {1 Attributes}

    {2 \[\@scad.unit\]}

    This annotation should be applied to abstract types and fields which represent unit
    vector. Types/fields marked with this will not be subject to transformations that
    would cause them to lose thier identity as such, or rotate about anything other than
    the world origin. Thus:

    - translate and scale will not be applied (identity function instead)
    - \{[rotate],[quaternion]\}[_about_pt] will be replaced by their pivot translation
      free counterparts

    {b For example:}

    {[
      type plane =
        { scad : Scad.d3
        ; normal : Vec3.t [@scad.unit]
        }
      [@@deriving scad]
    ]}

    {b In this case the following would hold:}

    {[
      let true =
        let plane = { scad = Scad.cube (v3 10. 10. 0.001); normal = (v3 0. 0. 1.) } in
        let trans = plane_translate (v3 5. 5. 0.) plane in
        Vec3.equal plane.normal trans.normal
    ]}

    {2 \[\@scad.ignore\]}

    This annotation marks a field (in a record, not applicable to abstract types) to be
    ignored by all generated transformations. This is useful for ignoring whatever
    flags/configuration data that you want to carry around along with your type for which
    the relevant functions have not been implemented.

    {[
      type mark =
        { scad : Scad.d3
        ; centre : Vec3.t
        ; id : int [@scad.ignore]
        }
      [@@deriving scad]
    ]}

    {2 \[\@scad.map\] and \[\@scad.mapf\]}

    This annotation marks a type/field for which the transformable type is contained
    within a mappable type (aka functor), for which [map] is defined, and whose parameter
    convention differs from the default specified by the deriver attached to the type
    declaration.

    - [\[@@deriving scad\]] -> positional [f] expected (e.g. [map f])
    - [\[@@deriving scad_jane\]] -> keyword [~f] expected (e.g. [map ~f])

    Thus, [\[@scad.map\]]indicates that relevant [map] functions will obey the convention
    of [f] being the first {i positional} argument (overiding [\[@@deriving scad_jane\]]),
    whereas [\[@scad.mapf\]] indicates that a keyword argument of [~f] is expected instead
    (overiding [\[@@deriving scad\]]). These attributes are not required for the [list],
    [option], and [result] types, as they do not rely on any functions in scope.

    {[
      open Base
      module IntMap = Caml.Map.Make (Int)

      module MixedMaps = struct
        type t =
          { std : Vec3.t IntMap.t
          ; jane : (Vec3.t Map.M(Int).t[@scad.mapf])
          }
        [@@deriving scad]
      end
    ]} *)

(** \[\@\@deriving scad\]

    Derives [translate], [scale], [rotate], [rotate_about_pt], [quaternion],
    [quaternion_about_pt], and [mirror] for the tagged abstract or record type. *)
val scad : Ppxlib.Deriving.t

(** [\[@@deriving scad_jane\]]

    Same as [\[@@deriving scad\]], but defaults to expecting keyword [~f] parameters for
    mappable types other than [list], [option], [result], and {b tuples}. This can be
    overridden with the [\[@scad.map\]] attribute. *)
val scad_jane : Ppxlib.Deriving.t
