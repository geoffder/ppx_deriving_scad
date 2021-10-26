(lang dune 2.8)

(name ppx_deriving_scad)

(generate_opam_files true)

(source
 (github geoffder/ppx_deriving_scad))

(documentation https://geoffder.github.io/ppx_deriving_scad/)

(license AGPL-3.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com>")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com>")

(package
 (name ppx_deriving_scad)
 (synopsis "PPX Deriver for Scad_ml transformation functions")
 (description
  "[@@deriving scad] generates functions for the\nspatial transformation of user defined abstract and record types containing\ntypes for which said transformation functions are defined, in particular, the\n`Scad.t` and `Vec3.t` types of the Scad_ml library.")
 (depends
  (ocaml
   (>= 4.08.0))
  (base
   (>= 0.14.1))
  (scad_ml
   (and
    (>= 1.0.0)
    :with-test))
  (ppxlib
   (>= 0.22.2))
  (ppx_inline_test :with-test)))
