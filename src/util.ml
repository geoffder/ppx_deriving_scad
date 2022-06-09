open Ppxlib

let is_constr = function
  | { ptyp_desc = Ptyp_constr _; _ } -> true
  | _ -> false
