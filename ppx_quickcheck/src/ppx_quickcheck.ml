open! Base
open Ppxlib
open Ppx_quickcheck_expander

let (_ : Deriving.t) = Deriving.add "quickcheck" ~sig_type_decl ~str_type_decl

let extend name f =
  Deriving.add name ~extension:(f ~portable:false) |> Deriving.ignore;
  Deriving.add (name ^ "_portable") ~extension:(f ~portable:true) |> Deriving.ignore
;;

let () = extend "quickcheck.generator" generator_extension
let () = extend "quickcheck.observer" observer_extension
let () = extend "quickcheck.shrinker" shrinker_extension
