include Base
include Ppxlib
include Ast_builder.Default

(* errors and error messages *)

let ( ^^ ) = Stdlib.( ^^ )
let error ~loc fmt = Location.raise_errorf ~loc ("ppx_quickcheck: " ^^ fmt)
let invalid ~loc fmt = error ~loc ("invalid syntax: " ^^ fmt)
let unsupported ~loc fmt = error ~loc ("unsupported: " ^^ fmt)
let internal_error ~loc fmt = error ~loc ("internal error: " ^^ fmt)

(* little syntax helpers *)

let loc_map { loc; txt } ~f = { loc; txt = f txt }
let lident_loc = loc_map ~f:lident

let prefixed_type_name ~name_is_portable prefix type_name =
  let suffix = if name_is_portable then "__portable" else "" in
  match type_name with
  | "t" -> prefix ^ suffix
  | _ -> prefix ^ "_" ^ type_name ^ suffix
;;

let generator_name ~name_is_portable type_name =
  prefixed_type_name ~name_is_portable "quickcheck_generator" type_name
;;

let observer_name ~name_is_portable type_name =
  prefixed_type_name ~name_is_portable "quickcheck_observer" type_name
;;

let shrinker_name ~name_is_portable type_name =
  prefixed_type_name ~name_is_portable "quickcheck_shrinker" type_name
;;

let pname { loc; txt } ~f = pvar ~loc (f txt)
let ename { loc; txt } ~f = evar ~loc (f txt)
let pgenerator ~name_is_portable = pname ~f:(generator_name ~name_is_portable)
let pobserver ~name_is_portable = pname ~f:(observer_name ~name_is_portable)
let pshrinker ~name_is_portable = pname ~f:(shrinker_name ~name_is_portable)
let egenerator ~name_is_portable = ename ~f:(generator_name ~name_is_portable)
let eobserver ~name_is_portable = ename ~f:(observer_name ~name_is_portable)
let eshrinker ~name_is_portable = ename ~f:(shrinker_name ~name_is_portable)

let portability_mode ~loc ~portable_value =
  if portable_value then [%expr portable] else [%expr nonportable]
;;

let name_is_portable ~portable_value args = portable_value && not (List.is_empty args)

let ptuple ~loc list =
  match list with
  | [] -> [%pat? ()]
  | [ pat ] -> pat
  | _ -> ppat_tuple ~loc list
;;

(* creating (probably-)unique symbols for generated code *)

let gensym prefix loc =
  let loc = { loc with loc_ghost = true } in
  let sym = gen_symbol ~prefix:("_" ^ prefix) () in
  pvar ~loc sym, evar ~loc sym
;;

let gensyms prefix loc_list = List.map loc_list ~f:(gensym prefix) |> List.unzip

let gensymss prefix loc_list_list =
  List.map loc_list_list ~f:(gensyms prefix) |> List.unzip
;;

(* expression to create a higher order function that maps from function with one kind of
   argument label to another *)

let fn_map_label ~loc ~from ~to_ =
  let f_pat, f_expr = gensym "f" loc in
  let x_pat, x_expr = gensym "x" loc in
  pexp_fun
    ~loc
    Nolabel
    None
    f_pat
    (pexp_fun ~loc to_ None x_pat (pexp_apply ~loc f_expr [ from, x_expr ]))
;;
