open! Import
include Field_syntax_intf

module Tuple = struct
  type ast = core_type
  type t = ast

  let create = Fn.id
  let location t = t.ptyp_loc
  let core_type t = t
  let pattern _ ~loc pat_list = ppat_tuple ~loc pat_list
  let expression _ ~loc expr_list = pexp_tuple ~loc expr_list
end

module Labeled_tuple = struct
  type ast = string option * core_type
  type t = ast

  let create = Fn.id
  let location (_, t) = t.ptyp_loc
  let core_type (_, t) = t

  let pattern list ~loc pat_list =
    let alist = List.map2_exn list pat_list ~f:(fun (label, _) pat -> label, pat) in
    Ppxlib_jane.Jane_syntax.Pattern.pat_of ~loc ~attrs:[] (Jpat_tuple (alist, Closed))
  ;;

  let expression list ~loc expr_list =
    let alist = List.map2_exn list expr_list ~f:(fun (label, _) expr -> label, expr) in
    Ppxlib_jane.Jane_syntax.Expression.expr_of ~loc ~attrs:[] (Jexp_tuple alist)
  ;;
end

module Record = struct
  type ast = label_declaration
  type t = ast

  let create ast = ast
  let location t = t.pld_loc
  let core_type t = t.pld_type

  let pattern list ~loc pat_list =
    let alist =
      List.map2_exn list pat_list ~f:(fun t pat -> lident_loc t.pld_name, pat)
    in
    ppat_record ~loc alist Closed
  ;;

  let expression list ~loc expr_list =
    let alist =
      List.map2_exn list expr_list ~f:(fun t expr -> lident_loc t.pld_name, expr)
    in
    pexp_record ~loc alist None
  ;;
end
