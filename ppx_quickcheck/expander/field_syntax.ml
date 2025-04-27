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

module Unboxed_tuple = struct
  type ast = core_type
  type t = ast

  let create = Fn.id
  let location t = t.ptyp_loc
  let core_type t = t

  let pattern _ ~loc pat_list =
    Ppxlib_jane.Ast_builder.Default.ppat_unboxed_tuple
      ~loc
      (List.map pat_list ~f:(fun pat -> None, pat))
      Closed
  ;;

  let expression _ ~loc expr_list =
    Ppxlib_jane.Ast_builder.Default.pexp_unboxed_tuple
      ~loc
      (List.map expr_list ~f:(fun expr -> None, expr))
  ;;
end

module Labeled_tuple = struct
  type ast = string option * core_type
  type t = ast

  let create = Fn.id
  let location (_, t) = t.ptyp_loc
  let core_type (_, t) = t

  let pattern list ~loc pat_list =
    let alist = List.map2_exn list pat_list ~f:(fun (label, _) pat -> label, pat) in
    Ppxlib_jane.Ast_builder.Default.ppat_tuple ~loc ~attrs:[] alist Closed
  ;;

  let expression list ~loc expr_list =
    let alist = List.map2_exn list expr_list ~f:(fun (label, _) expr -> label, expr) in
    Ppxlib_jane.Ast_builder.Default.pexp_tuple ~loc ~attrs:[] alist
  ;;
end

module Labeled_unboxed_tuple = struct
  type ast = string option * core_type
  type t = ast

  let create = Fn.id
  let location (_, t) = t.ptyp_loc
  let core_type (_, t) = t

  let pattern list ~loc pat_list =
    let alist = List.map2_exn list pat_list ~f:(fun (label, _) pat -> label, pat) in
    Ppxlib_jane.Ast_builder.Default.ppat_unboxed_tuple ~loc alist Closed
  ;;

  let expression list ~loc expr_list =
    let alist = List.map2_exn list expr_list ~f:(fun (label, _) expr -> label, expr) in
    Ppxlib_jane.Ast_builder.Default.pexp_unboxed_tuple ~loc alist
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

module Unboxed_record = struct
  type ast = label_declaration
  type t = ast

  let create ast = ast
  let location t = t.pld_loc
  let core_type t = t.pld_type

  let pattern list ~loc pat_list =
    let alist =
      List.map2_exn list pat_list ~f:(fun t pat -> lident_loc t.pld_name, pat)
    in
    Ppxlib_jane.Ast_builder.Default.ppat_record_unboxed_product ~loc alist Closed
  ;;

  let expression list ~loc expr_list =
    let alist =
      List.map2_exn list expr_list ~f:(fun t expr -> lident_loc t.pld_name, expr)
    in
    Ppxlib_jane.Ast_builder.Default.pexp_record_unboxed_product ~loc alist None
  ;;
end
