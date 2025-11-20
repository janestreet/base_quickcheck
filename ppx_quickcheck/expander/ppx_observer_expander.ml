open! Import

let any ~loc = [%expr Ppx_quickcheck_runtime.Base_quickcheck.Observer.opaque]

let arrow
  ~observer_of_core_type
  ~generator_of_core_type
  ~loc
  ~arg_label
  ~input_type
  ~output_type
  ~portable_value
  =
  let input_generator =
    match arg_label with
    | Nolabel | Labelled _ -> generator_of_core_type input_type
    | Optional _ ->
      [%expr
        (Ppx_quickcheck_runtime.Base_quickcheck.Generator.option
        [@mode [%e portability_mode ~loc ~portable_value]])
          [%e generator_of_core_type input_type]]
  in
  let output_observer = observer_of_core_type output_type in
  let unlabelled =
    [%expr
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
      [@mode [%e portability_mode ~loc ~portable_value]])
        [%e input_generator]
        [%e output_observer]]
  in
  match arg_label with
  | Nolabel -> unlabelled
  | Labelled _ | Optional _ ->
    [%expr
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.unmap
      [@mode [%e portability_mode ~loc ~portable_value]])
        ~f:[%e fn_map_label ~loc ~from:arg_label ~to_:Nolabel]
        [%e unlabelled]]
;;

let compound_hash ~loc ~size_expr ~hash_expr ~hash_pat ~observer_exprs ~field_exprs =
  let alist = List.zip_exn observer_exprs field_exprs in
  List.fold_right alist ~init:hash_expr ~f:(fun (observer_expr, field_expr) body_expr ->
    [%expr
      let [%p hash_pat] =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
          [%e observer_expr]
          (fun () -> [%e field_expr])
          ~size:[%e size_expr]
          ~hash:[%e hash_expr]
      in
      [%e body_expr]])
;;

let compound
  (type field)
  ~observer_of_core_type
  ~loc
  ~fields
  ~portable_value
  (module Field : Field_syntax.S with type ast = field)
  =
  let pat, exp = gensym "x" loc in
  let thunk_pat, thunk_expr = gensym "f" loc in
  let fields = List.map fields ~f:Field.create in
  let field_pats, field_exprs = gensyms "x" (List.map fields ~f:Field.location) in
  let record_pat = Field.pattern fields ~loc field_pats in
  let observer_exprs =
    List.map fields ~f:(fun field -> observer_of_core_type (Field.core_type field))
  in
  let size_pat, size_expr = gensym "size" loc in
  let hash_pat, hash_expr = gensym "hash" loc in
  [%expr
    (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
    [@mode [%e portability_mode ~loc ~portable_value]])
      (fun [%p thunk_pat] ~size:[%p size_pat] ~hash:[%p hash_pat] ->
         let [%p pat] = [%e thunk_expr] () in
         let [%p record_pat] = [%e exp] in
         [%e
           compound_hash ~loc ~size_expr ~hash_expr ~hash_pat ~observer_exprs ~field_exprs])]
;;

let variant
  (type clause)
  ~observer_of_core_type
  ~loc
  ~variant_type
  ~clauses
  ~portable_value
  (module Clause : Clause_syntax.S with type ast = clause)
  =
  let clauses = Clause.create_list clauses in
  let pat, expr = gensym "x" loc in
  let thunk_pat, thunk_expr = gensym "f" loc in
  let size_pat, size_expr = gensym "size" loc in
  let hash_pat, hash_expr = gensym "hash" loc in
  [%expr
    (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
    [@mode [%e portability_mode ~loc ~portable_value]])
      (fun [%p thunk_pat] ~size:[%p size_pat] ~hash:[%p hash_pat] ->
         let [%p pat] = [%e thunk_expr] () in
         [%e
           pexp_match
             ~loc
             expr
             (List.map clauses ~f:(fun clause ->
                let core_type_list = Clause.core_type_list clause in
                let observer_exprs = List.map core_type_list ~f:observer_of_core_type in
                let field_pats, field_exprs =
                  gensyms
                    "x"
                    (List.map core_type_list ~f:(fun core_type -> core_type.ptyp_loc))
                in
                let lhs = Clause.pattern clause ~loc variant_type field_pats in
                let body =
                  compound_hash
                    ~loc
                    ~size_expr
                    ~hash_expr
                    ~hash_pat
                    ~observer_exprs
                    ~field_exprs
                in
                let rhs =
                  match Clause.salt clause with
                  | None -> body
                  | Some salt ->
                    pexp_let
                      ~loc
                      Nonrecursive
                      [ value_binding
                          ~loc
                          ~pat:hash_pat
                          ~expr:
                            [%expr
                              Ppx_quickcheck_runtime.Base.hash_fold_int
                                [%e hash_expr]
                                [%e eint ~loc salt]]
                      ]
                      body
                in
                case ~lhs ~guard:None ~rhs))])]
;;
