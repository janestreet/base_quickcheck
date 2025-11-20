open! Import

let any ~loc = [%expr Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic]
let arrow ~loc = [%expr Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic]

let compound_sequence ~loc ~make_compound_expr ~field_pats ~field_exprs ~shrinker_exprs =
  let thunk_pat, thunk_expr = gensym "f" loc in
  [%expr
    Ppx_quickcheck_runtime.Base.Sequence.round_robin
      [%e
        elist
          ~loc
          (List.map3_exn
             field_pats
             field_exprs
             shrinker_exprs
             ~f:(fun field_pat field_expr shrinker ->
               let loc = { shrinker.pexp_loc with loc_ghost = true } in
               [%expr
                 Ppx_quickcheck_runtime.Base.Sequence.map
                   (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                      [%e shrinker]
                      (fun () -> [%e field_expr]))
                   ~f:(fun [%p thunk_pat] () ->
                     let [%p field_pat] = [%e thunk_expr] () in
                     [%e make_compound_expr ~loc field_exprs])]))]]
;;

let compound
  (type field)
  ~shrinker_of_core_type
  ~loc
  ~fields
  ~portable_value
  (module Field : Field_syntax.S with type ast = field)
  =
  let thunk_pat, thunk_expr = gensym "f" loc in
  let fields = List.map fields ~f:Field.create in
  let field_pats, field_exprs = gensyms "x" (List.map fields ~f:Field.location) in
  let shrinker_exprs =
    List.map fields ~f:(fun field -> shrinker_of_core_type (Field.core_type field))
  in
  [%expr
    (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
    [@mode [%e portability_mode ~loc ~portable_value]])
      (fun [%p thunk_pat] ->
         let [%p Field.pattern fields ~loc field_pats] = [%e thunk_expr] () in
         [%e
           compound_sequence
             ~loc
             ~make_compound_expr:(Field.expression fields)
             ~field_pats
             ~field_exprs
             ~shrinker_exprs])]
;;

let variant
  (type clause)
  ~shrinker_of_core_type
  ~loc
  ~variant_type
  ~clauses
  ~portable_value
  (module Clause : Clause_syntax.S with type ast = clause)
  =
  let thunk_pat, thunk_expr = gensym "f" loc in
  let clauses = Clause.create_list clauses in
  [%expr
    (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
    [@mode [%e portability_mode ~loc ~portable_value]])
      (fun [%p thunk_pat] ->
         [%e
           pexp_match
             ~loc
             [%expr [%e thunk_expr] ()]
             (List.map clauses ~f:(fun clause ->
                let loc = { (Clause.location clause) with loc_ghost = true } in
                let core_type_list = Clause.core_type_list clause in
                let field_pats, field_exprs =
                  gensyms
                    "x"
                    (List.map core_type_list ~f:(fun core_type -> core_type.ptyp_loc))
                in
                let shrinker_exprs = List.map core_type_list ~f:shrinker_of_core_type in
                let lhs = Clause.pattern clause ~loc variant_type field_pats in
                let rhs =
                  compound_sequence
                    ~loc
                    ~make_compound_expr:(Clause.expression clause variant_type)
                    ~field_pats
                    ~field_exprs
                    ~shrinker_exprs
                in
                case ~lhs ~guard:None ~rhs))])]
;;
