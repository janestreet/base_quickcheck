open! Base
open! Base_quickcheck

let state = Splittable_random.create Random.State.default

let bench_gen g () =
  (* Most of these generators don't really use the size parameter anyway --- it's just the
     combinators. And for those, size 10 is a good tradeoff point between realistic size
     and generation time. *)
  Generator.generate g ~random:state ~size:10
;;

let%bench_fun "bool" = bench_gen Generator.bool
let%bench_fun "string" = bench_gen Generator.string
let%bench_fun "bytes" = bench_gen Generator.bytes
let%bench_fun "int" = bench_gen Generator.int
let%bench_fun "int32" = bench_gen Generator.int32
let%bench_fun "int63" = bench_gen Generator.int63
let%bench_fun "int64" = bench_gen Generator.int64
let%bench_fun "nativeint" = bench_gen Generator.nativeint
let%bench_fun "float" = bench_gen Generator.float
let%bench_fun "sexp" = bench_gen Generator.sexp
let%bench_fun "option" = bench_gen (Generator.option Generator.bool)
let%bench_fun "list" = bench_gen (Generator.list Generator.bool)
let%bench_fun "array" = bench_gen (Generator.array Generator.bool)
let%bench_fun "either" = bench_gen (Generator.either Generator.bool Generator.bool)
let%bench_fun "result" = bench_gen (Generator.result Generator.bool Generator.bool)

let%bench_fun "small_positive_or_zero_int" =
  bench_gen Generator.small_positive_or_zero_int
;;

let%bench_fun "small_strictly_positive_int" =
  bench_gen Generator.small_strictly_positive_int
;;

let%bench_fun "int_uniform" = bench_gen Generator.int_uniform
let%bench_fun "int32_uniform" = bench_gen Generator.int32_uniform
let%bench_fun "int63_uniform" = bench_gen Generator.int63_uniform
let%bench_fun "int64_uniform" = bench_gen Generator.int64_uniform
let%bench_fun "nativeint_uniform" = bench_gen Generator.nativeint_uniform
let%bench_fun "int_inclusive" = bench_gen (Generator.int_inclusive 0 100)
let%bench_fun "int32_inclusive" = bench_gen (Generator.int32_inclusive 0l 100l)

let%bench_fun "int63_inclusive" =
  bench_gen (Generator.int63_inclusive Int63.zero (Int63.of_int 100))
;;

let%bench_fun "int64_inclusive" = bench_gen (Generator.int64_inclusive 0L 100L)
let%bench_fun "nativeint_inclusive" = bench_gen (Generator.nativeint_inclusive 0n 100n)
let%bench_fun "int_uniform_inclusive" = bench_gen (Generator.int_uniform_inclusive 0 100)

let%bench_fun "int_log_uniform_inclusive" =
  bench_gen (Generator.int_log_uniform_inclusive 1 1000)
;;

let%bench_fun "int_log_inclusive" = bench_gen (Generator.int_log_inclusive 1 1000)
let%bench_fun "int_geometric" = bench_gen (Generator.int_geometric 0 ~p:0.3)
let%bench_fun "float_inclusive" = bench_gen (Generator.float_inclusive 0.0 100.0)

let%bench_fun "float_uniform_exclusive" =
  bench_gen (Generator.float_uniform_exclusive 0.0 100.0)
;;

let%bench_fun "float_strictly_positive" = bench_gen Generator.float_strictly_positive
let%bench_fun "char_lowercase" = bench_gen Generator.char_lowercase
let%bench_fun "char_uppercase" = bench_gen Generator.char_uppercase
let%bench_fun "char_digit" = bench_gen Generator.char_digit
let%bench_fun "char_alpha" = bench_gen Generator.char_alpha
let%bench_fun "char_alphanum" = bench_gen Generator.char_alphanum
let%bench_fun "char_whitespace" = bench_gen Generator.char_whitespace
let%bench_fun "char_print" = bench_gen Generator.char_print

let%bench_fun "char_uniform_inclusive" =
  bench_gen (Generator.char_uniform_inclusive 'a' 'z')
;;

let%bench_fun "string_non_empty" = bench_gen Generator.string_non_empty
let%bench_fun "string_with_length" = bench_gen (Generator.string_with_length ~length:20)
let%bench_fun "string_of" = bench_gen (Generator.string_of Generator.char_alphanum)

let%bench_fun "string_non_empty_of" =
  bench_gen (Generator.string_non_empty_of Generator.char_alphanum)
;;

let%bench_fun "string_with_length_of" =
  bench_gen (Generator.string_with_length_of Generator.char_alphanum ~length:15)
;;

let%bench_fun "string_like" = bench_gen (Generator.string_like "hello world")
let%bench_fun "sexp_of" = bench_gen (Generator.sexp_of Generator.string_non_empty)
let%bench_fun "list_non_empty" = bench_gen (Generator.list_non_empty Generator.int)

let%bench_fun "list_with_length" =
  bench_gen (Generator.list_with_length Generator.int ~length:10)
;;

let%bench_fun "list_filtered" =
  bench_gen (Generator.list_filtered [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ])
;;

let%bench_fun "list_permutations" =
  bench_gen (Generator.list_permutations [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ])
;;

let%bench_fun "bigarray1" =
  bench_gen
    (Generator.bigarray1 Generator.int Bigarray.int Bigarray.c_layout ~length:(Some 100))
;;

let%bench_fun "bigstring_with_length" =
  bench_gen (Generator.bigstring_with_length ~length:100)
;;

let%bench_fun "float32_vec_with_length" =
  bench_gen (Generator.float32_vec_with_length ~length:100)
;;

let%bench_fun "float64_vec_with_length" =
  bench_gen (Generator.float64_vec_with_length ~length:100)
;;

let%bench_fun "of_list" = bench_gen (Generator.of_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ])

let%bench_fun "of_weighted_list" =
  bench_gen (Generator.of_weighted_list [ 1.0, 1; 5.0, 2; 2.0, 3; 9.0, 4; 1.0, 5 ])
;;

let%bench_fun "union" =
  bench_gen
    (Generator.union [ Generator.bool; Generator.bool; Generator.bool; Generator.bool ])
;;

let%bench_fun "weighted_union" =
  bench_gen
    (Generator.weighted_union
       [ 1.0, Generator.bool
       ; 4.0, Generator.bool
       ; 5.0, Generator.bool
       ; 2.0, Generator.bool
       ])
;;

let%bench_fun "recursive_union" =
  bench_gen
    (Generator.recursive_union
       [ Generator.return [] ]
       ~f:(fun gt ->
         [ Generator.map ~f:(fun (x, xs) -> x :: xs) (Generator.both Generator.bool gt) ]))
;;

let%bench_fun "weighted_recursive_union" =
  bench_gen
    (Generator.weighted_recursive_union
       [ 1.0, Generator.return [] ]
       ~f:(fun gt ->
         [ 1.0, Generator.return []
         ; ( 10.0
           , Generator.map ~f:(fun (x, xs) -> x :: xs) (Generator.both Generator.bool gt)
           )
         ]))
;;
