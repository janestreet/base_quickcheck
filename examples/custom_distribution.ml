open! Base
open  Base_quickcheck

module Variant : sig
  type t =
    | Nullary
    | Unary of bool
    | Binary of int * float
    | N_ary of string list
  [@@deriving quickcheck]
end = struct
  type t =
    | Nullary
    | Unary of bool
    | Binary of int * float
    | N_ary of string list
  [@@deriving compare, quickcheck, sexp_of]

  (* Perhaps we want more examples of the variants with more contents. We can override the
     default derived generator with one that uses [Generator.weighted_union] to skew the
     distribution. *)
  let quickcheck_generator =
    Generator.weighted_union [
      (1., Generator.return Nullary);
      (2., [%quickcheck.generator: bool]
           |> Generator.map ~f:(fun bool -> Unary bool));
      (4., [%quickcheck.generator: int * float]
           |> Generator.map ~f:(fun (int, float) -> Binary (int, float)));
      (10., [%quickcheck.generator: string list]
            |> Generator.map ~f:(fun list -> N_ary list));
    ]

  (* We can test our distribution: *)
  let%expect_test _ =
    let open Expect_test_helpers_kernel in
    Test.with_sample_exn
      quickcheck_generator
      ~config:{ Test.default_config with test_count = 20 }
      ~f:(fun sequence ->
        sequence
        |> Sequence.to_list
        |> List.sort ~compare
        |> List.map ~f:sexp_of_t
        |> List.iter ~f:print_s);
    [%expect {|
      Nullary
      (Unary true)
      (Unary true)
      (Binary -4611686018427387904 7.15049724116732E+210)
      (Binary -58823712978749242 1.326895442392441E+36)
      (Binary 85 -0)
      (Binary 860897119 -0.0001220703125)
      (Binary 1757545005705 -13.928729046486296)
      (Binary 195313760848289 -1.75)
      (Binary 7639858896310626 1255443193.0526123)
      (N_ary ())
      (N_ary (""))
      (N_ary (29 "" "" R 7 "" z g8 x))
      (N_ary ("7jX\171X" "" K Y4))
      (N_ary (MD))
      (N_ary (N7L))
      (N_ary (TQ v))
      (N_ary (fk0 "" "" M b b "" ""))
      (N_ary (hM B))
      (N_ary (wYe tn)) |}]
end

module Record : sig
  type t = {
    rationals : float list;
    index : int;
  }
  [@@deriving quickcheck]
end = struct
  type t = {
    rationals : float list;
    index : int;
  }
  [@@deriving compare, quickcheck, sexp_of]

  (* We might want to choose [rationals] from a distribution of finite floats, and for
     [index] to be a legal index into [rationals]. We can override the default derived
     generator with one that uses and some custom distributions, as well as
     [Generator.bind] so the choice of [index] can depend on the choice of [rationals]. *)
  let quickcheck_generator =
    let open Generator.Let_syntax in
    let%bind rationals = Generator.list_non_empty Generator.float_positive_or_zero in
    let%bind index = Generator.int_uniform_inclusive 0 (List.length rationals - 1) in
    return { rationals; index }

  (* We can test our distribution: *)
  let%expect_test _ =
    let open Expect_test_helpers_kernel in
    Test.with_sample_exn
      quickcheck_generator
      ~config:{ Test.default_config with test_count = 20 }
      ~f:(fun sequence ->
        sequence
        |> Sequence.to_list
        |> List.sort ~compare
        |> List.stable_sort
             ~compare:(Comparable.lift Int.ascending ~f:(fun t ->
               List.length t.rationals))
        |> List.map ~f:sexp_of_t
        |> List.iter ~f:print_s);
    [%expect {|
      ((rationals (2.4530950382211657E-308)) (index 0))
      ((rationals (4.2366697150646817E-308)) (index 0))
      ((rationals (3.5739111599475771E-178)) (index 0))
      ((rationals (0.01171875)) (index 0))
      ((rationals (0.125)) (index 0))
      ((rationals (0.125)) (index 0))
      ((rationals (1.9795913696289062)) (index 0))
      ((rationals (2.75)) (index 0))
      ((rationals (1157627904)) (index 0))
      ((rationals (6.1431989399976011E+106)) (index 0))
      ((rationals (2.0484140337806912E+244)) (index 0))
      ((rationals (1.1424167607754257E+308)) (index 0))
      ((rationals (1.7550194473742371E+308)) (index 0))
      ((rationals (1.1385925121758191 3.4466667342931032)) (index 1))
      ((rationals (0.01824775352821284 1.8224854137420152 5461990440960)) (index 2))
      ((rationals (0.466064453125 57.575509779824642 4.4501477170123791E-308))
       (index 2))
      ((rationals (
         3.9022946487292758E-308
         192
         1.0757961429753995E+71
         1.07706310793392E-321
         0.77307415008544922
         0.19299798905001353
         5.1212177483591671E+252
         6.1754202605911972E-23
         1.6035179932079019E-95
         3.4916006318991669E+46
         2.3443719901628377E-308
         9.54855849625892
         3.9999847412109375
         1.40631103515625))
       (index 9))
      ((rationals (
         1.5400615060210698E-18
         1.0892612374096327E-226
         18014397435740160
         9.4997854106038891E-77
         2.20056001438752
         6.1886096247576461E-10
         3.5407791527709811E-308
         2.9870388295270649E+111
         59045.107299804688
         5.8675643132203419
         4.297435799713911E-308
         6.27752527870636E-24
         2.2250738585072009E-308
         1.6166417891736373E-20))
       (index 11))
      ((rationals (
         0.9375
         4.46562265651841E+101
         50391.870195304975
         5663521048432384
         1.6875
         9.8710317674614133E-178
         0.84189605712890625
         1.7807147461350098E+308
         446422.03912734985
         1.1424624566895193E+280
         5.2438958771526814
         1.05190200652924E-313
         3.4039657583898103E+24
         1.5967894544723646E-20
         7.167464590104721E-299
         1.0061122386630193E-15))
       (index 5))
      ((rationals (
         2.716326520622772E+60
         126.14432978630066
         2.2765450197646858E-274
         237.88091122743026
         602.08834838867188
         0.062499999534338713
         547254367360
         1.28993722023237E-317
         3.638671875
         0.00014042180987416941
         134217727.875
         5.2492643594741821
         0.0060125291347503662
         2.1042999963788608E+32
         1.3189213165184165E+143
         8.8766473709256388E-05))
       (index 6)) |}]
end
