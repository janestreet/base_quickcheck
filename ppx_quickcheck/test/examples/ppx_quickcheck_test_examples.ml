open Base
open Base_quickcheck

(* ensure that shadowing doesn't break anything *)
include struct
  module Base = struct end
  module Base_quickcheck = struct end
  module Quickcheckable = struct end
end

module Simple_reference = struct
  type t = bool [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()
  let quickcheck_generator = quickcheck_generator_bool
  let _ = quickcheck_generator
  let quickcheck_observer = quickcheck_observer_bool
  let _ = quickcheck_observer
  let quickcheck_shrinker = quickcheck_shrinker_bool
  let _ = quickcheck_shrinker

  [@@@end]
end

module Dotted_reference = struct
  type t = Simple_reference.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()
  let quickcheck_generator = Simple_reference.quickcheck_generator
  let _ = quickcheck_generator
  let quickcheck_observer = Simple_reference.quickcheck_observer
  let _ = quickcheck_observer
  let quickcheck_shrinker = Simple_reference.quickcheck_shrinker
  let _ = quickcheck_shrinker

  [@@@end]
end

module Nonrec_reference = struct
  open Dotted_reference

  type nonrec t = t [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()
  let quickcheck_generator = quickcheck_generator
  let _ = quickcheck_generator
  let quickcheck_observer = quickcheck_observer
  let _ = quickcheck_observer
  let quickcheck_shrinker = quickcheck_shrinker
  let _ = quickcheck_shrinker

  [@@@end]
end

module Application_of_polymorphic_type = struct
  type t = bool option [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()
  let quickcheck_generator = quickcheck_generator_option quickcheck_generator_bool
  let _ = quickcheck_generator
  let quickcheck_observer = quickcheck_observer_option quickcheck_observer_bool
  let _ = quickcheck_observer
  let quickcheck_shrinker = quickcheck_shrinker_option quickcheck_shrinker_bool
  let _ = quickcheck_shrinker

  [@@@end]
end

module Tuple = struct
  type t = bool * unit option
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
      (fun ~size:_size__011_ ~random:_random__012_ () ->
         ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             quickcheck_generator_bool
             ~size:_size__011_
             ~random:_random__012_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             (quickcheck_generator_option quickcheck_generator_unit)
             ~size:_size__011_
             ~random:_random__012_
             () ))
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__006_ ~size:_size__009_ ~hash:_hash__010_ ->
         let _x__005_ = _f__006_ () in
         let _x__007_, _x__008_ = _x__005_ in
         let _hash__010_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             quickcheck_observer_bool
             (fun () -> _x__007_)
             ~size:_size__009_
             ~hash:_hash__010_
         in
         let _hash__010_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             (quickcheck_observer_option quickcheck_observer_unit)
             (fun () -> _x__008_)
             ~size:_size__009_
             ~hash:_hash__010_
         in
         _hash__010_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__001_ ->
      let _x__002_, _x__003_ = _f__001_ () in
      Ppx_quickcheck_runtime.Base.Sequence.round_robin
        [ Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               quickcheck_shrinker_bool
               (fun () -> _x__002_))
            ~f:(fun _f__004_ () ->
              let _x__002_ = _f__004_ () in
              _x__002_, _x__003_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               (quickcheck_shrinker_option quickcheck_shrinker_unit)
               (fun () -> _x__003_))
            ~f:(fun _f__004_ () ->
              let _x__003_ = _f__004_ () in
              _x__002_, _x__003_)
        ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Poly_variant = struct
  (* deliberately make pairs of isomorphic tags to make sure we hash tags properly *)
  type t =
    [ `A
    | `B
    | `C of bool
    | `D of bool
    | `E of bool * unit option
    | `F of bool * unit option
    ]
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
      [ ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__052_ ~random:_random__053_ () -> `A) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__054_ ~random:_random__055_ () -> `B) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__056_ ~random:_random__057_ () ->
               `C
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    quickcheck_generator_bool
                    ~size:_size__056_
                    ~random:_random__057_
                    ())) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__058_ ~random:_random__059_ () ->
               `D
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    quickcheck_generator_bool
                    ~size:_size__058_
                    ~random:_random__059_
                    ())) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__062_ ~random:_random__063_ () ->
               `E
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                       (fun ~size:_size__060_ ~random:_random__061_ () ->
                          ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .generate
                              quickcheck_generator_bool
                              ~size:_size__060_
                              ~random:_random__061_
                              ()
                          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .generate
                              (quickcheck_generator_option quickcheck_generator_unit)
                              ~size:_size__060_
                              ~random:_random__061_
                              () )))
                    ~size:_size__062_
                    ~random:_random__063_
                    ())) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__066_ ~random:_random__067_ () ->
               `F
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                       (fun ~size:_size__064_ ~random:_random__065_ () ->
                          ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .generate
                              quickcheck_generator_bool
                              ~size:_size__064_
                              ~random:_random__065_
                              ()
                          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .generate
                              (quickcheck_generator_option quickcheck_generator_unit)
                              ~size:_size__064_
                              ~random:_random__065_
                              () )))
                    ~size:_size__066_
                    ~random:_random__067_
                    ())) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__033_ ~size:_size__034_ ~hash:_hash__035_ ->
         let _x__032_ = _f__033_ () in
         match _x__032_ with
         | `A ->
           let _hash__035_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__035_ 65 in
           _hash__035_
         | `B ->
           let _hash__035_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__035_ 66 in
           _hash__035_
         | `C _x__036_ ->
           let _hash__035_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__035_ 67 in
           let _hash__035_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__036_)
               ~size:_size__034_
               ~hash:_hash__035_
           in
           _hash__035_
         | `D _x__037_ ->
           let _hash__035_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__035_ 68 in
           let _hash__035_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__037_)
               ~size:_size__034_
               ~hash:_hash__035_
           in
           _hash__035_
         | `E _x__044_ ->
           let _hash__035_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__035_ 69 in
           let _hash__035_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
                  (fun _f__039_ ~size:_size__042_ ~hash:_hash__043_ ->
                     let _x__038_ = _f__039_ () in
                     let _x__040_, _x__041_ = _x__038_ in
                     let _hash__043_ =
                       Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                         quickcheck_observer_bool
                         (fun () -> _x__040_)
                         ~size:_size__042_
                         ~hash:_hash__043_
                     in
                     let _hash__043_ =
                       Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                         (quickcheck_observer_option quickcheck_observer_unit)
                         (fun () -> _x__041_)
                         ~size:_size__042_
                         ~hash:_hash__043_
                     in
                     _hash__043_))
               (fun () -> _x__044_)
               ~size:_size__034_
               ~hash:_hash__035_
           in
           _hash__035_
         | `F _x__051_ ->
           let _hash__035_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__035_ 70 in
           let _hash__035_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
                  (fun _f__046_ ~size:_size__049_ ~hash:_hash__050_ ->
                     let _x__045_ = _f__046_ () in
                     let _x__047_, _x__048_ = _x__045_ in
                     let _hash__050_ =
                       Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                         quickcheck_observer_bool
                         (fun () -> _x__047_)
                         ~size:_size__049_
                         ~hash:_hash__050_
                     in
                     let _hash__050_ =
                       Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                         (quickcheck_observer_option quickcheck_observer_unit)
                         (fun () -> _x__048_)
                         ~size:_size__049_
                         ~hash:_hash__050_
                     in
                     _hash__050_))
               (fun () -> _x__051_)
               ~size:_size__034_
               ~hash:_hash__035_
           in
           _hash__035_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__013_ ->
      match _f__013_ () with
      | `A -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | `B -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | `C _x__016_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__016_))
              ~f:(fun _f__017_ () ->
                let _x__016_ = _f__017_ () in
                `C _x__016_)
          ]
      | `D _x__018_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__018_))
              ~f:(fun _f__019_ () ->
                let _x__018_ = _f__019_ () in
                `D _x__018_)
          ]
      | `E _x__020_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
                    (fun _f__021_ ->
                       let _x__022_, _x__023_ = _f__021_ () in
                       Ppx_quickcheck_runtime.Base.Sequence.round_robin
                         [ Ppx_quickcheck_runtime.Base.Sequence.map
                             (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                              .shrink
                                quickcheck_shrinker_bool
                                (fun () -> _x__022_))
                             ~f:(fun _f__024_ () ->
                               let _x__022_ = _f__024_ () in
                               _x__022_, _x__023_)
                         ; Ppx_quickcheck_runtime.Base.Sequence.map
                             (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                              .shrink
                                (quickcheck_shrinker_option quickcheck_shrinker_unit)
                                (fun () -> _x__023_))
                             ~f:(fun _f__024_ () ->
                               let _x__023_ = _f__024_ () in
                               _x__022_, _x__023_)
                         ]))
                 (fun () -> _x__020_))
              ~f:(fun _f__025_ () ->
                let _x__020_ = _f__025_ () in
                `E _x__020_)
          ]
      | `F _x__026_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
                    (fun _f__027_ ->
                       let _x__028_, _x__029_ = _f__027_ () in
                       Ppx_quickcheck_runtime.Base.Sequence.round_robin
                         [ Ppx_quickcheck_runtime.Base.Sequence.map
                             (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                              .shrink
                                quickcheck_shrinker_bool
                                (fun () -> _x__028_))
                             ~f:(fun _f__030_ () ->
                               let _x__028_ = _f__030_ () in
                               _x__028_, _x__029_)
                         ; Ppx_quickcheck_runtime.Base.Sequence.map
                             (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                              .shrink
                                (quickcheck_shrinker_option quickcheck_shrinker_unit)
                                (fun () -> _x__029_))
                             ~f:(fun _f__030_ () ->
                               let _x__029_ = _f__030_ () in
                               _x__028_, _x__029_)
                         ]))
                 (fun () -> _x__026_))
              ~f:(fun _f__031_ () ->
                let _x__026_ = _f__031_ () in
                `F _x__026_)
          ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Inherit_poly_variant = struct
  type t =
    [ `X
    | Poly_variant.t
    | `Z of unit option
    ]
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
      [ ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__080_ ~random:_random__081_ () -> `X) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__082_ ~random:_random__083_ () ->
               (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                  Poly_variant.quickcheck_generator
                  ~size:_size__082_
                  ~random:_random__083_
                  ()
                 : Poly_variant.t
                 :> [ `X | Poly_variant.t | `Z of unit option ])) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__084_ ~random:_random__085_ () ->
               `Z
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    (quickcheck_generator_option quickcheck_generator_unit)
                    ~size:_size__084_
                    ~random:_random__085_
                    ())) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__075_ ~size:_size__076_ ~hash:_hash__077_ ->
         let _x__074_ = _f__075_ () in
         match _x__074_ with
         | `X ->
           let _hash__077_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__077_ 88 in
           _hash__077_
         | (#Poly_variant.t as _x__078_ : [ `X | Poly_variant.t | `Z of unit option ]) ->
           let _hash__077_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               Poly_variant.quickcheck_observer
               (fun () -> _x__078_)
               ~size:_size__076_
               ~hash:_hash__077_
           in
           _hash__077_
         | `Z _x__079_ ->
           let _hash__077_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__077_ 90 in
           let _hash__077_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (quickcheck_observer_option quickcheck_observer_unit)
               (fun () -> _x__079_)
               ~size:_size__076_
               ~hash:_hash__077_
           in
           _hash__077_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__068_ ->
      match _f__068_ () with
      | `X -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | (#Poly_variant.t as _x__070_ : [ `X | Poly_variant.t | `Z of unit option ]) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 Poly_variant.quickcheck_shrinker
                 (fun () -> _x__070_))
              ~f:(fun _f__071_ () ->
                let _x__070_ = _f__071_ () in
                (_x__070_ : Poly_variant.t :> [ `X | Poly_variant.t | `Z of unit option ]))
          ]
      | `Z _x__072_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (quickcheck_shrinker_option quickcheck_shrinker_unit)
                 (fun () -> _x__072_))
              ~f:(fun _f__073_ () ->
                let _x__072_ = _f__073_ () in
                `Z _x__072_)
          ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Record_type = struct
  type t =
    { mutable x : bool
    ; y : unit option
    }
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
      (fun ~size:_size__096_ ~random:_random__097_ () ->
         { x =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
               quickcheck_generator_bool
               ~size:_size__096_
               ~random:_random__097_
               ()
         ; y =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
               (quickcheck_generator_option quickcheck_generator_unit)
               ~size:_size__096_
               ~random:_random__097_
               ()
         })
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__091_ ~size:_size__094_ ~hash:_hash__095_ ->
         let _x__090_ = _f__091_ () in
         let { x = _x__092_; y = _x__093_ } = _x__090_ in
         let _hash__095_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             quickcheck_observer_bool
             (fun () -> _x__092_)
             ~size:_size__094_
             ~hash:_hash__095_
         in
         let _hash__095_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             (quickcheck_observer_option quickcheck_observer_unit)
             (fun () -> _x__093_)
             ~size:_size__094_
             ~hash:_hash__095_
         in
         _hash__095_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__086_ ->
      let { x = _x__087_; y = _x__088_ } = _f__086_ () in
      Ppx_quickcheck_runtime.Base.Sequence.round_robin
        [ Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               quickcheck_shrinker_bool
               (fun () -> _x__087_))
            ~f:(fun _f__089_ () ->
              let _x__087_ = _f__089_ () in
              { x = _x__087_; y = _x__088_ })
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               (quickcheck_shrinker_option quickcheck_shrinker_unit)
               (fun () -> _x__088_))
            ~f:(fun _f__089_ () ->
              let _x__088_ = _f__089_ () in
              { x = _x__087_; y = _x__088_ })
        ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Nullary_and_unary_variant = struct
  (* deliberately make pairs of isomorphic tags to make sure we hash tags properly *)
  type t =
    | A
    | B
    | C of unit
    | D of unit
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
      [ ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__113_ ~random:_random__114_ () ->
               let _x__115_ = (A : t) in
               _x__115_) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__116_ ~random:_random__117_ () ->
               let _x__118_ = (B : t) in
               _x__118_) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__119_ ~random:_random__120_ () ->
               let _x__121_ =
                 (C
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       quickcheck_generator_unit
                       ~size:_size__119_
                       ~random:_random__120_
                       ())
                  : t)
               in
               _x__121_) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__122_ ~random:_random__123_ () ->
               let _x__124_ =
                 (D
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       quickcheck_generator_unit
                       ~size:_size__122_
                       ~random:_random__123_
                       ())
                  : t)
               in
               _x__124_) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__108_ ~size:_size__109_ ~hash:_hash__110_ ->
         let _x__107_ = _f__108_ () in
         match _x__107_ with
         | (A : t) ->
           let _hash__110_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__110_ 0 in
           _hash__110_
         | (B : t) ->
           let _hash__110_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__110_ 1 in
           _hash__110_
         | (C _x__111_ : t) ->
           let _hash__110_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__110_ 2 in
           let _hash__110_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_unit
               (fun () -> _x__111_)
               ~size:_size__109_
               ~hash:_hash__110_
           in
           _hash__110_
         | (D _x__112_ : t) ->
           let _hash__110_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__110_ 3 in
           let _hash__110_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_unit
               (fun () -> _x__112_)
               ~size:_size__109_
               ~hash:_hash__110_
           in
           _hash__110_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__098_ ->
      match _f__098_ () with
      | (A : t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | (B : t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | (C _x__101_ : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_unit
                 (fun () -> _x__101_))
              ~f:(fun _f__102_ () ->
                let _x__101_ = _f__102_ () in
                let _x__103_ = (C _x__101_ : t) in
                _x__103_)
          ]
      | (D _x__104_ : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_unit
                 (fun () -> _x__104_))
              ~f:(fun _f__105_ () ->
                let _x__104_ = _f__105_ () in
                let _x__106_ = (D _x__104_ : t) in
                _x__106_)
          ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Binary_and_record_variant = struct
  type t =
    | A of bool * [ `X | `Y | `Z of unit ]
    | B of bool * [ `X | `Y | `Z of unit ]
    | C of
        { x : unit option
        ; mutable y : bool
        }
    | D of
        { x : unit option
        ; mutable y : bool
        }
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
      [ ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__184_ ~random:_random__185_ () ->
               let _x__186_ =
                 (A
                    ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                        quickcheck_generator_bool
                        ~size:_size__184_
                        ~random:_random__185_
                        ()
                    , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                        (Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
                           [ ( 1.
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .create
                                 (fun ~size:_size__178_ ~random:_random__179_ () -> `X) )
                           ; ( 1.
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .create
                                 (fun ~size:_size__180_ ~random:_random__181_ () -> `Y) )
                           ; ( 1.
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .create
                                 (fun ~size:_size__182_ ~random:_random__183_ () ->
                                    `Z
                                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                       .Via_thunk
                                       .generate
                                         quickcheck_generator_unit
                                         ~size:_size__182_
                                         ~random:_random__183_
                                         ())) )
                           ])
                        ~size:_size__184_
                        ~random:_random__185_
                        () )
                  : t)
               in
               _x__186_) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__193_ ~random:_random__194_ () ->
               let _x__195_ =
                 (B
                    ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                        quickcheck_generator_bool
                        ~size:_size__193_
                        ~random:_random__194_
                        ()
                    , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                        (Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
                           [ ( 1.
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .create
                                 (fun ~size:_size__187_ ~random:_random__188_ () -> `X) )
                           ; ( 1.
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .create
                                 (fun ~size:_size__189_ ~random:_random__190_ () -> `Y) )
                           ; ( 1.
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .create
                                 (fun ~size:_size__191_ ~random:_random__192_ () ->
                                    `Z
                                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                       .Via_thunk
                                       .generate
                                         quickcheck_generator_unit
                                         ~size:_size__191_
                                         ~random:_random__192_
                                         ())) )
                           ])
                        ~size:_size__193_
                        ~random:_random__194_
                        () )
                  : t)
               in
               _x__195_) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__196_ ~random:_random__197_ () ->
               let _x__198_ =
                 (C
                    { x =
                        Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                        .generate
                          (quickcheck_generator_option quickcheck_generator_unit)
                          ~size:_size__196_
                          ~random:_random__197_
                          ()
                    ; y =
                        Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                        .generate
                          quickcheck_generator_bool
                          ~size:_size__196_
                          ~random:_random__197_
                          ()
                    }
                  : t)
               in
               _x__198_) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__199_ ~random:_random__200_ () ->
               let _x__201_ =
                 (D
                    { x =
                        Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                        .generate
                          (quickcheck_generator_option quickcheck_generator_unit)
                          ~size:_size__199_
                          ~random:_random__200_
                          ()
                    ; y =
                        Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                        .generate
                          quickcheck_generator_bool
                          ~size:_size__199_
                          ~random:_random__200_
                          ()
                    }
                  : t)
               in
               _x__201_) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__157_ ~size:_size__158_ ~hash:_hash__159_ ->
         let _x__156_ = _f__157_ () in
         match _x__156_ with
         | (A (_x__165_, _x__166_) : t) ->
           let _hash__159_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__159_ 0 in
           let _hash__159_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__165_)
               ~size:_size__158_
               ~hash:_hash__159_
           in
           let _hash__159_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
                  (fun _f__161_ ~size:_size__162_ ~hash:_hash__163_ ->
                     let _x__160_ = _f__161_ () in
                     match _x__160_ with
                     | `X ->
                       let _hash__163_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__163_ 88
                       in
                       _hash__163_
                     | `Y ->
                       let _hash__163_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__163_ 89
                       in
                       _hash__163_
                     | `Z _x__164_ ->
                       let _hash__163_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__163_ 90
                       in
                       let _hash__163_ =
                         Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                           quickcheck_observer_unit
                           (fun () -> _x__164_)
                           ~size:_size__162_
                           ~hash:_hash__163_
                       in
                       _hash__163_))
               (fun () -> _x__166_)
               ~size:_size__158_
               ~hash:_hash__159_
           in
           _hash__159_
         | (B (_x__172_, _x__173_) : t) ->
           let _hash__159_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__159_ 1 in
           let _hash__159_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__172_)
               ~size:_size__158_
               ~hash:_hash__159_
           in
           let _hash__159_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
                  (fun _f__168_ ~size:_size__169_ ~hash:_hash__170_ ->
                     let _x__167_ = _f__168_ () in
                     match _x__167_ with
                     | `X ->
                       let _hash__170_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__170_ 88
                       in
                       _hash__170_
                     | `Y ->
                       let _hash__170_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__170_ 89
                       in
                       _hash__170_
                     | `Z _x__171_ ->
                       let _hash__170_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__170_ 90
                       in
                       let _hash__170_ =
                         Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                           quickcheck_observer_unit
                           (fun () -> _x__171_)
                           ~size:_size__169_
                           ~hash:_hash__170_
                       in
                       _hash__170_))
               (fun () -> _x__173_)
               ~size:_size__158_
               ~hash:_hash__159_
           in
           _hash__159_
         | (C { x = _x__174_; y = _x__175_ } : t) ->
           let _hash__159_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__159_ 2 in
           let _hash__159_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (quickcheck_observer_option quickcheck_observer_unit)
               (fun () -> _x__174_)
               ~size:_size__158_
               ~hash:_hash__159_
           in
           let _hash__159_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__175_)
               ~size:_size__158_
               ~hash:_hash__159_
           in
           _hash__159_
         | (D { x = _x__176_; y = _x__177_ } : t) ->
           let _hash__159_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__159_ 3 in
           let _hash__159_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (quickcheck_observer_option quickcheck_observer_unit)
               (fun () -> _x__176_)
               ~size:_size__158_
               ~hash:_hash__159_
           in
           let _hash__159_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__177_)
               ~size:_size__158_
               ~hash:_hash__159_
           in
           _hash__159_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__125_ ->
      match _f__125_ () with
      | (A (_x__126_, _x__127_) : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__126_))
              ~f:(fun _f__133_ () ->
                let _x__126_ = _f__133_ () in
                let _x__134_ = (A (_x__126_, _x__127_) : t) in
                _x__134_)
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
                    (fun _f__128_ ->
                       match _f__128_ () with
                       | `X -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                       | `Y -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                       | `Z _x__131_ ->
                         Ppx_quickcheck_runtime.Base.Sequence.round_robin
                           [ Ppx_quickcheck_runtime.Base.Sequence.map
                               (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                                .shrink
                                  quickcheck_shrinker_unit
                                  (fun () -> _x__131_))
                               ~f:(fun _f__132_ () ->
                                 let _x__131_ = _f__132_ () in
                                 `Z _x__131_)
                           ]))
                 (fun () -> _x__127_))
              ~f:(fun _f__133_ () ->
                let _x__127_ = _f__133_ () in
                let _x__135_ = (A (_x__126_, _x__127_) : t) in
                _x__135_)
          ]
      | (B (_x__136_, _x__137_) : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__136_))
              ~f:(fun _f__143_ () ->
                let _x__136_ = _f__143_ () in
                let _x__144_ = (B (_x__136_, _x__137_) : t) in
                _x__144_)
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
                    (fun _f__138_ ->
                       match _f__138_ () with
                       | `X -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                       | `Y -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                       | `Z _x__141_ ->
                         Ppx_quickcheck_runtime.Base.Sequence.round_robin
                           [ Ppx_quickcheck_runtime.Base.Sequence.map
                               (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                                .shrink
                                  quickcheck_shrinker_unit
                                  (fun () -> _x__141_))
                               ~f:(fun _f__142_ () ->
                                 let _x__141_ = _f__142_ () in
                                 `Z _x__141_)
                           ]))
                 (fun () -> _x__137_))
              ~f:(fun _f__143_ () ->
                let _x__137_ = _f__143_ () in
                let _x__145_ = (B (_x__136_, _x__137_) : t) in
                _x__145_)
          ]
      | (C { x = _x__146_; y = _x__147_ } : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (quickcheck_shrinker_option quickcheck_shrinker_unit)
                 (fun () -> _x__146_))
              ~f:(fun _f__148_ () ->
                let _x__146_ = _f__148_ () in
                let _x__149_ = (C { x = _x__146_; y = _x__147_ } : t) in
                _x__149_)
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__147_))
              ~f:(fun _f__148_ () ->
                let _x__147_ = _f__148_ () in
                let _x__150_ = (C { x = _x__146_; y = _x__147_ } : t) in
                _x__150_)
          ]
      | (D { x = _x__151_; y = _x__152_ } : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (quickcheck_shrinker_option quickcheck_shrinker_unit)
                 (fun () -> _x__151_))
              ~f:(fun _f__153_ () ->
                let _x__151_ = _f__153_ () in
                let _x__154_ = (D { x = _x__151_; y = _x__152_ } : t) in
                _x__154_)
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__152_))
              ~f:(fun _f__153_ () ->
                let _x__152_ = _f__153_ () in
                let _x__155_ = (D { x = _x__151_; y = _x__152_ } : t) in
                _x__155_)
          ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Simple_arrow = struct
  type t = unit option -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
      (quickcheck_observer_option quickcheck_observer_unit)
      quickcheck_generator_bool
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
      (quickcheck_generator_option quickcheck_generator_unit)
      quickcheck_observer_bool
  ;;

  let _ = quickcheck_observer
  let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
  let _ = quickcheck_shrinker

  [@@@end]
end

module Named_arrow = struct
  type t = x:unit option -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.map
      ~f:(fun _f__204_ ~x:_x__205_ -> _f__204_ _x__205_)
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
         (quickcheck_observer_option quickcheck_observer_unit)
         quickcheck_generator_bool)
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.unmap
      ~f:(fun _f__202_ _x__203_ -> _f__202_ ~x:_x__203_)
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
         (quickcheck_generator_option quickcheck_generator_unit)
         quickcheck_observer_bool)
  ;;

  let _ = quickcheck_observer
  let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
  let _ = quickcheck_shrinker

  [@@@end]
end

module Optional_arrow = struct
  type t = ?x:unit option -> unit -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.map
      ~f:(fun _f__208_ ?x:_x__209_ -> _f__208_ _x__209_)
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
         (Ppx_quickcheck_runtime.Base_quickcheck.Observer.option
            (quickcheck_observer_option quickcheck_observer_unit))
         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
            quickcheck_observer_unit
            quickcheck_generator_bool))
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.unmap
      ~f:(fun _f__206_ _x__207_ -> _f__206_ ?x:_x__207_)
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.option
            (quickcheck_generator_option quickcheck_generator_unit))
         (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
            quickcheck_generator_unit
            quickcheck_observer_bool))
  ;;

  let _ = quickcheck_observer
  let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
  let _ = quickcheck_shrinker

  [@@@end]
end

module Curried_arrow = struct
  type t = unit option -> bool option -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
      (quickcheck_observer_option quickcheck_observer_unit)
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
         (quickcheck_observer_option quickcheck_observer_bool)
         quickcheck_generator_bool)
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
      (quickcheck_generator_option quickcheck_generator_unit)
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
         (quickcheck_generator_option quickcheck_generator_bool)
         quickcheck_observer_bool)
  ;;

  let _ = quickcheck_observer
  let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
  let _ = quickcheck_shrinker

  [@@@end]
end

module Simple_higher_order = struct
  type t = (unit option -> bool option) -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
         (quickcheck_generator_option quickcheck_generator_unit)
         (quickcheck_observer_option quickcheck_observer_bool))
      quickcheck_generator_bool
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
         (quickcheck_observer_option quickcheck_observer_unit)
         (quickcheck_generator_option quickcheck_generator_bool))
      quickcheck_observer_bool
  ;;

  let _ = quickcheck_observer
  let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
  let _ = quickcheck_shrinker

  [@@@end]
end

module Named_higher_order = struct
  type t = (x:unit option -> bool option) -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.unmap
         ~f:(fun _f__212_ _x__213_ -> _f__212_ ~x:_x__213_)
         (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
            (quickcheck_generator_option quickcheck_generator_unit)
            (quickcheck_observer_option quickcheck_observer_bool)))
      quickcheck_generator_bool
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.map
         ~f:(fun _f__210_ ~x:_x__211_ -> _f__210_ _x__211_)
         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
            (quickcheck_observer_option quickcheck_observer_unit)
            (quickcheck_generator_option quickcheck_generator_bool)))
      quickcheck_observer_bool
  ;;

  let _ = quickcheck_observer
  let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
  let _ = quickcheck_shrinker

  [@@@end]
end

module Optional_higher_order = struct
  type t = (?x:unit option -> unit -> bool option) -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.unmap
         ~f:(fun _f__216_ _x__217_ -> _f__216_ ?x:_x__217_)
         (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.option
               (quickcheck_generator_option quickcheck_generator_unit))
            (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
               quickcheck_generator_unit
               (quickcheck_observer_option quickcheck_observer_bool))))
      quickcheck_generator_bool
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.map
         ~f:(fun _f__214_ ?x:_x__215_ -> _f__214_ _x__215_)
         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
            (Ppx_quickcheck_runtime.Base_quickcheck.Observer.option
               (quickcheck_observer_option quickcheck_observer_unit))
            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
               quickcheck_observer_unit
               (quickcheck_generator_option quickcheck_generator_bool))))
      quickcheck_observer_bool
  ;;

  let _ = quickcheck_observer
  let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
  let _ = quickcheck_shrinker

  [@@@end]
end

module Poly_unary = struct
  type 'a t = 'a list [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : 'a t) -> ()
  let quickcheck_generator _generator__220_ = quickcheck_generator_list _generator__220_
  let _ = quickcheck_generator
  let quickcheck_observer _observer__219_ = quickcheck_observer_list _observer__219_
  let _ = quickcheck_observer
  let quickcheck_shrinker _shrinker__218_ = quickcheck_shrinker_list _shrinker__218_
  let _ = quickcheck_shrinker

  [@@@end]
end

module Instance_of_unary = struct
  type t = bool Poly_unary.t [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()
  let quickcheck_generator = Poly_unary.quickcheck_generator quickcheck_generator_bool
  let _ = quickcheck_generator
  let quickcheck_observer = Poly_unary.quickcheck_observer quickcheck_observer_bool
  let _ = quickcheck_observer
  let quickcheck_shrinker = Poly_unary.quickcheck_shrinker quickcheck_shrinker_bool
  let _ = quickcheck_shrinker

  [@@@end]
end

module Poly_binary = struct
  type ('a, 'b) t = 'a * 'b [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let quickcheck_generator _generator__235_ _generator__236_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
      (fun ~size:_size__237_ ~random:_random__238_ () ->
         ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__235_
             ~size:_size__237_
             ~random:_random__238_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__236_
             ~size:_size__237_
             ~random:_random__238_
             () ))
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer _observer__227_ _observer__228_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__230_ ~size:_size__233_ ~hash:_hash__234_ ->
         let _x__229_ = _f__230_ () in
         let _x__231_, _x__232_ = _x__229_ in
         let _hash__234_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__227_
             (fun () -> _x__231_)
             ~size:_size__233_
             ~hash:_hash__234_
         in
         let _hash__234_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__228_
             (fun () -> _x__232_)
             ~size:_size__233_
             ~hash:_hash__234_
         in
         _hash__234_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker _shrinker__221_ _shrinker__222_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__223_ ->
      let _x__224_, _x__225_ = _f__223_ () in
      Ppx_quickcheck_runtime.Base.Sequence.round_robin
        [ Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__221_
               (fun () -> _x__224_))
            ~f:(fun _f__226_ () ->
              let _x__224_ = _f__226_ () in
              _x__224_, _x__225_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__222_
               (fun () -> _x__225_))
            ~f:(fun _f__226_ () ->
              let _x__225_ = _f__226_ () in
              _x__224_, _x__225_)
        ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Instance_of_binary = struct
  type t = (bool, unit option) Poly_binary.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Poly_binary.quickcheck_generator
      quickcheck_generator_bool
      (quickcheck_generator_option quickcheck_generator_unit)
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Poly_binary.quickcheck_observer
      quickcheck_observer_bool
      (quickcheck_observer_option quickcheck_observer_unit)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Poly_binary.quickcheck_shrinker
      quickcheck_shrinker_bool
      (quickcheck_shrinker_option quickcheck_shrinker_unit)
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Poly_ternary = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : ('a, 'b, 'c) t) -> ()

  let quickcheck_generator _generator__257_ _generator__258_ _generator__259_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
      (fun ~size:_size__260_ ~random:_random__261_ () ->
         ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__257_
             ~size:_size__260_
             ~random:_random__261_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__258_
             ~size:_size__260_
             ~random:_random__261_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__259_
             ~size:_size__260_
             ~random:_random__261_
             () ))
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer _observer__247_ _observer__248_ _observer__249_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__251_ ~size:_size__255_ ~hash:_hash__256_ ->
         let _x__250_ = _f__251_ () in
         let _x__252_, _x__253_, _x__254_ = _x__250_ in
         let _hash__256_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__247_
             (fun () -> _x__252_)
             ~size:_size__255_
             ~hash:_hash__256_
         in
         let _hash__256_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__248_
             (fun () -> _x__253_)
             ~size:_size__255_
             ~hash:_hash__256_
         in
         let _hash__256_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__249_
             (fun () -> _x__254_)
             ~size:_size__255_
             ~hash:_hash__256_
         in
         _hash__256_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker _shrinker__239_ _shrinker__240_ _shrinker__241_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__242_ ->
      let _x__243_, _x__244_, _x__245_ = _f__242_ () in
      Ppx_quickcheck_runtime.Base.Sequence.round_robin
        [ Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__239_
               (fun () -> _x__243_))
            ~f:(fun _f__246_ () ->
              let _x__243_ = _f__246_ () in
              _x__243_, _x__244_, _x__245_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__240_
               (fun () -> _x__244_))
            ~f:(fun _f__246_ () ->
              let _x__244_ = _f__246_ () in
              _x__243_, _x__244_, _x__245_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__241_
               (fun () -> _x__245_))
            ~f:(fun _f__246_ () ->
              let _x__245_ = _f__246_ () in
              _x__243_, _x__244_, _x__245_)
        ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Instance_of_ternary = struct
  type t = (bool, unit option, (unit option, bool) Poly_binary.t) Poly_ternary.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Poly_ternary.quickcheck_generator
      quickcheck_generator_bool
      (quickcheck_generator_option quickcheck_generator_unit)
      (Poly_binary.quickcheck_generator
         (quickcheck_generator_option quickcheck_generator_unit)
         quickcheck_generator_bool)
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Poly_ternary.quickcheck_observer
      quickcheck_observer_bool
      (quickcheck_observer_option quickcheck_observer_unit)
      (Poly_binary.quickcheck_observer
         (quickcheck_observer_option quickcheck_observer_unit)
         quickcheck_observer_bool)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Poly_ternary.quickcheck_shrinker
      quickcheck_shrinker_bool
      (quickcheck_shrinker_option quickcheck_shrinker_unit)
      (Poly_binary.quickcheck_shrinker
         (quickcheck_shrinker_option quickcheck_shrinker_unit)
         quickcheck_shrinker_bool)
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Poly_with_variance = struct
  type (-'a, +'b) t = 'b * ('a -> 'b)
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let quickcheck_generator _observer__276_ _generator__277_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
      (fun ~size:_size__278_ ~random:_random__279_ () ->
         ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__277_
             ~size:_size__278_
             ~random:_random__279_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
                _observer__276_
                _generator__277_)
             ~size:_size__278_
             ~random:_random__279_
             () ))
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer _generator__268_ _observer__269_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__271_ ~size:_size__274_ ~hash:_hash__275_ ->
         let _x__270_ = _f__271_ () in
         let _x__272_, _x__273_ = _x__270_ in
         let _hash__275_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__269_
             (fun () -> _x__272_)
             ~size:_size__274_
             ~hash:_hash__275_
         in
         let _hash__275_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
                _generator__268_
                _observer__269_)
             (fun () -> _x__273_)
             ~size:_size__274_
             ~hash:_hash__275_
         in
         _hash__275_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker _shrinker__262_ _shrinker__263_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__264_ ->
      let _x__265_, _x__266_ = _f__264_ () in
      Ppx_quickcheck_runtime.Base.Sequence.round_robin
        [ Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__263_
               (fun () -> _x__265_))
            ~f:(fun _f__267_ () ->
              let _x__265_ = _f__267_ () in
              _x__265_, _x__266_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
               (fun () -> _x__266_))
            ~f:(fun _f__267_ () ->
              let _x__266_ = _f__267_ () in
              _x__265_, _x__266_)
        ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Instance_with_variance = struct
  type t = (bool, unit option) Poly_with_variance.t

  (* We cannot use [@@deriving_inline quickcheck ~generator ~observer ~shrinker] [@@@end]
     here because ppx_quickcheck cannot tell the [bool] argument needs to swap generators
     with observers. *)
  let quickcheck_generator =
    Poly_with_variance.quickcheck_generator
      quickcheck_observer_bool
      (quickcheck_generator_option quickcheck_generator_unit)
  ;;

  let quickcheck_observer =
    Poly_with_variance.quickcheck_observer
      quickcheck_generator_bool
      (quickcheck_observer_option quickcheck_observer_unit)
  ;;

  let quickcheck_shrinker =
    Poly_with_variance.quickcheck_shrinker
      quickcheck_shrinker_bool
      (quickcheck_shrinker_option quickcheck_shrinker_unit)
  ;;

  let _quickcheck_generator_with_custom =
    [%quickcheck.generator: [ `Foo of [%custom quickcheck_generator] ]]
  ;;
end

module Poly_with_phantom = struct
  type _ t = unit option [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : _ t) -> ()

  let quickcheck_generator _generator__287_ =
    quickcheck_generator_option quickcheck_generator_unit
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer _observer__285_ =
    quickcheck_observer_option quickcheck_observer_unit
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker _shrinker__283_ =
    quickcheck_shrinker_option quickcheck_shrinker_unit
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Instance_with_phantom = struct
  type t = [ `phantom ] Poly_with_phantom.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Poly_with_phantom.quickcheck_generator
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
         [ ( 1.
           , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
               (fun ~size:_size__294_ ~random:_random__295_ () -> `phantom) )
         ])
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Poly_with_phantom.quickcheck_observer
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
         (fun _f__291_ ~size:_size__292_ ~hash:_hash__293_ ->
            let _x__290_ = _f__291_ () in
            match _x__290_ with
            | `phantom ->
              let _hash__293_ =
                Ppx_quickcheck_runtime.Base.hash_fold_int _hash__293_ (-378691891)
              in
              _hash__293_))
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Poly_with_phantom.quickcheck_shrinker
      (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__288_ ->
         match _f__288_ () with
         | `phantom -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []))
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Poly_with_kind = struct
  type ('a : value_or_null) t = 'a option
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : ('a : value_or_null) t) -> ()
  let quickcheck_generator _generator__298_ = quickcheck_generator_option _generator__298_
  let _ = quickcheck_generator
  let quickcheck_observer _observer__297_ = quickcheck_observer_option _observer__297_
  let _ = quickcheck_observer
  let quickcheck_shrinker _shrinker__296_ = quickcheck_shrinker_option _shrinker__296_
  let _ = quickcheck_shrinker

  [@@@end]
end

module Instance_with_kind = struct
  type t = unit or_null Poly_with_kind.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Poly_with_kind.quickcheck_generator
      (quickcheck_generator_or_null quickcheck_generator_unit)
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Poly_with_kind.quickcheck_observer
      (quickcheck_observer_or_null quickcheck_observer_unit)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Poly_with_kind.quickcheck_shrinker
      (quickcheck_shrinker_or_null quickcheck_shrinker_unit)
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Recursive = struct
  type t =
    | Leaf
    | Node of t * t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  include struct
    open struct
      let rec quickcheck_generator
        : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_generator =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
           in
           ignore quickcheck_generator;
           let _pair__315_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__320_ ~random:_random__321_ () ->
                    let _x__322_ = (Leaf : t) in
                    _x__322_) )
           and _pair__316_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__312_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__312_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__317_ ~random:_random__318_ () ->
                           let _x__319_ =
                             (Node
                                ( Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator
                                    ~size:_size__317_
                                    ~random:_random__318_
                                    ()
                                , Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator
                                    ~size:_size__317_
                                    ~random:_random__318_
                                    () )
                              : t)
                           in
                           _x__319_))) )
           in
           let _gen__313_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__315_ ]
           and _gen__314_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__315_; _pair__316_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__313_
             | _ -> _gen__314_))
      ;;

      let _ = quickcheck_generator
    end

    let quickcheck_generator =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
    ;;

    let _ = quickcheck_generator
  end

  include struct
    open struct
      let rec quickcheck_observer
        : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_observer =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
           in
           ignore quickcheck_observer;
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
             (fun _f__307_ ~size:_size__308_ ~hash:_hash__309_ ->
                let _x__306_ = _f__307_ () in
                match _x__306_ with
                | (Leaf : t) ->
                  let _hash__309_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__309_ 0
                  in
                  _hash__309_
                | (Node (_x__310_, _x__311_) : t) ->
                  let _hash__309_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__309_ 1
                  in
                  let _hash__309_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__310_)
                      ~size:_size__308_
                      ~hash:_hash__309_
                  in
                  let _hash__309_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__311_)
                      ~size:_size__308_
                      ~hash:_hash__309_
                  in
                  _hash__309_))
      ;;

      let _ = quickcheck_observer
    end

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
    ;;

    let _ = quickcheck_observer
  end

  include struct
    open struct
      let rec quickcheck_shrinker
        : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_shrinker =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
           in
           ignore quickcheck_shrinker;
           Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
             (fun _f__299_ ->
                match _f__299_ () with
                | (Leaf : t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                | (Node (_x__301_, _x__302_) : t) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__301_))
                        ~f:(fun _f__303_ () ->
                          let _x__301_ = _f__303_ () in
                          let _x__304_ = (Node (_x__301_, _x__302_) : t) in
                          _x__304_)
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__302_))
                        ~f:(fun _f__303_ () ->
                          let _x__302_ = _f__303_ () in
                          let _x__305_ = (Node (_x__301_, _x__302_) : t) in
                          _x__305_)
                    ]))
      ;;

      let _ = quickcheck_shrinker
    end

    let quickcheck_shrinker =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
    ;;

    let _ = quickcheck_shrinker
  end

  [@@@end]
end

module Recursive_with_indirect_base_case = struct
  type t = { children : t list }
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  include struct
    open struct
      let rec quickcheck_generator
        : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_generator =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
           in
           ignore quickcheck_generator;
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
             (fun ~size:_size__331_ ~random:_random__332_ () ->
                { children =
                    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                      (quickcheck_generator_list quickcheck_generator)
                      ~size:_size__331_
                      ~random:_random__332_
                      ()
                }))
      ;;

      let _ = quickcheck_generator
    end

    let quickcheck_generator =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
    ;;

    let _ = quickcheck_generator
  end

  include struct
    open struct
      let rec quickcheck_observer
        : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_observer =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
           in
           ignore quickcheck_observer;
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
             (fun _f__327_ ~size:_size__329_ ~hash:_hash__330_ ->
                let _x__326_ = _f__327_ () in
                let { children = _x__328_ } = _x__326_ in
                let _hash__330_ =
                  Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                    (quickcheck_observer_list quickcheck_observer)
                    (fun () -> _x__328_)
                    ~size:_size__329_
                    ~hash:_hash__330_
                in
                _hash__330_))
      ;;

      let _ = quickcheck_observer
    end

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
    ;;

    let _ = quickcheck_observer
  end

  include struct
    open struct
      let rec quickcheck_shrinker
        : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_shrinker =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
           in
           ignore quickcheck_shrinker;
           Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
             (fun _f__323_ ->
                let { children = _x__324_ } = _f__323_ () in
                Ppx_quickcheck_runtime.Base.Sequence.round_robin
                  [ Ppx_quickcheck_runtime.Base.Sequence.map
                      (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                         (quickcheck_shrinker_list quickcheck_shrinker)
                         (fun () -> _x__324_))
                      ~f:(fun _f__325_ () ->
                        let _x__324_ = _f__325_ () in
                        { children = _x__324_ })
                  ]))
      ;;

      let _ = quickcheck_shrinker
    end

    let quickcheck_shrinker =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
    ;;

    let _ = quickcheck_shrinker
  end

  [@@@end]
end

module Mutually_recursive = struct
  type expr =
    | Constant of int64
    | Operator of op
    | Application of expr * args

  and op =
    [ `plus
    | `minus
    | `abs
    ]

  and args = expr list [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : expr) -> ()
  let _ = fun (_ : op) -> ()
  let _ = fun (_ : args) -> ()

  include struct
    open struct
      let rec quickcheck_generator_expr
        : expr Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_generator_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_expr
           and quickcheck_generator_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_op
           and quickcheck_generator_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_args
           in
           ignore quickcheck_generator_args;
           ignore quickcheck_generator_op;
           ignore quickcheck_generator_expr;
           let _pair__364_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__373_ ~random:_random__374_ () ->
                    let _x__375_ =
                      (Constant
                         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                          .generate
                            quickcheck_generator_int64
                            ~size:_size__373_
                            ~random:_random__374_
                            ())
                       : expr)
                    in
                    _x__375_) )
           and _pair__365_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__361_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__361_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__367_ ~random:_random__368_ () ->
                           let _x__369_ =
                             (Operator
                                (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                 .Via_thunk
                                 .generate
                                   quickcheck_generator_op
                                   ~size:_size__367_
                                   ~random:_random__368_
                                   ())
                              : expr)
                           in
                           _x__369_))) )
           and _pair__366_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__361_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__361_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__370_ ~random:_random__371_ () ->
                           let _x__372_ =
                             (Application
                                ( Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator_expr
                                    ~size:_size__370_
                                    ~random:_random__371_
                                    ()
                                , Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator_args
                                    ~size:_size__370_
                                    ~random:_random__371_
                                    () )
                              : expr)
                           in
                           _x__372_))) )
           in
           let _gen__362_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__364_ ]
           and _gen__363_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__364_; _pair__365_; _pair__366_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__362_
             | _ -> _gen__363_))

      and quickcheck_generator_op
        : op Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_generator_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_expr
           and quickcheck_generator_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_op
           and quickcheck_generator_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_args
           in
           ignore quickcheck_generator_args;
           ignore quickcheck_generator_op;
           ignore quickcheck_generator_expr;
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
             [ ( 1.
               , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                   (fun ~size:_size__376_ ~random:_random__377_ () -> `plus) )
             ; ( 1.
               , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                   (fun ~size:_size__378_ ~random:_random__379_ () -> `minus) )
             ; ( 1.
               , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                   (fun ~size:_size__380_ ~random:_random__381_ () -> `abs) )
             ])

      and quickcheck_generator_args
        : args Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_generator_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_expr
           and quickcheck_generator_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_op
           and quickcheck_generator_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_args
           in
           ignore quickcheck_generator_args;
           ignore quickcheck_generator_op;
           ignore quickcheck_generator_expr;
           quickcheck_generator_list quickcheck_generator_expr)
      ;;

      let _ = quickcheck_generator_expr
      and _ = quickcheck_generator_op
      and _ = quickcheck_generator_args
    end

    let quickcheck_generator_expr =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator_expr

    and quickcheck_generator_op =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator_op

    and quickcheck_generator_args =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator_args
    ;;

    let _ = quickcheck_generator_expr
    and _ = quickcheck_generator_op
    and _ = quickcheck_generator_args
  end

  include struct
    open struct
      let rec quickcheck_observer_expr
        : expr Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_observer_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_expr
           and quickcheck_observer_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_op
           and quickcheck_observer_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_args
           in
           ignore quickcheck_observer_args;
           ignore quickcheck_observer_op;
           ignore quickcheck_observer_expr;
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
             (fun _f__350_ ~size:_size__351_ ~hash:_hash__352_ ->
                let _x__349_ = _f__350_ () in
                match _x__349_ with
                | (Constant _x__353_ : expr) ->
                  let _hash__352_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__352_ 0
                  in
                  let _hash__352_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_int64
                      (fun () -> _x__353_)
                      ~size:_size__351_
                      ~hash:_hash__352_
                  in
                  _hash__352_
                | (Operator _x__354_ : expr) ->
                  let _hash__352_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__352_ 1
                  in
                  let _hash__352_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_op
                      (fun () -> _x__354_)
                      ~size:_size__351_
                      ~hash:_hash__352_
                  in
                  _hash__352_
                | (Application (_x__355_, _x__356_) : expr) ->
                  let _hash__352_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__352_ 2
                  in
                  let _hash__352_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_expr
                      (fun () -> _x__355_)
                      ~size:_size__351_
                      ~hash:_hash__352_
                  in
                  let _hash__352_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_args
                      (fun () -> _x__356_)
                      ~size:_size__351_
                      ~hash:_hash__352_
                  in
                  _hash__352_))

      and quickcheck_observer_op
        : op Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_observer_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_expr
           and quickcheck_observer_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_op
           and quickcheck_observer_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_args
           in
           ignore quickcheck_observer_args;
           ignore quickcheck_observer_op;
           ignore quickcheck_observer_expr;
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
             (fun _f__358_ ~size:_size__359_ ~hash:_hash__360_ ->
                let _x__357_ = _f__358_ () in
                match _x__357_ with
                | `plus ->
                  let _hash__360_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__360_ (-900055206)
                  in
                  _hash__360_
                | `minus ->
                  let _hash__360_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__360_ 141066352
                  in
                  _hash__360_
                | `abs ->
                  let _hash__360_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__360_ 4845682
                  in
                  _hash__360_))

      and quickcheck_observer_args
        : args Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_observer_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_expr
           and quickcheck_observer_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_op
           and quickcheck_observer_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               quickcheck_observer_args
           in
           ignore quickcheck_observer_args;
           ignore quickcheck_observer_op;
           ignore quickcheck_observer_expr;
           quickcheck_observer_list quickcheck_observer_expr)
      ;;

      let _ = quickcheck_observer_expr
      and _ = quickcheck_observer_op
      and _ = quickcheck_observer_args
    end

    let quickcheck_observer_expr =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer_expr

    and quickcheck_observer_op =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer_op

    and quickcheck_observer_args =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer_args
    ;;

    let _ = quickcheck_observer_expr
    and _ = quickcheck_observer_op
    and _ = quickcheck_observer_args
  end

  include struct
    open struct
      let rec quickcheck_shrinker_expr
        : expr Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_shrinker_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_expr
           and quickcheck_shrinker_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_op
           and quickcheck_shrinker_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_args
           in
           ignore quickcheck_shrinker_args;
           ignore quickcheck_shrinker_op;
           ignore quickcheck_shrinker_expr;
           Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
             (fun _f__333_ ->
                match _f__333_ () with
                | (Constant _x__334_ : expr) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_int64
                           (fun () -> _x__334_))
                        ~f:(fun _f__335_ () ->
                          let _x__334_ = _f__335_ () in
                          let _x__336_ = (Constant _x__334_ : expr) in
                          _x__336_)
                    ]
                | (Operator _x__337_ : expr) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_op
                           (fun () -> _x__337_))
                        ~f:(fun _f__338_ () ->
                          let _x__337_ = _f__338_ () in
                          let _x__339_ = (Operator _x__337_ : expr) in
                          _x__339_)
                    ]
                | (Application (_x__340_, _x__341_) : expr) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_expr
                           (fun () -> _x__340_))
                        ~f:(fun _f__342_ () ->
                          let _x__340_ = _f__342_ () in
                          let _x__343_ = (Application (_x__340_, _x__341_) : expr) in
                          _x__343_)
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_args
                           (fun () -> _x__341_))
                        ~f:(fun _f__342_ () ->
                          let _x__341_ = _f__342_ () in
                          let _x__344_ = (Application (_x__340_, _x__341_) : expr) in
                          _x__344_)
                    ]))

      and quickcheck_shrinker_op
        : op Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_shrinker_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_expr
           and quickcheck_shrinker_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_op
           and quickcheck_shrinker_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_args
           in
           ignore quickcheck_shrinker_args;
           ignore quickcheck_shrinker_op;
           ignore quickcheck_shrinker_expr;
           Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
             (fun _f__345_ ->
                match _f__345_ () with
                | `plus -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                | `minus -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                | `abs -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []))

      and quickcheck_shrinker_args
        : args Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_shrinker_expr =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_expr
           and quickcheck_shrinker_op =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_op
           and quickcheck_shrinker_args =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               quickcheck_shrinker_args
           in
           ignore quickcheck_shrinker_args;
           ignore quickcheck_shrinker_op;
           ignore quickcheck_shrinker_expr;
           quickcheck_shrinker_list quickcheck_shrinker_expr)
      ;;

      let _ = quickcheck_shrinker_expr
      and _ = quickcheck_shrinker_op
      and _ = quickcheck_shrinker_args
    end

    let quickcheck_shrinker_expr =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker_expr

    and quickcheck_shrinker_op =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker_op

    and quickcheck_shrinker_args =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker_args
    ;;

    let _ = quickcheck_shrinker_expr
    and _ = quickcheck_shrinker_op
    and _ = quickcheck_shrinker_args
  end

  [@@@end]
end

module Poly_recursive = struct
  type 'a t =
    | Zero
    | Succ of 'a * 'a t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : 'a t) -> ()

  include struct
    open struct
      let rec quickcheck_generator
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        =
        fun _generator__401_ ->
        let quickcheck_generator _recur__413_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
            (lazy (quickcheck_generator _recur__413_))
        in
        ignore quickcheck_generator;
        let _pair__405_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
              (fun ~size:_size__410_ ~random:_random__411_ () ->
                 let _x__412_ = (Zero : _ t) in
                 _x__412_) )
        and _pair__406_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
              ~f:(fun _size__402_ ->
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                  ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__402_)
                  (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                     (fun ~size:_size__407_ ~random:_random__408_ () ->
                        let _x__409_ =
                          (Succ
                             ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 _generator__401_
                                 ~size:_size__407_
                                 ~random:_random__408_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 (quickcheck_generator _generator__401_)
                                 ~size:_size__407_
                                 ~random:_random__408_
                                 () )
                           : _ t)
                        in
                        _x__409_))) )
        in
        let _gen__403_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union [ _pair__405_ ]
        and _gen__404_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
            [ _pair__405_; _pair__406_ ]
        in
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
          ~f:(function
          | 0 -> _gen__403_
          | _ -> _gen__404_)
      ;;

      let _ = quickcheck_generator
    end

    let quickcheck_generator _recur__414_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
        (lazy (quickcheck_generator _recur__414_))
    ;;

    let _ = quickcheck_generator
  end

  include struct
    open struct
      let rec quickcheck_observer
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        =
        fun _observer__392_ ->
        let quickcheck_observer _recur__399_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
            (lazy (quickcheck_observer _recur__399_))
        in
        ignore quickcheck_observer;
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
          (fun _f__394_ ~size:_size__395_ ~hash:_hash__396_ ->
             let _x__393_ = _f__394_ () in
             match _x__393_ with
             | (Zero : _ t) ->
               let _hash__396_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__396_ 0
               in
               _hash__396_
             | (Succ (_x__397_, _x__398_) : _ t) ->
               let _hash__396_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__396_ 1
               in
               let _hash__396_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   _observer__392_
                   (fun () -> _x__397_)
                   ~size:_size__395_
                   ~hash:_hash__396_
               in
               let _hash__396_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   (quickcheck_observer _observer__392_)
                   (fun () -> _x__398_)
                   ~size:_size__395_
                   ~hash:_hash__396_
               in
               _hash__396_)
      ;;

      let _ = quickcheck_observer
    end

    let quickcheck_observer _recur__400_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
        (lazy (quickcheck_observer _recur__400_))
    ;;

    let _ = quickcheck_observer
  end

  include struct
    open struct
      let rec quickcheck_shrinker
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        =
        fun _shrinker__382_ ->
        let quickcheck_shrinker _recur__390_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
            (lazy (quickcheck_shrinker _recur__390_))
        in
        ignore quickcheck_shrinker;
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__383_ ->
          match _f__383_ () with
          | (Zero : _ t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
          | (Succ (_x__385_, _x__386_) : _ t) ->
            Ppx_quickcheck_runtime.Base.Sequence.round_robin
              [ Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     _shrinker__382_
                     (fun () -> _x__385_))
                  ~f:(fun _f__387_ () ->
                    let _x__385_ = _f__387_ () in
                    let _x__388_ = (Succ (_x__385_, _x__386_) : _ t) in
                    _x__388_)
              ; Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     (quickcheck_shrinker _shrinker__382_)
                     (fun () -> _x__386_))
                  ~f:(fun _f__387_ () ->
                    let _x__386_ = _f__387_ () in
                    let _x__389_ = (Succ (_x__385_, _x__386_) : _ t) in
                    _x__389_)
              ])
      ;;

      let _ = quickcheck_shrinker
    end

    let quickcheck_shrinker _recur__391_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
        (lazy (quickcheck_shrinker _recur__391_))
    ;;

    let _ = quickcheck_shrinker
  end

  [@@@end]
end

module Instance_of_recursive = struct
  type t = bool Poly_recursive.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()
  let quickcheck_generator = Poly_recursive.quickcheck_generator quickcheck_generator_bool
  let _ = quickcheck_generator
  let quickcheck_observer = Poly_recursive.quickcheck_observer quickcheck_observer_bool
  let _ = quickcheck_observer
  let quickcheck_shrinker = Poly_recursive.quickcheck_shrinker quickcheck_shrinker_bool
  let _ = quickcheck_shrinker

  [@@@end]
end

module Murec_poly_mono = struct
  type t =
    | Leaf of bool
    | Node of t node

  and 'a node = 'a list [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : 'a node) -> ()

  include struct
    open struct
      let rec quickcheck_generator
        : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_generator =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
           and quickcheck_generator_node _recur__446_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               (lazy (quickcheck_generator_node _recur__446_))
           in
           ignore quickcheck_generator_node;
           ignore quickcheck_generator;
           let _pair__437_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__442_ ~random:_random__443_ () ->
                    let _x__444_ =
                      (Leaf
                         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                          .generate
                            quickcheck_generator_bool
                            ~size:_size__442_
                            ~random:_random__443_
                            ())
                       : t)
                    in
                    _x__444_) )
           and _pair__438_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__434_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__434_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__439_ ~random:_random__440_ () ->
                           let _x__441_ =
                             (Node
                                (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                 .Via_thunk
                                 .generate
                                   (quickcheck_generator_node quickcheck_generator)
                                   ~size:_size__439_
                                   ~random:_random__440_
                                   ())
                              : t)
                           in
                           _x__441_))) )
           in
           let _gen__435_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__437_ ]
           and _gen__436_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__437_; _pair__438_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__435_
             | _ -> _gen__436_))

      and quickcheck_generator_node
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        =
        fun _generator__445_ ->
        let quickcheck_generator =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
        and quickcheck_generator_node _recur__446_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
            (lazy (quickcheck_generator_node _recur__446_))
        in
        ignore quickcheck_generator_node;
        ignore quickcheck_generator;
        quickcheck_generator_list _generator__445_
      ;;

      let _ = quickcheck_generator
      and _ = quickcheck_generator_node
    end

    let quickcheck_generator =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator

    and quickcheck_generator_node _recur__447_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
        (lazy (quickcheck_generator_node _recur__447_))
    ;;

    let _ = quickcheck_generator
    and _ = quickcheck_generator_node
  end

  include struct
    open struct
      let rec quickcheck_observer
        : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_observer =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
           and quickcheck_observer_node _recur__432_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               (lazy (quickcheck_observer_node _recur__432_))
           in
           ignore quickcheck_observer_node;
           ignore quickcheck_observer;
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
             (fun _f__426_ ~size:_size__427_ ~hash:_hash__428_ ->
                let _x__425_ = _f__426_ () in
                match _x__425_ with
                | (Leaf _x__429_ : t) ->
                  let _hash__428_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__428_ 0
                  in
                  let _hash__428_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_bool
                      (fun () -> _x__429_)
                      ~size:_size__427_
                      ~hash:_hash__428_
                  in
                  _hash__428_
                | (Node _x__430_ : t) ->
                  let _hash__428_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__428_ 1
                  in
                  let _hash__428_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      (quickcheck_observer_node quickcheck_observer)
                      (fun () -> _x__430_)
                      ~size:_size__427_
                      ~hash:_hash__428_
                  in
                  _hash__428_))

      and quickcheck_observer_node
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        =
        fun _observer__431_ ->
        let quickcheck_observer =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
        and quickcheck_observer_node _recur__432_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
            (lazy (quickcheck_observer_node _recur__432_))
        in
        ignore quickcheck_observer_node;
        ignore quickcheck_observer;
        quickcheck_observer_list _observer__431_
      ;;

      let _ = quickcheck_observer
      and _ = quickcheck_observer_node
    end

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer

    and quickcheck_observer_node _recur__433_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
        (lazy (quickcheck_observer_node _recur__433_))
    ;;

    let _ = quickcheck_observer
    and _ = quickcheck_observer_node
  end

  include struct
    open struct
      let rec quickcheck_shrinker
        : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_shrinker =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
           and quickcheck_shrinker_node _recur__423_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               (lazy (quickcheck_shrinker_node _recur__423_))
           in
           ignore quickcheck_shrinker_node;
           ignore quickcheck_shrinker;
           Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
             (fun _f__415_ ->
                match _f__415_ () with
                | (Leaf _x__416_ : t) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_bool
                           (fun () -> _x__416_))
                        ~f:(fun _f__417_ () ->
                          let _x__416_ = _f__417_ () in
                          let _x__418_ = (Leaf _x__416_ : t) in
                          _x__418_)
                    ]
                | (Node _x__419_ : t) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           (quickcheck_shrinker_node quickcheck_shrinker)
                           (fun () -> _x__419_))
                        ~f:(fun _f__420_ () ->
                          let _x__419_ = _f__420_ () in
                          let _x__421_ = (Node _x__419_ : t) in
                          _x__421_)
                    ]))

      and quickcheck_shrinker_node
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        =
        fun _shrinker__422_ ->
        let quickcheck_shrinker =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
        and quickcheck_shrinker_node _recur__423_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
            (lazy (quickcheck_shrinker_node _recur__423_))
        in
        ignore quickcheck_shrinker_node;
        ignore quickcheck_shrinker;
        quickcheck_shrinker_list _shrinker__422_
      ;;

      let _ = quickcheck_shrinker
      and _ = quickcheck_shrinker_node
    end

    let quickcheck_shrinker =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker

    and quickcheck_shrinker_node _recur__424_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
        (lazy (quickcheck_shrinker_node _recur__424_))
    ;;

    let _ = quickcheck_shrinker
    and _ = quickcheck_shrinker_node
  end

  [@@@end]
end

module Polymorphic_recursion = struct
  type 'a t =
    | Single of 'a
    | Double of ('a * 'a) t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : 'a t) -> ()

  include struct
    open struct
      let rec quickcheck_generator
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        =
        fun _generator__477_ ->
        let quickcheck_generator _recur__491_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
            (lazy (quickcheck_generator _recur__491_))
        in
        ignore quickcheck_generator;
        let _pair__481_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
              (fun ~size:_size__488_ ~random:_random__489_ () ->
                 let _x__490_ =
                   (Single
                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                         _generator__477_
                         ~size:_size__488_
                         ~random:_random__489_
                         ())
                    : _ t)
                 in
                 _x__490_) )
        and _pair__482_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
              ~f:(fun _size__478_ ->
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                  ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__478_)
                  (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                     (fun ~size:_size__485_ ~random:_random__486_ () ->
                        let _x__487_ =
                          (Double
                             (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                              .generate
                                (quickcheck_generator
                                   (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                    .Via_thunk
                                    .create
                                      (fun ~size:_size__483_ ~random:_random__484_ () ->
                                         ( Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                           .Via_thunk
                                           .generate
                                             _generator__477_
                                             ~size:_size__483_
                                             ~random:_random__484_
                                             ()
                                         , Ppx_quickcheck_runtime.Base_quickcheck
                                           .Generator
                                           .Via_thunk
                                           .generate
                                             _generator__477_
                                             ~size:_size__483_
                                             ~random:_random__484_
                                             () ))))
                                ~size:_size__485_
                                ~random:_random__486_
                                ())
                           : _ t)
                        in
                        _x__487_))) )
        in
        let _gen__479_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union [ _pair__481_ ]
        and _gen__480_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
            [ _pair__481_; _pair__482_ ]
        in
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
          ~f:(function
          | 0 -> _gen__479_
          | _ -> _gen__480_)
      ;;

      let _ = quickcheck_generator
    end

    let quickcheck_generator _recur__492_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
        (lazy (quickcheck_generator _recur__492_))
    ;;

    let _ = quickcheck_generator
  end

  include struct
    open struct
      let rec quickcheck_observer
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        =
        fun _observer__462_ ->
        let quickcheck_observer _recur__475_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
            (lazy (quickcheck_observer _recur__475_))
        in
        ignore quickcheck_observer;
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
          (fun _f__464_ ~size:_size__465_ ~hash:_hash__466_ ->
             let _x__463_ = _f__464_ () in
             match _x__463_ with
             | (Single _x__467_ : _ t) ->
               let _hash__466_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__466_ 0
               in
               let _hash__466_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   _observer__462_
                   (fun () -> _x__467_)
                   ~size:_size__465_
                   ~hash:_hash__466_
               in
               _hash__466_
             | (Double _x__474_ : _ t) ->
               let _hash__466_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__466_ 1
               in
               let _hash__466_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   (quickcheck_observer
                      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
                         (fun _f__469_ ~size:_size__472_ ~hash:_hash__473_ ->
                            let _x__468_ = _f__469_ () in
                            let _x__470_, _x__471_ = _x__468_ in
                            let _hash__473_ =
                              Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk
                              .observe
                                _observer__462_
                                (fun () -> _x__470_)
                                ~size:_size__472_
                                ~hash:_hash__473_
                            in
                            let _hash__473_ =
                              Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk
                              .observe
                                _observer__462_
                                (fun () -> _x__471_)
                                ~size:_size__472_
                                ~hash:_hash__473_
                            in
                            _hash__473_)))
                   (fun () -> _x__474_)
                   ~size:_size__465_
                   ~hash:_hash__466_
               in
               _hash__466_)
      ;;

      let _ = quickcheck_observer
    end

    let quickcheck_observer _recur__476_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
        (lazy (quickcheck_observer _recur__476_))
    ;;

    let _ = quickcheck_observer
  end

  include struct
    open struct
      let rec quickcheck_shrinker
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        =
        fun _shrinker__448_ ->
        let quickcheck_shrinker _recur__460_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
            (lazy (quickcheck_shrinker _recur__460_))
        in
        ignore quickcheck_shrinker;
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__449_ ->
          match _f__449_ () with
          | (Single _x__450_ : _ t) ->
            Ppx_quickcheck_runtime.Base.Sequence.round_robin
              [ Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     _shrinker__448_
                     (fun () -> _x__450_))
                  ~f:(fun _f__451_ () ->
                    let _x__450_ = _f__451_ () in
                    let _x__452_ = (Single _x__450_ : _ t) in
                    _x__452_)
              ]
          | (Double _x__453_ : _ t) ->
            Ppx_quickcheck_runtime.Base.Sequence.round_robin
              [ Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     (quickcheck_shrinker
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
                           (fun _f__454_ ->
                              let _x__455_, _x__456_ = _f__454_ () in
                              Ppx_quickcheck_runtime.Base.Sequence.round_robin
                                [ Ppx_quickcheck_runtime.Base.Sequence.map
                                    (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker
                                     .Via_thunk
                                     .shrink
                                       _shrinker__448_
                                       (fun () -> _x__455_))
                                    ~f:(fun _f__457_ () ->
                                      let _x__455_ = _f__457_ () in
                                      _x__455_, _x__456_)
                                ; Ppx_quickcheck_runtime.Base.Sequence.map
                                    (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker
                                     .Via_thunk
                                     .shrink
                                       _shrinker__448_
                                       (fun () -> _x__456_))
                                    ~f:(fun _f__457_ () ->
                                      let _x__456_ = _f__457_ () in
                                      _x__455_, _x__456_)
                                ])))
                     (fun () -> _x__453_))
                  ~f:(fun _f__458_ () ->
                    let _x__453_ = _f__458_ () in
                    let _x__459_ = (Double _x__453_ : _ t) in
                    _x__459_)
              ])
      ;;

      let _ = quickcheck_shrinker
    end

    let quickcheck_shrinker _recur__461_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
        (lazy (quickcheck_shrinker _recur__461_))
    ;;

    let _ = quickcheck_shrinker
  end

  [@@@end]
end

module Extensions = struct
  type t =
    [ `A
    | `B of bool * unit option
    ]

  let quickcheck_generator = [%quickcheck.generator: [ `A | `B of bool * unit option ]]
  let quickcheck_observer = [%quickcheck.observer: [ `A | `B of bool * unit option ]]
  let quickcheck_shrinker = [%quickcheck.shrinker: [ `A | `B of bool * unit option ]]
end

module Escaped = struct
  type t = int * char * bool option

  let quickcheck_generator =
    [%quickcheck.generator:
      [%custom Generator.small_strictly_positive_int] * char * bool option]
  ;;

  let quickcheck_observer =
    [%quickcheck.observer: int * [%custom Observer.opaque] * bool option]
  ;;

  let quickcheck_shrinker = [%quickcheck.shrinker: int * char * [%custom Shrinker.atomic]]
end

module Wildcard (Elt : sig
    type t

    val examples : t list
  end) =
struct
  type t = Elt.t list

  let quickcheck_generator = Generator.list (Generator.of_list Elt.examples)
  let quickcheck_observer : t Observer.t = [%quickcheck.observer: _ list]
  let quickcheck_shrinker : t Shrinker.t = [%quickcheck.shrinker: _ list]
end

module Attribute_override = struct
  type t =
    | Null [@quickcheck.weight 0.1]
    | Text of
        (string
        [@quickcheck.generator Generator.string_of Generator.char_lowercase]
        [@quickcheck.observer Observer.string]
        [@quickcheck.shrinker Shrinker.string])
    | Number of (float[@quickcheck.generator Generator.float_strictly_positive])
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
      [ ( 0.1
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__546_ ~random:_random__547_ () ->
               let _x__548_ = (Null : t) in
               _x__548_) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__549_ ~random:_random__550_ () ->
               let _x__551_ =
                 (Text
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       (Generator.string_of Generator.char_lowercase)
                       ~size:_size__549_
                       ~random:_random__550_
                       ())
                  : t)
               in
               _x__551_) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__552_ ~random:_random__553_ () ->
               let _x__554_ =
                 (Number
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       Generator.float_strictly_positive
                       ~size:_size__552_
                       ~random:_random__553_
                       ())
                  : t)
               in
               _x__554_) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__541_ ~size:_size__542_ ~hash:_hash__543_ ->
         let _x__540_ = _f__541_ () in
         match _x__540_ with
         | (Null : t) ->
           let _hash__543_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__543_ 0 in
           _hash__543_
         | (Text _x__544_ : t) ->
           let _hash__543_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__543_ 1 in
           let _hash__543_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               Observer.string
               (fun () -> _x__544_)
               ~size:_size__542_
               ~hash:_hash__543_
           in
           _hash__543_
         | (Number _x__545_ : t) ->
           let _hash__543_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__543_ 2 in
           let _hash__543_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_float
               (fun () -> _x__545_)
               ~size:_size__542_
               ~hash:_hash__543_
           in
           _hash__543_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__532_ ->
      match _f__532_ () with
      | (Null : t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | (Text _x__534_ : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 Shrinker.string
                 (fun () -> _x__534_))
              ~f:(fun _f__535_ () ->
                let _x__534_ = _f__535_ () in
                let _x__536_ = (Text _x__534_ : t) in
                _x__536_)
          ]
      | (Number _x__537_ : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_float
                 (fun () -> _x__537_))
              ~f:(fun _f__538_ () ->
                let _x__537_ = _f__538_ () in
                let _x__539_ = (Number _x__537_ : t) in
                _x__539_)
          ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Attribute_override_recursive = struct
  type t =
    | Leaf
    | Node1 of t * int64 * t [@quickcheck.weight 0.5]
    | Node2 of t * int64 * t * int64 * t [@quickcheck.weight 0.25]
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  include struct
    open struct
      let rec quickcheck_generator
        : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_generator =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
           in
           ignore quickcheck_generator;
           let _pair__590_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__599_ ~random:_random__600_ () ->
                    let _x__601_ = (Leaf : t) in
                    _x__601_) )
           and _pair__591_ =
             ( 0.5
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__587_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__587_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__593_ ~random:_random__594_ () ->
                           let _x__595_ =
                             (Node1
                                ( Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator
                                    ~size:_size__593_
                                    ~random:_random__594_
                                    ()
                                , Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator_int64
                                    ~size:_size__593_
                                    ~random:_random__594_
                                    ()
                                , Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator
                                    ~size:_size__593_
                                    ~random:_random__594_
                                    () )
                              : t)
                           in
                           _x__595_))) )
           and _pair__592_ =
             ( 0.25
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__587_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__587_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__596_ ~random:_random__597_ () ->
                           let _x__598_ =
                             (Node2
                                ( Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator
                                    ~size:_size__596_
                                    ~random:_random__597_
                                    ()
                                , Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator_int64
                                    ~size:_size__596_
                                    ~random:_random__597_
                                    ()
                                , Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator
                                    ~size:_size__596_
                                    ~random:_random__597_
                                    ()
                                , Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator_int64
                                    ~size:_size__596_
                                    ~random:_random__597_
                                    ()
                                , Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator
                                    ~size:_size__596_
                                    ~random:_random__597_
                                    () )
                              : t)
                           in
                           _x__598_))) )
           in
           let _gen__588_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__590_ ]
           and _gen__589_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__590_; _pair__591_; _pair__592_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__588_
             | _ -> _gen__589_))
      ;;

      let _ = quickcheck_generator
    end

    let quickcheck_generator =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
    ;;

    let _ = quickcheck_generator
  end

  include struct
    open struct
      let rec quickcheck_observer
        : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_observer =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
           in
           ignore quickcheck_observer;
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
             (fun _f__576_ ~size:_size__577_ ~hash:_hash__578_ ->
                let _x__575_ = _f__576_ () in
                match _x__575_ with
                | (Leaf : t) ->
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__578_ 0
                  in
                  _hash__578_
                | (Node1 (_x__579_, _x__580_, _x__581_) : t) ->
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__578_ 1
                  in
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__579_)
                      ~size:_size__577_
                      ~hash:_hash__578_
                  in
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_int64
                      (fun () -> _x__580_)
                      ~size:_size__577_
                      ~hash:_hash__578_
                  in
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__581_)
                      ~size:_size__577_
                      ~hash:_hash__578_
                  in
                  _hash__578_
                | (Node2 (_x__582_, _x__583_, _x__584_, _x__585_, _x__586_) : t) ->
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__578_ 2
                  in
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__582_)
                      ~size:_size__577_
                      ~hash:_hash__578_
                  in
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_int64
                      (fun () -> _x__583_)
                      ~size:_size__577_
                      ~hash:_hash__578_
                  in
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__584_)
                      ~size:_size__577_
                      ~hash:_hash__578_
                  in
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_int64
                      (fun () -> _x__585_)
                      ~size:_size__577_
                      ~hash:_hash__578_
                  in
                  let _hash__578_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__586_)
                      ~size:_size__577_
                      ~hash:_hash__578_
                  in
                  _hash__578_))
      ;;

      let _ = quickcheck_observer
    end

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
    ;;

    let _ = quickcheck_observer
  end

  include struct
    open struct
      let rec quickcheck_shrinker
        : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_shrinker =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
           in
           ignore quickcheck_shrinker;
           Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
             (fun _f__555_ ->
                match _f__555_ () with
                | (Leaf : t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                | (Node1 (_x__557_, _x__558_, _x__559_) : t) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__557_))
                        ~f:(fun _f__560_ () ->
                          let _x__557_ = _f__560_ () in
                          let _x__561_ = (Node1 (_x__557_, _x__558_, _x__559_) : t) in
                          _x__561_)
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_int64
                           (fun () -> _x__558_))
                        ~f:(fun _f__560_ () ->
                          let _x__558_ = _f__560_ () in
                          let _x__562_ = (Node1 (_x__557_, _x__558_, _x__559_) : t) in
                          _x__562_)
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__559_))
                        ~f:(fun _f__560_ () ->
                          let _x__559_ = _f__560_ () in
                          let _x__563_ = (Node1 (_x__557_, _x__558_, _x__559_) : t) in
                          _x__563_)
                    ]
                | (Node2 (_x__564_, _x__565_, _x__566_, _x__567_, _x__568_) : t) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__564_))
                        ~f:(fun _f__569_ () ->
                          let _x__564_ = _f__569_ () in
                          let _x__570_ =
                            (Node2 (_x__564_, _x__565_, _x__566_, _x__567_, _x__568_) : t)
                          in
                          _x__570_)
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_int64
                           (fun () -> _x__565_))
                        ~f:(fun _f__569_ () ->
                          let _x__565_ = _f__569_ () in
                          let _x__571_ =
                            (Node2 (_x__564_, _x__565_, _x__566_, _x__567_, _x__568_) : t)
                          in
                          _x__571_)
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__566_))
                        ~f:(fun _f__569_ () ->
                          let _x__566_ = _f__569_ () in
                          let _x__572_ =
                            (Node2 (_x__564_, _x__565_, _x__566_, _x__567_, _x__568_) : t)
                          in
                          _x__572_)
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_int64
                           (fun () -> _x__567_))
                        ~f:(fun _f__569_ () ->
                          let _x__567_ = _f__569_ () in
                          let _x__573_ =
                            (Node2 (_x__564_, _x__565_, _x__566_, _x__567_, _x__568_) : t)
                          in
                          _x__573_)
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__568_))
                        ~f:(fun _f__569_ () ->
                          let _x__568_ = _f__569_ () in
                          let _x__574_ =
                            (Node2 (_x__564_, _x__565_, _x__566_, _x__567_, _x__568_) : t)
                          in
                          _x__574_)
                    ]))
      ;;

      let _ = quickcheck_shrinker
    end

    let quickcheck_shrinker =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
    ;;

    let _ = quickcheck_shrinker
  end

  [@@@end]
end

module Deriving_from_wildcard = struct
  type _ transparent = string
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : _ transparent) -> ()
  let quickcheck_generator_transparent _generator__607_ = quickcheck_generator_string
  let _ = quickcheck_generator_transparent
  let quickcheck_observer_transparent _observer__605_ = quickcheck_observer_string
  let _ = quickcheck_observer_transparent
  let quickcheck_shrinker_transparent _shrinker__603_ = quickcheck_shrinker_string
  let _ = quickcheck_shrinker_transparent

  [@@@end]

  type 'a opaque = 'a option [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : 'a opaque) -> ()

  let quickcheck_generator_opaque _generator__610_ =
    quickcheck_generator_option _generator__610_
  ;;

  let _ = quickcheck_generator_opaque

  let quickcheck_observer_opaque _observer__609_ =
    quickcheck_observer_option _observer__609_
  ;;

  let _ = quickcheck_observer_opaque

  let quickcheck_shrinker_opaque _shrinker__608_ =
    quickcheck_shrinker_option _shrinker__608_
  ;;

  let _ = quickcheck_shrinker_opaque

  [@@@end]

  let compare_opaque = compare_option
  let sexp_of_opaque = sexp_of_option
  let opaque_examples = [ None; Some 0L; Some 1L ]
end

module Do_not_generate_clauses = struct
  module Cannot_generate = struct
    type t = bool option

    let all = None :: List.map Bool.all ~f:Option.return
    let compare = Option.compare Bool.compare
    let sexp_of_t = Option.sexp_of_t Bool.sexp_of_t
    let quickcheck_observer = quickcheck_observer_option quickcheck_observer_bool
    let quickcheck_shrinker = quickcheck_shrinker_option quickcheck_shrinker_bool
  end

  type t =
    | Can_generate of bool
    | Cannot_generate of Cannot_generate.t [@quickcheck.do_not_generate]
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : t) -> ()

  let quickcheck_generator =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
      [ ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__624_ ~random:_random__625_ () ->
               let _x__626_ =
                 (Can_generate
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       quickcheck_generator_bool
                       ~size:_size__624_
                       ~random:_random__625_
                       ())
                  : t)
               in
               _x__626_) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__619_ ~size:_size__620_ ~hash:_hash__621_ ->
         let _x__618_ = _f__619_ () in
         match _x__618_ with
         | (Can_generate _x__622_ : t) ->
           let _hash__621_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__621_ 0 in
           let _hash__621_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__622_)
               ~size:_size__620_
               ~hash:_hash__621_
           in
           _hash__621_
         | (Cannot_generate _x__623_ : t) ->
           let _hash__621_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__621_ 1 in
           let _hash__621_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               Cannot_generate.quickcheck_observer
               (fun () -> _x__623_)
               ~size:_size__620_
               ~hash:_hash__621_
           in
           _hash__621_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__611_ ->
      match _f__611_ () with
      | (Can_generate _x__612_ : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__612_))
              ~f:(fun _f__613_ () ->
                let _x__612_ = _f__613_ () in
                let _x__614_ = (Can_generate _x__612_ : t) in
                _x__614_)
          ]
      | (Cannot_generate _x__615_ : t) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 Cannot_generate.quickcheck_shrinker
                 (fun () -> _x__615_))
              ~f:(fun _f__616_ () ->
                let _x__615_ = _f__616_ () in
                let _x__617_ = (Cannot_generate _x__615_ : t) in
                _x__617_)
          ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]

  module Poly = struct
    type t =
      [ `Can_generate of bool
      | `Cannot_generate of Cannot_generate.t [@quickcheck.do_not_generate]
      ]
    [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

    let _ = fun (_ : t) -> ()

    let quickcheck_generator =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
        [ ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
              (fun ~size:_size__638_ ~random:_random__639_ () ->
                 `Can_generate
                   (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                      quickcheck_generator_bool
                      ~size:_size__638_
                      ~random:_random__639_
                      ())) )
        ]
    ;;

    let _ = quickcheck_generator

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
        (fun _f__633_ ~size:_size__634_ ~hash:_hash__635_ ->
           let _x__632_ = _f__633_ () in
           match _x__632_ with
           | `Can_generate _x__636_ ->
             let _hash__635_ =
               Ppx_quickcheck_runtime.Base.hash_fold_int _hash__635_ (-895996764)
             in
             let _hash__635_ =
               Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                 quickcheck_observer_bool
                 (fun () -> _x__636_)
                 ~size:_size__634_
                 ~hash:_hash__635_
             in
             _hash__635_
           | `Cannot_generate _x__637_ ->
             let _hash__635_ =
               Ppx_quickcheck_runtime.Base.hash_fold_int _hash__635_ 501585681
             in
             let _hash__635_ =
               Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                 Cannot_generate.quickcheck_observer
                 (fun () -> _x__637_)
                 ~size:_size__634_
                 ~hash:_hash__635_
             in
             _hash__635_)
    ;;

    let _ = quickcheck_observer

    let quickcheck_shrinker =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__627_ ->
        match _f__627_ () with
        | `Can_generate _x__628_ ->
          Ppx_quickcheck_runtime.Base.Sequence.round_robin
            [ Ppx_quickcheck_runtime.Base.Sequence.map
                (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                   quickcheck_shrinker_bool
                   (fun () -> _x__628_))
                ~f:(fun _f__629_ () ->
                  let _x__628_ = _f__629_ () in
                  `Can_generate _x__628_)
            ]
        | `Cannot_generate _x__630_ ->
          Ppx_quickcheck_runtime.Base.Sequence.round_robin
            [ Ppx_quickcheck_runtime.Base.Sequence.map
                (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                   Cannot_generate.quickcheck_shrinker
                   (fun () -> _x__630_))
                ~f:(fun _f__631_ () ->
                  let _x__630_ = _f__631_ () in
                  `Cannot_generate _x__630_)
            ])
    ;;

    let _ = quickcheck_shrinker

    [@@@end]
  end

  module _ = struct
    type t =
      | A
      | B of t [@quickcheck.do_not_generate]
    [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

    let _ = fun (_ : t) -> ()

    include struct
      open struct
        let rec quickcheck_generator
          : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
              Ppx_quickcheck_runtime.Base.Lazy.t
          =
          lazy
            (let quickcheck_generator =
               Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
                 quickcheck_generator
             in
             ignore quickcheck_generator;
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                     (fun ~size:_size__650_ ~random:_random__651_ () ->
                        let _x__652_ = (A : t) in
                        _x__652_) )
               ])
        ;;

        let _ = quickcheck_generator
      end

      let quickcheck_generator =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator
      ;;

      let _ = quickcheck_generator
    end

    include struct
      open struct
        let rec quickcheck_observer
          : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
              Ppx_quickcheck_runtime.Base.Lazy.t
          =
          lazy
            (let quickcheck_observer =
               Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
             in
             ignore quickcheck_observer;
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
               (fun _f__646_ ~size:_size__647_ ~hash:_hash__648_ ->
                  let _x__645_ = _f__646_ () in
                  match _x__645_ with
                  | (A : t) ->
                    let _hash__648_ =
                      Ppx_quickcheck_runtime.Base.hash_fold_int _hash__648_ 0
                    in
                    _hash__648_
                  | (B _x__649_ : t) ->
                    let _hash__648_ =
                      Ppx_quickcheck_runtime.Base.hash_fold_int _hash__648_ 1
                    in
                    let _hash__648_ =
                      Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                        quickcheck_observer
                        (fun () -> _x__649_)
                        ~size:_size__647_
                        ~hash:_hash__648_
                    in
                    _hash__648_))
        ;;

        let _ = quickcheck_observer
      end

      let quickcheck_observer =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
      ;;

      let _ = quickcheck_observer
    end

    include struct
      open struct
        let rec quickcheck_shrinker
          : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
              Ppx_quickcheck_runtime.Base.Lazy.t
          =
          lazy
            (let quickcheck_shrinker =
               Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
             in
             ignore quickcheck_shrinker;
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
               (fun _f__640_ ->
                  match _f__640_ () with
                  | (A : t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                  | (B _x__642_ : t) ->
                    Ppx_quickcheck_runtime.Base.Sequence.round_robin
                      [ Ppx_quickcheck_runtime.Base.Sequence.map
                          (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                           .shrink
                             quickcheck_shrinker
                             (fun () -> _x__642_))
                          ~f:(fun _f__643_ () ->
                            let _x__642_ = _f__643_ () in
                            let _x__644_ = (B _x__642_ : t) in
                            _x__644_)
                      ]))
        ;;

        let _ = quickcheck_shrinker
      end

      let quickcheck_shrinker =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
      ;;

      let _ = quickcheck_shrinker
    end

    [@@@end]
  end
end

module Type_based_constructor_disambiguation = struct
  type a =
    | A of a
    | B
    | C of (a, a b) c

  and 'a b =
    | A
    | B of 'a b
    | C of ('a, 'a b) c

  and ('a, 'b) c =
    | A of 'a
    | B of 'b
    | C of ('a, 'b) c
  [@@deriving_inline quickcheck]

  let _ = fun (_ : a) -> ()
  let _ = fun (_ : 'a b) -> ()
  let _ = fun (_ : ('a, 'b) c) -> ()

  include struct
    open struct
      let rec quickcheck_generator_a
        : a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_generator_a =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               quickcheck_generator_a
           and quickcheck_generator_b _recur__764_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               (lazy (quickcheck_generator_b _recur__764_))
           and quickcheck_generator_c _recur__765_ _recur__766_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               (lazy (quickcheck_generator_c _recur__765_ _recur__766_))
           in
           ignore quickcheck_generator_c;
           ignore quickcheck_generator_b;
           ignore quickcheck_generator_a;
           let _pair__719_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__728_ ~random:_random__729_ () ->
                    let _x__730_ = (B : a) in
                    _x__730_) )
           and _pair__720_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__716_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__716_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__722_ ~random:_random__723_ () ->
                           let _x__724_ =
                             (A
                                (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                 .Via_thunk
                                 .generate
                                   quickcheck_generator_a
                                   ~size:_size__722_
                                   ~random:_random__723_
                                   ())
                              : a)
                           in
                           _x__724_))) )
           and _pair__721_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__716_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__716_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__725_ ~random:_random__726_ () ->
                           let _x__727_ =
                             (C
                                (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                 .Via_thunk
                                 .generate
                                   (quickcheck_generator_c
                                      quickcheck_generator_a
                                      (quickcheck_generator_b quickcheck_generator_a))
                                   ~size:_size__725_
                                   ~random:_random__726_
                                   ())
                              : a)
                           in
                           _x__727_))) )
           in
           let _gen__717_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__719_ ]
           and _gen__718_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__719_; _pair__720_; _pair__721_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__717_
             | _ -> _gen__718_))

      and quickcheck_generator_b
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> 'a b Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        =
        fun _generator__731_ ->
        let quickcheck_generator_a =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator_a
        and quickcheck_generator_b _recur__764_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
            (lazy (quickcheck_generator_b _recur__764_))
        and quickcheck_generator_c _recur__765_ _recur__766_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
            (lazy (quickcheck_generator_c _recur__765_ _recur__766_))
        in
        ignore quickcheck_generator_c;
        ignore quickcheck_generator_b;
        ignore quickcheck_generator_a;
        let _pair__735_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
              (fun ~size:_size__744_ ~random:_random__745_ () ->
                 let _x__746_ = (A : _ b) in
                 _x__746_) )
        and _pair__736_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
              ~f:(fun _size__732_ ->
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                  ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__732_)
                  (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                     (fun ~size:_size__738_ ~random:_random__739_ () ->
                        let _x__740_ =
                          (B
                             (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                              .generate
                                (quickcheck_generator_b _generator__731_)
                                ~size:_size__738_
                                ~random:_random__739_
                                ())
                           : _ b)
                        in
                        _x__740_))) )
        and _pair__737_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
              ~f:(fun _size__732_ ->
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                  ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__732_)
                  (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                     (fun ~size:_size__741_ ~random:_random__742_ () ->
                        let _x__743_ =
                          (C
                             (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                              .generate
                                (quickcheck_generator_c
                                   _generator__731_
                                   (quickcheck_generator_b _generator__731_))
                                ~size:_size__741_
                                ~random:_random__742_
                                ())
                           : _ b)
                        in
                        _x__743_))) )
        in
        let _gen__733_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union [ _pair__735_ ]
        and _gen__734_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
            [ _pair__735_; _pair__736_; _pair__737_ ]
        in
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
          ~f:(function
          | 0 -> _gen__733_
          | _ -> _gen__734_)

      and quickcheck_generator_c
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> ('a, 'b) c Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        =
        fun _generator__747_ _generator__748_ ->
        let quickcheck_generator_a =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator_a
        and quickcheck_generator_b _recur__764_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
            (lazy (quickcheck_generator_b _recur__764_))
        and quickcheck_generator_c _recur__765_ _recur__766_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
            (lazy (quickcheck_generator_c _recur__765_ _recur__766_))
        in
        ignore quickcheck_generator_c;
        ignore quickcheck_generator_b;
        ignore quickcheck_generator_a;
        let _pair__752_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
              (fun ~size:_size__758_ ~random:_random__759_ () ->
                 let _x__760_ =
                   (A
                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                         _generator__747_
                         ~size:_size__758_
                         ~random:_random__759_
                         ())
                    : (_, _) c)
                 in
                 _x__760_) )
        and _pair__753_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
              (fun ~size:_size__761_ ~random:_random__762_ () ->
                 let _x__763_ =
                   (B
                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                         _generator__748_
                         ~size:_size__761_
                         ~random:_random__762_
                         ())
                    : (_, _) c)
                 in
                 _x__763_) )
        and _pair__754_ =
          ( 1.
          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
              ~f:(fun _size__749_ ->
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                  ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__749_)
                  (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                     (fun ~size:_size__755_ ~random:_random__756_ () ->
                        let _x__757_ =
                          (C
                             (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                              .generate
                                (quickcheck_generator_c _generator__747_ _generator__748_)
                                ~size:_size__755_
                                ~random:_random__756_
                                ())
                           : (_, _) c)
                        in
                        _x__757_))) )
        in
        let _gen__750_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
            [ _pair__752_; _pair__753_ ]
        and _gen__751_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
            [ _pair__752_; _pair__753_; _pair__754_ ]
        in
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
          ~f:(function
          | 0 -> _gen__750_
          | _ -> _gen__751_)
      ;;

      let _ = quickcheck_generator_a
      and _ = quickcheck_generator_b
      and _ = quickcheck_generator_c
    end

    let quickcheck_generator_a =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator_a

    and quickcheck_generator_b _recur__767_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
        (lazy (quickcheck_generator_b _recur__767_))

    and quickcheck_generator_c _recur__768_ _recur__769_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
        (lazy (quickcheck_generator_c _recur__768_ _recur__769_))
    ;;

    let _ = quickcheck_generator_a
    and _ = quickcheck_generator_b
    and _ = quickcheck_generator_c
  end

  include struct
    open struct
      let rec quickcheck_observer_a
        : a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_observer_a =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer_a
           and quickcheck_observer_b _recur__710_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               (lazy (quickcheck_observer_b _recur__710_))
           and quickcheck_observer_c _recur__711_ _recur__712_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               (lazy (quickcheck_observer_c _recur__711_ _recur__712_))
           in
           ignore quickcheck_observer_c;
           ignore quickcheck_observer_b;
           ignore quickcheck_observer_a;
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
             (fun _f__689_ ~size:_size__690_ ~hash:_hash__691_ ->
                let _x__688_ = _f__689_ () in
                match _x__688_ with
                | (A _x__692_ : a) ->
                  let _hash__691_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__691_ 0
                  in
                  let _hash__691_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_a
                      (fun () -> _x__692_)
                      ~size:_size__690_
                      ~hash:_hash__691_
                  in
                  _hash__691_
                | (B : a) ->
                  let _hash__691_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__691_ 1
                  in
                  _hash__691_
                | (C _x__693_ : a) ->
                  let _hash__691_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__691_ 2
                  in
                  let _hash__691_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      (quickcheck_observer_c
                         quickcheck_observer_a
                         (quickcheck_observer_b quickcheck_observer_a))
                      (fun () -> _x__693_)
                      ~size:_size__690_
                      ~hash:_hash__691_
                  in
                  _hash__691_))

      and quickcheck_observer_b
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> 'a b Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        =
        fun _observer__694_ ->
        let quickcheck_observer_a =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer_a
        and quickcheck_observer_b _recur__710_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
            (lazy (quickcheck_observer_b _recur__710_))
        and quickcheck_observer_c _recur__711_ _recur__712_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
            (lazy (quickcheck_observer_c _recur__711_ _recur__712_))
        in
        ignore quickcheck_observer_c;
        ignore quickcheck_observer_b;
        ignore quickcheck_observer_a;
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
          (fun _f__696_ ~size:_size__697_ ~hash:_hash__698_ ->
             let _x__695_ = _f__696_ () in
             match _x__695_ with
             | (A : _ b) ->
               let _hash__698_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__698_ 0
               in
               _hash__698_
             | (B _x__699_ : _ b) ->
               let _hash__698_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__698_ 1
               in
               let _hash__698_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   (quickcheck_observer_b _observer__694_)
                   (fun () -> _x__699_)
                   ~size:_size__697_
                   ~hash:_hash__698_
               in
               _hash__698_
             | (C _x__700_ : _ b) ->
               let _hash__698_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__698_ 2
               in
               let _hash__698_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   (quickcheck_observer_c
                      _observer__694_
                      (quickcheck_observer_b _observer__694_))
                   (fun () -> _x__700_)
                   ~size:_size__697_
                   ~hash:_hash__698_
               in
               _hash__698_)

      and quickcheck_observer_c
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> ('a, 'b) c Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        =
        fun _observer__701_ _observer__702_ ->
        let quickcheck_observer_a =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer_a
        and quickcheck_observer_b _recur__710_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
            (lazy (quickcheck_observer_b _recur__710_))
        and quickcheck_observer_c _recur__711_ _recur__712_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
            (lazy (quickcheck_observer_c _recur__711_ _recur__712_))
        in
        ignore quickcheck_observer_c;
        ignore quickcheck_observer_b;
        ignore quickcheck_observer_a;
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
          (fun _f__704_ ~size:_size__705_ ~hash:_hash__706_ ->
             let _x__703_ = _f__704_ () in
             match _x__703_ with
             | (A _x__707_ : (_, _) c) ->
               let _hash__706_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__706_ 0
               in
               let _hash__706_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   _observer__701_
                   (fun () -> _x__707_)
                   ~size:_size__705_
                   ~hash:_hash__706_
               in
               _hash__706_
             | (B _x__708_ : (_, _) c) ->
               let _hash__706_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__706_ 1
               in
               let _hash__706_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   _observer__702_
                   (fun () -> _x__708_)
                   ~size:_size__705_
                   ~hash:_hash__706_
               in
               _hash__706_
             | (C _x__709_ : (_, _) c) ->
               let _hash__706_ =
                 Ppx_quickcheck_runtime.Base.hash_fold_int _hash__706_ 2
               in
               let _hash__706_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                   (quickcheck_observer_c _observer__701_ _observer__702_)
                   (fun () -> _x__709_)
                   ~size:_size__705_
                   ~hash:_hash__706_
               in
               _hash__706_)
      ;;

      let _ = quickcheck_observer_a
      and _ = quickcheck_observer_b
      and _ = quickcheck_observer_c
    end

    let quickcheck_observer_a =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer_a

    and quickcheck_observer_b _recur__713_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
        (lazy (quickcheck_observer_b _recur__713_))

    and quickcheck_observer_c _recur__714_ _recur__715_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
        (lazy (quickcheck_observer_c _recur__714_ _recur__715_))
    ;;

    let _ = quickcheck_observer_a
    and _ = quickcheck_observer_b
    and _ = quickcheck_observer_c
  end

  include struct
    open struct
      let rec quickcheck_shrinker_a
        : a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
            Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (let quickcheck_shrinker_a =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker_a
           and quickcheck_shrinker_b _recur__682_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               (lazy (quickcheck_shrinker_b _recur__682_))
           and quickcheck_shrinker_c _recur__683_ _recur__684_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               (lazy (quickcheck_shrinker_c _recur__683_ _recur__684_))
           in
           ignore quickcheck_shrinker_c;
           ignore quickcheck_shrinker_b;
           ignore quickcheck_shrinker_a;
           Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
             (fun _f__653_ ->
                match _f__653_ () with
                | (A _x__654_ : a) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_a
                           (fun () -> _x__654_))
                        ~f:(fun _f__655_ () ->
                          let _x__654_ = _f__655_ () in
                          let _x__656_ = (A _x__654_ : a) in
                          _x__656_)
                    ]
                | (B : a) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                | (C _x__658_ : a) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           (quickcheck_shrinker_c
                              quickcheck_shrinker_a
                              (quickcheck_shrinker_b quickcheck_shrinker_a))
                           (fun () -> _x__658_))
                        ~f:(fun _f__659_ () ->
                          let _x__658_ = _f__659_ () in
                          let _x__660_ = (C _x__658_ : a) in
                          _x__660_)
                    ]))

      and quickcheck_shrinker_b
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'a b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        =
        fun _shrinker__661_ ->
        let quickcheck_shrinker_a =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker_a
        and quickcheck_shrinker_b _recur__682_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
            (lazy (quickcheck_shrinker_b _recur__682_))
        and quickcheck_shrinker_c _recur__683_ _recur__684_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
            (lazy (quickcheck_shrinker_c _recur__683_ _recur__684_))
        in
        ignore quickcheck_shrinker_c;
        ignore quickcheck_shrinker_b;
        ignore quickcheck_shrinker_a;
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__662_ ->
          match _f__662_ () with
          | (A : _ b) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
          | (B _x__664_ : _ b) ->
            Ppx_quickcheck_runtime.Base.Sequence.round_robin
              [ Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     (quickcheck_shrinker_b _shrinker__661_)
                     (fun () -> _x__664_))
                  ~f:(fun _f__665_ () ->
                    let _x__664_ = _f__665_ () in
                    let _x__666_ = (B _x__664_ : _ b) in
                    _x__666_)
              ]
          | (C _x__667_ : _ b) ->
            Ppx_quickcheck_runtime.Base.Sequence.round_robin
              [ Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     (quickcheck_shrinker_c
                        _shrinker__661_
                        (quickcheck_shrinker_b _shrinker__661_))
                     (fun () -> _x__667_))
                  ~f:(fun _f__668_ () ->
                    let _x__667_ = _f__668_ () in
                    let _x__669_ = (C _x__667_ : _ b) in
                    _x__669_)
              ])

      and quickcheck_shrinker_c
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> ('a, 'b) c Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        =
        fun _shrinker__670_ _shrinker__671_ ->
        let quickcheck_shrinker_a =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker_a
        and quickcheck_shrinker_b _recur__682_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
            (lazy (quickcheck_shrinker_b _recur__682_))
        and quickcheck_shrinker_c _recur__683_ _recur__684_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
            (lazy (quickcheck_shrinker_c _recur__683_ _recur__684_))
        in
        ignore quickcheck_shrinker_c;
        ignore quickcheck_shrinker_b;
        ignore quickcheck_shrinker_a;
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__672_ ->
          match _f__672_ () with
          | (A _x__673_ : (_, _) c) ->
            Ppx_quickcheck_runtime.Base.Sequence.round_robin
              [ Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     _shrinker__670_
                     (fun () -> _x__673_))
                  ~f:(fun _f__674_ () ->
                    let _x__673_ = _f__674_ () in
                    let _x__675_ = (A _x__673_ : (_, _) c) in
                    _x__675_)
              ]
          | (B _x__676_ : (_, _) c) ->
            Ppx_quickcheck_runtime.Base.Sequence.round_robin
              [ Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     _shrinker__671_
                     (fun () -> _x__676_))
                  ~f:(fun _f__677_ () ->
                    let _x__676_ = _f__677_ () in
                    let _x__678_ = (B _x__676_ : (_, _) c) in
                    _x__678_)
              ]
          | (C _x__679_ : (_, _) c) ->
            Ppx_quickcheck_runtime.Base.Sequence.round_robin
              [ Ppx_quickcheck_runtime.Base.Sequence.map
                  (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                     (quickcheck_shrinker_c _shrinker__670_ _shrinker__671_)
                     (fun () -> _x__679_))
                  ~f:(fun _f__680_ () ->
                    let _x__679_ = _f__680_ () in
                    let _x__681_ = (C _x__679_ : (_, _) c) in
                    _x__681_)
              ])
      ;;

      let _ = quickcheck_shrinker_a
      and _ = quickcheck_shrinker_b
      and _ = quickcheck_shrinker_c
    end

    let quickcheck_shrinker_a =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker_a

    and quickcheck_shrinker_b _recur__685_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
        (lazy (quickcheck_shrinker_b _recur__685_))

    and quickcheck_shrinker_c _recur__686_ _recur__687_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
        (lazy (quickcheck_shrinker_c _recur__686_ _recur__687_))
    ;;

    let _ = quickcheck_shrinker_a
    and _ = quickcheck_shrinker_b
    and _ = quickcheck_shrinker_c
  end

  [@@@end]
end
