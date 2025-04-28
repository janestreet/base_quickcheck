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
         | #Poly_variant.t as _x__078_ ->
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
      | #Poly_variant.t as _x__070_ ->
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
            (fun ~size:_size__111_ ~random:_random__112_ () -> A) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__113_ ~random:_random__114_ () -> B) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__115_ ~random:_random__116_ () ->
               C
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    quickcheck_generator_unit
                    ~size:_size__115_
                    ~random:_random__116_
                    ())) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__117_ ~random:_random__118_ () ->
               D
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    quickcheck_generator_unit
                    ~size:_size__117_
                    ~random:_random__118_
                    ())) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__106_ ~size:_size__107_ ~hash:_hash__108_ ->
         let _x__105_ = _f__106_ () in
         match _x__105_ with
         | A ->
           let _hash__108_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__108_ 0 in
           _hash__108_
         | B ->
           let _hash__108_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__108_ 1 in
           _hash__108_
         | C _x__109_ ->
           let _hash__108_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__108_ 2 in
           let _hash__108_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_unit
               (fun () -> _x__109_)
               ~size:_size__107_
               ~hash:_hash__108_
           in
           _hash__108_
         | D _x__110_ ->
           let _hash__108_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__108_ 3 in
           let _hash__108_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_unit
               (fun () -> _x__110_)
               ~size:_size__107_
               ~hash:_hash__108_
           in
           _hash__108_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__098_ ->
      match _f__098_ () with
      | A -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | B -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | C _x__101_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_unit
                 (fun () -> _x__101_))
              ~f:(fun _f__102_ () ->
                let _x__101_ = _f__102_ () in
                C _x__101_)
          ]
      | D _x__103_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_unit
                 (fun () -> _x__103_))
              ~f:(fun _f__104_ () ->
                let _x__103_ = _f__104_ () in
                D _x__103_)
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
            (fun ~size:_size__170_ ~random:_random__171_ () ->
               A
                 ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                     quickcheck_generator_bool
                     ~size:_size__170_
                     ~random:_random__171_
                     ()
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
                        [ ( 1.
                          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .create
                              (fun ~size:_size__164_ ~random:_random__165_ () -> `X) )
                        ; ( 1.
                          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .create
                              (fun ~size:_size__166_ ~random:_random__167_ () -> `Y) )
                        ; ( 1.
                          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .create
                              (fun ~size:_size__168_ ~random:_random__169_ () ->
                                 `Z
                                   (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                    .Via_thunk
                                    .generate
                                      quickcheck_generator_unit
                                      ~size:_size__168_
                                      ~random:_random__169_
                                      ())) )
                        ])
                     ~size:_size__170_
                     ~random:_random__171_
                     () )) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__178_ ~random:_random__179_ () ->
               B
                 ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                     quickcheck_generator_bool
                     ~size:_size__178_
                     ~random:_random__179_
                     ()
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
                        [ ( 1.
                          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .create
                              (fun ~size:_size__172_ ~random:_random__173_ () -> `X) )
                        ; ( 1.
                          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .create
                              (fun ~size:_size__174_ ~random:_random__175_ () -> `Y) )
                        ; ( 1.
                          , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                            .create
                              (fun ~size:_size__176_ ~random:_random__177_ () ->
                                 `Z
                                   (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                    .Via_thunk
                                    .generate
                                      quickcheck_generator_unit
                                      ~size:_size__176_
                                      ~random:_random__177_
                                      ())) )
                        ])
                     ~size:_size__178_
                     ~random:_random__179_
                     () )) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__180_ ~random:_random__181_ () ->
               C
                 { x =
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       (quickcheck_generator_option quickcheck_generator_unit)
                       ~size:_size__180_
                       ~random:_random__181_
                       ()
                 ; y =
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       quickcheck_generator_bool
                       ~size:_size__180_
                       ~random:_random__181_
                       ()
                 }) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__182_ ~random:_random__183_ () ->
               D
                 { x =
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       (quickcheck_generator_option quickcheck_generator_unit)
                       ~size:_size__182_
                       ~random:_random__183_
                       ()
                 ; y =
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                       quickcheck_generator_bool
                       ~size:_size__182_
                       ~random:_random__183_
                       ()
                 }) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__143_ ~size:_size__144_ ~hash:_hash__145_ ->
         let _x__142_ = _f__143_ () in
         match _x__142_ with
         | A (_x__151_, _x__152_) ->
           let _hash__145_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__145_ 0 in
           let _hash__145_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__151_)
               ~size:_size__144_
               ~hash:_hash__145_
           in
           let _hash__145_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
                  (fun _f__147_ ~size:_size__148_ ~hash:_hash__149_ ->
                     let _x__146_ = _f__147_ () in
                     match _x__146_ with
                     | `X ->
                       let _hash__149_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__149_ 88
                       in
                       _hash__149_
                     | `Y ->
                       let _hash__149_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__149_ 89
                       in
                       _hash__149_
                     | `Z _x__150_ ->
                       let _hash__149_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__149_ 90
                       in
                       let _hash__149_ =
                         Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                           quickcheck_observer_unit
                           (fun () -> _x__150_)
                           ~size:_size__148_
                           ~hash:_hash__149_
                       in
                       _hash__149_))
               (fun () -> _x__152_)
               ~size:_size__144_
               ~hash:_hash__145_
           in
           _hash__145_
         | B (_x__158_, _x__159_) ->
           let _hash__145_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__145_ 1 in
           let _hash__145_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__158_)
               ~size:_size__144_
               ~hash:_hash__145_
           in
           let _hash__145_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
                  (fun _f__154_ ~size:_size__155_ ~hash:_hash__156_ ->
                     let _x__153_ = _f__154_ () in
                     match _x__153_ with
                     | `X ->
                       let _hash__156_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__156_ 88
                       in
                       _hash__156_
                     | `Y ->
                       let _hash__156_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__156_ 89
                       in
                       _hash__156_
                     | `Z _x__157_ ->
                       let _hash__156_ =
                         Ppx_quickcheck_runtime.Base.hash_fold_int _hash__156_ 90
                       in
                       let _hash__156_ =
                         Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                           quickcheck_observer_unit
                           (fun () -> _x__157_)
                           ~size:_size__155_
                           ~hash:_hash__156_
                       in
                       _hash__156_))
               (fun () -> _x__159_)
               ~size:_size__144_
               ~hash:_hash__145_
           in
           _hash__145_
         | C { x = _x__160_; y = _x__161_ } ->
           let _hash__145_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__145_ 2 in
           let _hash__145_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (quickcheck_observer_option quickcheck_observer_unit)
               (fun () -> _x__160_)
               ~size:_size__144_
               ~hash:_hash__145_
           in
           let _hash__145_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__161_)
               ~size:_size__144_
               ~hash:_hash__145_
           in
           _hash__145_
         | D { x = _x__162_; y = _x__163_ } ->
           let _hash__145_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__145_ 3 in
           let _hash__145_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (quickcheck_observer_option quickcheck_observer_unit)
               (fun () -> _x__162_)
               ~size:_size__144_
               ~hash:_hash__145_
           in
           let _hash__145_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__163_)
               ~size:_size__144_
               ~hash:_hash__145_
           in
           _hash__145_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__119_ ->
      match _f__119_ () with
      | A (_x__120_, _x__121_) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__120_))
              ~f:(fun _f__127_ () ->
                let _x__120_ = _f__127_ () in
                A (_x__120_, _x__121_))
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
                    (fun _f__122_ ->
                       match _f__122_ () with
                       | `X -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                       | `Y -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                       | `Z _x__125_ ->
                         Ppx_quickcheck_runtime.Base.Sequence.round_robin
                           [ Ppx_quickcheck_runtime.Base.Sequence.map
                               (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                                .shrink
                                  quickcheck_shrinker_unit
                                  (fun () -> _x__125_))
                               ~f:(fun _f__126_ () ->
                                 let _x__125_ = _f__126_ () in
                                 `Z _x__125_)
                           ]))
                 (fun () -> _x__121_))
              ~f:(fun _f__127_ () ->
                let _x__121_ = _f__127_ () in
                A (_x__120_, _x__121_))
          ]
      | B (_x__128_, _x__129_) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__128_))
              ~f:(fun _f__135_ () ->
                let _x__128_ = _f__135_ () in
                B (_x__128_, _x__129_))
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
                    (fun _f__130_ ->
                       match _f__130_ () with
                       | `X -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                       | `Y -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                       | `Z _x__133_ ->
                         Ppx_quickcheck_runtime.Base.Sequence.round_robin
                           [ Ppx_quickcheck_runtime.Base.Sequence.map
                               (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                                .shrink
                                  quickcheck_shrinker_unit
                                  (fun () -> _x__133_))
                               ~f:(fun _f__134_ () ->
                                 let _x__133_ = _f__134_ () in
                                 `Z _x__133_)
                           ]))
                 (fun () -> _x__129_))
              ~f:(fun _f__135_ () ->
                let _x__129_ = _f__135_ () in
                B (_x__128_, _x__129_))
          ]
      | C { x = _x__136_; y = _x__137_ } ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (quickcheck_shrinker_option quickcheck_shrinker_unit)
                 (fun () -> _x__136_))
              ~f:(fun _f__138_ () ->
                let _x__136_ = _f__138_ () in
                C { x = _x__136_; y = _x__137_ })
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__137_))
              ~f:(fun _f__138_ () ->
                let _x__137_ = _f__138_ () in
                C { x = _x__136_; y = _x__137_ })
          ]
      | D { x = _x__139_; y = _x__140_ } ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 (quickcheck_shrinker_option quickcheck_shrinker_unit)
                 (fun () -> _x__139_))
              ~f:(fun _f__141_ () ->
                let _x__139_ = _f__141_ () in
                D { x = _x__139_; y = _x__140_ })
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__140_))
              ~f:(fun _f__141_ () ->
                let _x__140_ = _f__141_ () in
                D { x = _x__139_; y = _x__140_ })
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
      ~f:(fun _f__186_ ~x:_x__187_ -> _f__186_ _x__187_)
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
         (quickcheck_observer_option quickcheck_observer_unit)
         quickcheck_generator_bool)
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.unmap
      ~f:(fun _f__184_ _x__185_ -> _f__184_ ~x:_x__185_)
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
      ~f:(fun _f__190_ ?x:_x__191_ -> _f__190_ _x__191_)
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
      ~f:(fun _f__188_ _x__189_ -> _f__188_ ?x:_x__189_)
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
         ~f:(fun _f__194_ _x__195_ -> _f__194_ ~x:_x__195_)
         (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
            (quickcheck_generator_option quickcheck_generator_unit)
            (quickcheck_observer_option quickcheck_observer_bool)))
      quickcheck_generator_bool
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.map
         ~f:(fun _f__192_ ~x:_x__193_ -> _f__192_ _x__193_)
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
         ~f:(fun _f__198_ _x__199_ -> _f__198_ ?x:_x__199_)
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
         ~f:(fun _f__196_ ?x:_x__197_ -> _f__196_ _x__197_)
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
  let quickcheck_generator _generator__202_ = quickcheck_generator_list _generator__202_
  let _ = quickcheck_generator
  let quickcheck_observer _observer__201_ = quickcheck_observer_list _observer__201_
  let _ = quickcheck_observer
  let quickcheck_shrinker _shrinker__200_ = quickcheck_shrinker_list _shrinker__200_
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

  let quickcheck_generator _generator__217_ _generator__218_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
      (fun ~size:_size__219_ ~random:_random__220_ () ->
         ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__217_
             ~size:_size__219_
             ~random:_random__220_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__218_
             ~size:_size__219_
             ~random:_random__220_
             () ))
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer _observer__209_ _observer__210_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__212_ ~size:_size__215_ ~hash:_hash__216_ ->
         let _x__211_ = _f__212_ () in
         let _x__213_, _x__214_ = _x__211_ in
         let _hash__216_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__209_
             (fun () -> _x__213_)
             ~size:_size__215_
             ~hash:_hash__216_
         in
         let _hash__216_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__210_
             (fun () -> _x__214_)
             ~size:_size__215_
             ~hash:_hash__216_
         in
         _hash__216_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker _shrinker__203_ _shrinker__204_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__205_ ->
      let _x__206_, _x__207_ = _f__205_ () in
      Ppx_quickcheck_runtime.Base.Sequence.round_robin
        [ Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__203_
               (fun () -> _x__206_))
            ~f:(fun _f__208_ () ->
              let _x__206_ = _f__208_ () in
              _x__206_, _x__207_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__204_
               (fun () -> _x__207_))
            ~f:(fun _f__208_ () ->
              let _x__207_ = _f__208_ () in
              _x__206_, _x__207_)
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

  let quickcheck_generator _generator__239_ _generator__240_ _generator__241_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
      (fun ~size:_size__242_ ~random:_random__243_ () ->
         ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__239_
             ~size:_size__242_
             ~random:_random__243_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__240_
             ~size:_size__242_
             ~random:_random__243_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__241_
             ~size:_size__242_
             ~random:_random__243_
             () ))
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer _observer__229_ _observer__230_ _observer__231_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__233_ ~size:_size__237_ ~hash:_hash__238_ ->
         let _x__232_ = _f__233_ () in
         let _x__234_, _x__235_, _x__236_ = _x__232_ in
         let _hash__238_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__229_
             (fun () -> _x__234_)
             ~size:_size__237_
             ~hash:_hash__238_
         in
         let _hash__238_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__230_
             (fun () -> _x__235_)
             ~size:_size__237_
             ~hash:_hash__238_
         in
         let _hash__238_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__231_
             (fun () -> _x__236_)
             ~size:_size__237_
             ~hash:_hash__238_
         in
         _hash__238_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker _shrinker__221_ _shrinker__222_ _shrinker__223_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__224_ ->
      let _x__225_, _x__226_, _x__227_ = _f__224_ () in
      Ppx_quickcheck_runtime.Base.Sequence.round_robin
        [ Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__221_
               (fun () -> _x__225_))
            ~f:(fun _f__228_ () ->
              let _x__225_ = _f__228_ () in
              _x__225_, _x__226_, _x__227_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__222_
               (fun () -> _x__226_))
            ~f:(fun _f__228_ () ->
              let _x__226_ = _f__228_ () in
              _x__225_, _x__226_, _x__227_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__223_
               (fun () -> _x__227_))
            ~f:(fun _f__228_ () ->
              let _x__227_ = _f__228_ () in
              _x__225_, _x__226_, _x__227_)
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

  let quickcheck_generator _observer__258_ _generator__259_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
      (fun ~size:_size__260_ ~random:_random__261_ () ->
         ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             _generator__259_
             ~size:_size__260_
             ~random:_random__261_
             ()
         , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
             (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
                _observer__258_
                _generator__259_)
             ~size:_size__260_
             ~random:_random__261_
             () ))
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer _generator__250_ _observer__251_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__253_ ~size:_size__256_ ~hash:_hash__257_ ->
         let _x__252_ = _f__253_ () in
         let _x__254_, _x__255_ = _x__252_ in
         let _hash__257_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             _observer__251_
             (fun () -> _x__254_)
             ~size:_size__256_
             ~hash:_hash__257_
         in
         let _hash__257_ =
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
             (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
                _generator__250_
                _observer__251_)
             (fun () -> _x__255_)
             ~size:_size__256_
             ~hash:_hash__257_
         in
         _hash__257_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker _shrinker__244_ _shrinker__245_ =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__246_ ->
      let _x__247_, _x__248_ = _f__246_ () in
      Ppx_quickcheck_runtime.Base.Sequence.round_robin
        [ Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               _shrinker__245_
               (fun () -> _x__247_))
            ~f:(fun _f__249_ () ->
              let _x__247_ = _f__249_ () in
              _x__247_, _x__248_)
        ; Ppx_quickcheck_runtime.Base.Sequence.map
            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
               Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
               (fun () -> _x__248_))
            ~f:(fun _f__249_ () ->
              let _x__248_ = _f__249_ () in
              _x__247_, _x__248_)
        ])
  ;;

  let _ = quickcheck_shrinker

  [@@@end]
end

module Instance_with_variance = struct
  type t = (bool, unit option) Poly_with_variance.t

  (* We cannot use [@@deriving_inline quickcheck ~generator ~observer ~shrinker] [@@@end] here because
     ppx_quickcheck cannot tell the [bool] argument needs to swap generators with
     observers. *)
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

  let quickcheck_generator _generator__269_ =
    quickcheck_generator_option quickcheck_generator_unit
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer _observer__267_ =
    quickcheck_observer_option quickcheck_observer_unit
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker _shrinker__265_ =
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
               (fun ~size:_size__276_ ~random:_random__277_ () -> `phantom) )
         ])
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Poly_with_phantom.quickcheck_observer
      (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
         (fun _f__273_ ~size:_size__274_ ~hash:_hash__275_ ->
            let _x__272_ = _f__273_ () in
            match _x__272_ with
            | `phantom ->
              let _hash__275_ =
                Ppx_quickcheck_runtime.Base.hash_fold_int _hash__275_ (-378691891)
              in
              _hash__275_))
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Poly_with_phantom.quickcheck_shrinker
      (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__270_ ->
         match _f__270_ () with
         | `phantom -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []))
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
           let _pair__292_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__296_ ~random:_random__297_ () -> Leaf) )
           and _pair__293_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__289_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__289_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__294_ ~random:_random__295_ () ->
                           Node
                             ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator
                                 ~size:_size__294_
                                 ~random:_random__295_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator
                                 ~size:_size__294_
                                 ~random:_random__295_
                                 () )))) )
           in
           let _gen__290_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__292_ ]
           and _gen__291_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__292_; _pair__293_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__290_
             | _ -> _gen__291_))
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
             (fun _f__284_ ~size:_size__285_ ~hash:_hash__286_ ->
                let _x__283_ = _f__284_ () in
                match _x__283_ with
                | Leaf ->
                  let _hash__286_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__286_ 0
                  in
                  _hash__286_
                | Node (_x__287_, _x__288_) ->
                  let _hash__286_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__286_ 1
                  in
                  let _hash__286_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__287_)
                      ~size:_size__285_
                      ~hash:_hash__286_
                  in
                  let _hash__286_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__288_)
                      ~size:_size__285_
                      ~hash:_hash__286_
                  in
                  _hash__286_))
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
             (fun _f__278_ ->
                match _f__278_ () with
                | Leaf -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                | Node (_x__280_, _x__281_) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__280_))
                        ~f:(fun _f__282_ () ->
                          let _x__280_ = _f__282_ () in
                          Node (_x__280_, _x__281_))
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__281_))
                        ~f:(fun _f__282_ () ->
                          let _x__281_ = _f__282_ () in
                          Node (_x__280_, _x__281_))
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
             (fun ~size:_size__306_ ~random:_random__307_ () ->
                { children =
                    Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                      (quickcheck_generator_list quickcheck_generator)
                      ~size:_size__306_
                      ~random:_random__307_
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
             (fun _f__302_ ~size:_size__304_ ~hash:_hash__305_ ->
                let _x__301_ = _f__302_ () in
                let { children = _x__303_ } = _x__301_ in
                let _hash__305_ =
                  Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                    (quickcheck_observer_list quickcheck_observer)
                    (fun () -> _x__303_)
                    ~size:_size__304_
                    ~hash:_hash__305_
                in
                _hash__305_))
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
             (fun _f__298_ ->
                let { children = _x__299_ } = _f__298_ () in
                Ppx_quickcheck_runtime.Base.Sequence.round_robin
                  [ Ppx_quickcheck_runtime.Base.Sequence.map
                      (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                         (quickcheck_shrinker_list quickcheck_shrinker)
                         (fun () -> _x__299_))
                      ~f:(fun _f__300_ () ->
                        let _x__299_ = _f__300_ () in
                        { children = _x__299_ })
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
           let _pair__335_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__342_ ~random:_random__343_ () ->
                    Constant
                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                         quickcheck_generator_int64
                         ~size:_size__342_
                         ~random:_random__343_
                         ())) )
           and _pair__336_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__332_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__332_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__338_ ~random:_random__339_ () ->
                           Operator
                             (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                              .generate
                                quickcheck_generator_op
                                ~size:_size__338_
                                ~random:_random__339_
                                ())))) )
           and _pair__337_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__332_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__332_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__340_ ~random:_random__341_ () ->
                           Application
                             ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator_expr
                                 ~size:_size__340_
                                 ~random:_random__341_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator_args
                                 ~size:_size__340_
                                 ~random:_random__341_
                                 () )))) )
           in
           let _gen__333_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__335_ ]
           and _gen__334_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__335_; _pair__336_; _pair__337_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__333_
             | _ -> _gen__334_))

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
                   (fun ~size:_size__344_ ~random:_random__345_ () -> `plus) )
             ; ( 1.
               , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                   (fun ~size:_size__346_ ~random:_random__347_ () -> `minus) )
             ; ( 1.
               , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                   (fun ~size:_size__348_ ~random:_random__349_ () -> `abs) )
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
             (fun _f__321_ ~size:_size__322_ ~hash:_hash__323_ ->
                let _x__320_ = _f__321_ () in
                match _x__320_ with
                | Constant _x__324_ ->
                  let _hash__323_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__323_ 0
                  in
                  let _hash__323_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_int64
                      (fun () -> _x__324_)
                      ~size:_size__322_
                      ~hash:_hash__323_
                  in
                  _hash__323_
                | Operator _x__325_ ->
                  let _hash__323_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__323_ 1
                  in
                  let _hash__323_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_op
                      (fun () -> _x__325_)
                      ~size:_size__322_
                      ~hash:_hash__323_
                  in
                  _hash__323_
                | Application (_x__326_, _x__327_) ->
                  let _hash__323_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__323_ 2
                  in
                  let _hash__323_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_expr
                      (fun () -> _x__326_)
                      ~size:_size__322_
                      ~hash:_hash__323_
                  in
                  let _hash__323_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_args
                      (fun () -> _x__327_)
                      ~size:_size__322_
                      ~hash:_hash__323_
                  in
                  _hash__323_))

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
             (fun _f__329_ ~size:_size__330_ ~hash:_hash__331_ ->
                let _x__328_ = _f__329_ () in
                match _x__328_ with
                | `plus ->
                  let _hash__331_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__331_ (-900055206)
                  in
                  _hash__331_
                | `minus ->
                  let _hash__331_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__331_ 141066352
                  in
                  _hash__331_
                | `abs ->
                  let _hash__331_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__331_ 4845682
                  in
                  _hash__331_))

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
             (fun _f__308_ ->
                match _f__308_ () with
                | Constant _x__309_ ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_int64
                           (fun () -> _x__309_))
                        ~f:(fun _f__310_ () ->
                          let _x__309_ = _f__310_ () in
                          Constant _x__309_)
                    ]
                | Operator _x__311_ ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_op
                           (fun () -> _x__311_))
                        ~f:(fun _f__312_ () ->
                          let _x__311_ = _f__312_ () in
                          Operator _x__311_)
                    ]
                | Application (_x__313_, _x__314_) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_expr
                           (fun () -> _x__313_))
                        ~f:(fun _f__315_ () ->
                          let _x__313_ = _f__315_ () in
                          Application (_x__313_, _x__314_))
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_args
                           (fun () -> _x__314_))
                        ~f:(fun _f__315_ () ->
                          let _x__314_ = _f__315_ () in
                          Application (_x__313_, _x__314_))
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
             (fun _f__316_ ->
                match _f__316_ () with
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
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
         -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _generator__367_ ->
            let quickcheck_generator _recur__377_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_generator
                     _recur__377_))
            in
            ignore quickcheck_generator;
            let _pair__371_ =
              ( 1.
              , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                  (fun ~size:_size__375_ ~random:_random__376_ () -> Zero) )
            and _pair__372_ =
              ( 1.
              , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                  ~f:(fun _size__368_ ->
                    Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                      ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__368_)
                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                         (fun ~size:_size__373_ ~random:_random__374_ () ->
                            Succ
                              ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                                .generate
                                  _generator__367_
                                  ~size:_size__373_
                                  ~random:_random__374_
                                  ()
                              , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                                .generate
                                  (quickcheck_generator _generator__367_)
                                  ~size:_size__373_
                                  ~random:_random__374_
                                  () )))) )
            in
            let _gen__369_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
                [ _pair__371_ ]
            and _gen__370_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
                [ _pair__371_; _pair__372_ ]
            in
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
              ~f:(function
              | 0 -> _gen__369_
              | _ -> _gen__370_))
      ;;

      let _ = quickcheck_generator
    end

    let quickcheck_generator _recur__378_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
        (lazy (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_generator _recur__378_))
    ;;

    let _ = quickcheck_generator
  end

  include struct
    open struct
      let rec quickcheck_observer
        : 'a.
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
         -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _observer__358_ ->
            let quickcheck_observer _recur__365_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_observer
                     _recur__365_))
            in
            ignore quickcheck_observer;
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
              (fun _f__360_ ~size:_size__361_ ~hash:_hash__362_ ->
                 let _x__359_ = _f__360_ () in
                 match _x__359_ with
                 | Zero ->
                   let _hash__362_ =
                     Ppx_quickcheck_runtime.Base.hash_fold_int _hash__362_ 0
                   in
                   _hash__362_
                 | Succ (_x__363_, _x__364_) ->
                   let _hash__362_ =
                     Ppx_quickcheck_runtime.Base.hash_fold_int _hash__362_ 1
                   in
                   let _hash__362_ =
                     Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                       _observer__358_
                       (fun () -> _x__363_)
                       ~size:_size__361_
                       ~hash:_hash__362_
                   in
                   let _hash__362_ =
                     Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                       (quickcheck_observer _observer__358_)
                       (fun () -> _x__364_)
                       ~size:_size__361_
                       ~hash:_hash__362_
                   in
                   _hash__362_))
      ;;

      let _ = quickcheck_observer
    end

    let quickcheck_observer _recur__366_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
        (lazy (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_observer _recur__366_))
    ;;

    let _ = quickcheck_observer
  end

  include struct
    open struct
      let rec quickcheck_shrinker
        : 'a.
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
         -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _shrinker__350_ ->
            let quickcheck_shrinker _recur__356_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_shrinker
                     _recur__356_))
            in
            ignore quickcheck_shrinker;
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
              (fun _f__351_ ->
                 match _f__351_ () with
                 | Zero -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                 | Succ (_x__353_, _x__354_) ->
                   Ppx_quickcheck_runtime.Base.Sequence.round_robin
                     [ Ppx_quickcheck_runtime.Base.Sequence.map
                         (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                            _shrinker__350_
                            (fun () -> _x__353_))
                         ~f:(fun _f__355_ () ->
                           let _x__353_ = _f__355_ () in
                           Succ (_x__353_, _x__354_))
                     ; Ppx_quickcheck_runtime.Base.Sequence.map
                         (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                            (quickcheck_shrinker _shrinker__350_)
                            (fun () -> _x__354_))
                         ~f:(fun _f__355_ () ->
                           let _x__354_ = _f__355_ () in
                           Succ (_x__353_, _x__354_))
                     ]))
      ;;

      let _ = quickcheck_shrinker
    end

    let quickcheck_shrinker _recur__357_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
        (lazy (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_shrinker _recur__357_))
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
           and quickcheck_generator_node _recur__406_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
               (lazy
                 (Ppx_quickcheck_runtime.Base.Lazy.force
                    quickcheck_generator_node
                    _recur__406_))
           in
           ignore quickcheck_generator_node;
           ignore quickcheck_generator;
           let _pair__399_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__403_ ~random:_random__404_ () ->
                    Leaf
                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                         quickcheck_generator_bool
                         ~size:_size__403_
                         ~random:_random__404_
                         ())) )
           and _pair__400_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__396_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__396_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__401_ ~random:_random__402_ () ->
                           Node
                             (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                              .generate
                                (quickcheck_generator_node quickcheck_generator)
                                ~size:_size__401_
                                ~random:_random__402_
                                ())))) )
           in
           let _gen__397_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__399_ ]
           and _gen__398_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__399_; _pair__400_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__397_
             | _ -> _gen__398_))

      and quickcheck_generator_node
        : 'a.
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
         -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Generator.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _generator__405_ ->
            let quickcheck_generator =
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
                quickcheck_generator
            and quickcheck_generator_node _recur__406_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_generator_node
                     _recur__406_))
            in
            ignore quickcheck_generator_node;
            ignore quickcheck_generator;
            quickcheck_generator_list _generator__405_)
      ;;

      let _ = quickcheck_generator
      and _ = quickcheck_generator_node
    end

    let quickcheck_generator =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy quickcheck_generator

    and quickcheck_generator_node _recur__407_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
        (lazy
          (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_generator_node _recur__407_))
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
           and quickcheck_observer_node _recur__394_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
               (lazy
                 (Ppx_quickcheck_runtime.Base.Lazy.force
                    quickcheck_observer_node
                    _recur__394_))
           in
           ignore quickcheck_observer_node;
           ignore quickcheck_observer;
           Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
             (fun _f__388_ ~size:_size__389_ ~hash:_hash__390_ ->
                let _x__387_ = _f__388_ () in
                match _x__387_ with
                | Leaf _x__391_ ->
                  let _hash__390_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__390_ 0
                  in
                  let _hash__390_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_bool
                      (fun () -> _x__391_)
                      ~size:_size__389_
                      ~hash:_hash__390_
                  in
                  _hash__390_
                | Node _x__392_ ->
                  let _hash__390_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__390_ 1
                  in
                  let _hash__390_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      (quickcheck_observer_node quickcheck_observer)
                      (fun () -> _x__392_)
                      ~size:_size__389_
                      ~hash:_hash__390_
                  in
                  _hash__390_))

      and quickcheck_observer_node
        : 'a.
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
         -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Observer.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _observer__393_ ->
            let quickcheck_observer =
              Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer
            and quickcheck_observer_node _recur__394_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_observer_node
                     _recur__394_))
            in
            ignore quickcheck_observer_node;
            ignore quickcheck_observer;
            quickcheck_observer_list _observer__393_)
      ;;

      let _ = quickcheck_observer
      and _ = quickcheck_observer_node
    end

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy quickcheck_observer

    and quickcheck_observer_node _recur__395_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
        (lazy
          (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_observer_node _recur__395_))
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
           and quickcheck_shrinker_node _recur__385_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
               (lazy
                 (Ppx_quickcheck_runtime.Base.Lazy.force
                    quickcheck_shrinker_node
                    _recur__385_))
           in
           ignore quickcheck_shrinker_node;
           ignore quickcheck_shrinker;
           Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
             (fun _f__379_ ->
                match _f__379_ () with
                | Leaf _x__380_ ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_bool
                           (fun () -> _x__380_))
                        ~f:(fun _f__381_ () ->
                          let _x__380_ = _f__381_ () in
                          Leaf _x__380_)
                    ]
                | Node _x__382_ ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           (quickcheck_shrinker_node quickcheck_shrinker)
                           (fun () -> _x__382_))
                        ~f:(fun _f__383_ () ->
                          let _x__382_ = _f__383_ () in
                          Node _x__382_)
                    ]))

      and quickcheck_shrinker_node
        : 'a.
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
         -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _shrinker__384_ ->
            let quickcheck_shrinker =
              Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker
            and quickcheck_shrinker_node _recur__385_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_shrinker_node
                     _recur__385_))
            in
            ignore quickcheck_shrinker_node;
            ignore quickcheck_shrinker;
            quickcheck_shrinker_list _shrinker__384_)
      ;;

      let _ = quickcheck_shrinker
      and _ = quickcheck_shrinker_node
    end

    let quickcheck_shrinker =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy quickcheck_shrinker

    and quickcheck_shrinker_node _recur__386_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
        (lazy
          (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_shrinker_node _recur__386_))
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
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
         -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _generator__435_ ->
            let quickcheck_generator _recur__447_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_generator
                     _recur__447_))
            in
            ignore quickcheck_generator;
            let _pair__439_ =
              ( 1.
              , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                  (fun ~size:_size__445_ ~random:_random__446_ () ->
                     Single
                       (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                        .generate
                          _generator__435_
                          ~size:_size__445_
                          ~random:_random__446_
                          ())) )
            and _pair__440_ =
              ( 1.
              , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                  ~f:(fun _size__436_ ->
                    Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                      ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__436_)
                      (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                         (fun ~size:_size__443_ ~random:_random__444_ () ->
                            Double
                              (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 (quickcheck_generator
                                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                     .Via_thunk
                                     .create
                                       (fun ~size:_size__441_ ~random:_random__442_ () ->
                                          ( Ppx_quickcheck_runtime.Base_quickcheck
                                            .Generator
                                            .Via_thunk
                                            .generate
                                              _generator__435_
                                              ~size:_size__441_
                                              ~random:_random__442_
                                              ()
                                          , Ppx_quickcheck_runtime.Base_quickcheck
                                            .Generator
                                            .Via_thunk
                                            .generate
                                              _generator__435_
                                              ~size:_size__441_
                                              ~random:_random__442_
                                              () ))))
                                 ~size:_size__443_
                                 ~random:_random__444_
                                 ())))) )
            in
            let _gen__437_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
                [ _pair__439_ ]
            and _gen__438_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
                [ _pair__439_; _pair__440_ ]
            in
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
              Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
              ~f:(function
              | 0 -> _gen__437_
              | _ -> _gen__438_))
      ;;

      let _ = quickcheck_generator
    end

    let quickcheck_generator _recur__448_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
        (lazy (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_generator _recur__448_))
    ;;

    let _ = quickcheck_generator
  end

  include struct
    open struct
      let rec quickcheck_observer
        : 'a.
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
         -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _observer__420_ ->
            let quickcheck_observer _recur__433_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_observer
                     _recur__433_))
            in
            ignore quickcheck_observer;
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
              (fun _f__422_ ~size:_size__423_ ~hash:_hash__424_ ->
                 let _x__421_ = _f__422_ () in
                 match _x__421_ with
                 | Single _x__425_ ->
                   let _hash__424_ =
                     Ppx_quickcheck_runtime.Base.hash_fold_int _hash__424_ 0
                   in
                   let _hash__424_ =
                     Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                       _observer__420_
                       (fun () -> _x__425_)
                       ~size:_size__423_
                       ~hash:_hash__424_
                   in
                   _hash__424_
                 | Double _x__432_ ->
                   let _hash__424_ =
                     Ppx_quickcheck_runtime.Base.hash_fold_int _hash__424_ 1
                   in
                   let _hash__424_ =
                     Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                       (quickcheck_observer
                          (Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk
                           .create
                             (fun _f__427_ ~size:_size__430_ ~hash:_hash__431_ ->
                                let _x__426_ = _f__427_ () in
                                let _x__428_, _x__429_ = _x__426_ in
                                let _hash__431_ =
                                  Ppx_quickcheck_runtime.Base_quickcheck.Observer
                                  .Via_thunk
                                  .observe
                                    _observer__420_
                                    (fun () -> _x__428_)
                                    ~size:_size__430_
                                    ~hash:_hash__431_
                                in
                                let _hash__431_ =
                                  Ppx_quickcheck_runtime.Base_quickcheck.Observer
                                  .Via_thunk
                                  .observe
                                    _observer__420_
                                    (fun () -> _x__429_)
                                    ~size:_size__430_
                                    ~hash:_hash__431_
                                in
                                _hash__431_)))
                       (fun () -> _x__432_)
                       ~size:_size__423_
                       ~hash:_hash__424_
                   in
                   _hash__424_))
      ;;

      let _ = quickcheck_observer
    end

    let quickcheck_observer _recur__434_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
        (lazy (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_observer _recur__434_))
    ;;

    let _ = quickcheck_observer
  end

  include struct
    open struct
      let rec quickcheck_shrinker
        : 'a.
        ('a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
         -> 'a t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t)
          Ppx_quickcheck_runtime.Base.Lazy.t
        =
        lazy
          (fun _shrinker__408_ ->
            let quickcheck_shrinker _recur__418_ =
              Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
                (lazy
                  (Ppx_quickcheck_runtime.Base.Lazy.force
                     quickcheck_shrinker
                     _recur__418_))
            in
            ignore quickcheck_shrinker;
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
              (fun _f__409_ ->
                 match _f__409_ () with
                 | Single _x__410_ ->
                   Ppx_quickcheck_runtime.Base.Sequence.round_robin
                     [ Ppx_quickcheck_runtime.Base.Sequence.map
                         (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                            _shrinker__408_
                            (fun () -> _x__410_))
                         ~f:(fun _f__411_ () ->
                           let _x__410_ = _f__411_ () in
                           Single _x__410_)
                     ]
                 | Double _x__412_ ->
                   Ppx_quickcheck_runtime.Base.Sequence.round_robin
                     [ Ppx_quickcheck_runtime.Base.Sequence.map
                         (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                            (quickcheck_shrinker
                               (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                                .create
                                  (fun _f__413_ ->
                                     let _x__414_, _x__415_ = _f__413_ () in
                                     Ppx_quickcheck_runtime.Base.Sequence.round_robin
                                       [ Ppx_quickcheck_runtime.Base.Sequence.map
                                           (Ppx_quickcheck_runtime.Base_quickcheck
                                            .Shrinker
                                            .Via_thunk
                                            .shrink
                                              _shrinker__408_
                                              (fun () -> _x__414_))
                                           ~f:(fun _f__416_ () ->
                                             let _x__414_ = _f__416_ () in
                                             _x__414_, _x__415_)
                                       ; Ppx_quickcheck_runtime.Base.Sequence.map
                                           (Ppx_quickcheck_runtime.Base_quickcheck
                                            .Shrinker
                                            .Via_thunk
                                            .shrink
                                              _shrinker__408_
                                              (fun () -> _x__415_))
                                           ~f:(fun _f__416_ () ->
                                             let _x__415_ = _f__416_ () in
                                             _x__414_, _x__415_)
                                       ])))
                            (fun () -> _x__412_))
                         ~f:(fun _f__417_ () ->
                           let _x__412_ = _f__417_ () in
                           Double _x__412_)
                     ]))
      ;;

      let _ = quickcheck_shrinker
    end

    let quickcheck_shrinker _recur__419_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
        (lazy (Ppx_quickcheck_runtime.Base.Lazy.force quickcheck_shrinker _recur__419_))
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
            (fun ~size:_size__500_ ~random:_random__501_ () -> Null) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__502_ ~random:_random__503_ () ->
               Text
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    (Generator.string_of Generator.char_lowercase)
                    ~size:_size__502_
                    ~random:_random__503_
                    ())) )
      ; ( 1.
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
            (fun ~size:_size__504_ ~random:_random__505_ () ->
               Number
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    Generator.float_strictly_positive
                    ~size:_size__504_
                    ~random:_random__505_
                    ())) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__495_ ~size:_size__496_ ~hash:_hash__497_ ->
         let _x__494_ = _f__495_ () in
         match _x__494_ with
         | Null ->
           let _hash__497_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__497_ 0 in
           _hash__497_
         | Text _x__498_ ->
           let _hash__497_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__497_ 1 in
           let _hash__497_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               Observer.string
               (fun () -> _x__498_)
               ~size:_size__496_
               ~hash:_hash__497_
           in
           _hash__497_
         | Number _x__499_ ->
           let _hash__497_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__497_ 2 in
           let _hash__497_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_float
               (fun () -> _x__499_)
               ~size:_size__496_
               ~hash:_hash__497_
           in
           _hash__497_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__488_ ->
      match _f__488_ () with
      | Null -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
      | Text _x__490_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 Shrinker.string
                 (fun () -> _x__490_))
              ~f:(fun _f__491_ () ->
                let _x__490_ = _f__491_ () in
                Text _x__490_)
          ]
      | Number _x__492_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_float
                 (fun () -> _x__492_))
              ~f:(fun _f__493_ () ->
                let _x__492_ = _f__493_ () in
                Number _x__492_)
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
           let _pair__533_ =
             ( 1.
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                 (fun ~size:_size__540_ ~random:_random__541_ () -> Leaf) )
           and _pair__534_ =
             ( 0.5
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__530_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__530_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__536_ ~random:_random__537_ () ->
                           Node1
                             ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator
                                 ~size:_size__536_
                                 ~random:_random__537_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator_int64
                                 ~size:_size__536_
                                 ~random:_random__537_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator
                                 ~size:_size__536_
                                 ~random:_random__537_
                                 () )))) )
           and _pair__535_ =
             ( 0.25
             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(fun _size__530_ ->
                   Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                     ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__530_)
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                        (fun ~size:_size__538_ ~random:_random__539_ () ->
                           Node2
                             ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator
                                 ~size:_size__538_
                                 ~random:_random__539_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator_int64
                                 ~size:_size__538_
                                 ~random:_random__539_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator
                                 ~size:_size__538_
                                 ~random:_random__539_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator_int64
                                 ~size:_size__538_
                                 ~random:_random__539_
                                 ()
                             , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                               .generate
                                 quickcheck_generator
                                 ~size:_size__538_
                                 ~random:_random__539_
                                 () )))) )
           in
           let _gen__531_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__533_ ]
           and _gen__532_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
               [ _pair__533_; _pair__534_; _pair__535_ ]
           in
           Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
             Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
             ~f:(function
             | 0 -> _gen__531_
             | _ -> _gen__532_))
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
             (fun _f__519_ ~size:_size__520_ ~hash:_hash__521_ ->
                let _x__518_ = _f__519_ () in
                match _x__518_ with
                | Leaf ->
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__521_ 0
                  in
                  _hash__521_
                | Node1 (_x__522_, _x__523_, _x__524_) ->
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__521_ 1
                  in
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__522_)
                      ~size:_size__520_
                      ~hash:_hash__521_
                  in
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_int64
                      (fun () -> _x__523_)
                      ~size:_size__520_
                      ~hash:_hash__521_
                  in
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__524_)
                      ~size:_size__520_
                      ~hash:_hash__521_
                  in
                  _hash__521_
                | Node2 (_x__525_, _x__526_, _x__527_, _x__528_, _x__529_) ->
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base.hash_fold_int _hash__521_ 2
                  in
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__525_)
                      ~size:_size__520_
                      ~hash:_hash__521_
                  in
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_int64
                      (fun () -> _x__526_)
                      ~size:_size__520_
                      ~hash:_hash__521_
                  in
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__527_)
                      ~size:_size__520_
                      ~hash:_hash__521_
                  in
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer_int64
                      (fun () -> _x__528_)
                      ~size:_size__520_
                      ~hash:_hash__521_
                  in
                  let _hash__521_ =
                    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                      quickcheck_observer
                      (fun () -> _x__529_)
                      ~size:_size__520_
                      ~hash:_hash__521_
                  in
                  _hash__521_))
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
             (fun _f__506_ ->
                match _f__506_ () with
                | Leaf -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                | Node1 (_x__508_, _x__509_, _x__510_) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__508_))
                        ~f:(fun _f__511_ () ->
                          let _x__508_ = _f__511_ () in
                          Node1 (_x__508_, _x__509_, _x__510_))
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_int64
                           (fun () -> _x__509_))
                        ~f:(fun _f__511_ () ->
                          let _x__509_ = _f__511_ () in
                          Node1 (_x__508_, _x__509_, _x__510_))
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__510_))
                        ~f:(fun _f__511_ () ->
                          let _x__510_ = _f__511_ () in
                          Node1 (_x__508_, _x__509_, _x__510_))
                    ]
                | Node2 (_x__512_, _x__513_, _x__514_, _x__515_, _x__516_) ->
                  Ppx_quickcheck_runtime.Base.Sequence.round_robin
                    [ Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__512_))
                        ~f:(fun _f__517_ () ->
                          let _x__512_ = _f__517_ () in
                          Node2 (_x__512_, _x__513_, _x__514_, _x__515_, _x__516_))
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_int64
                           (fun () -> _x__513_))
                        ~f:(fun _f__517_ () ->
                          let _x__513_ = _f__517_ () in
                          Node2 (_x__512_, _x__513_, _x__514_, _x__515_, _x__516_))
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__514_))
                        ~f:(fun _f__517_ () ->
                          let _x__514_ = _f__517_ () in
                          Node2 (_x__512_, _x__513_, _x__514_, _x__515_, _x__516_))
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker_int64
                           (fun () -> _x__515_))
                        ~f:(fun _f__517_ () ->
                          let _x__515_ = _f__517_ () in
                          Node2 (_x__512_, _x__513_, _x__514_, _x__515_, _x__516_))
                    ; Ppx_quickcheck_runtime.Base.Sequence.map
                        (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                           quickcheck_shrinker
                           (fun () -> _x__516_))
                        ~f:(fun _f__517_ () ->
                          let _x__516_ = _f__517_ () in
                          Node2 (_x__512_, _x__513_, _x__514_, _x__515_, _x__516_))
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
  let quickcheck_generator_transparent _generator__547_ = quickcheck_generator_string
  let _ = quickcheck_generator_transparent
  let quickcheck_observer_transparent _observer__545_ = quickcheck_observer_string
  let _ = quickcheck_observer_transparent
  let quickcheck_shrinker_transparent _shrinker__543_ = quickcheck_shrinker_string
  let _ = quickcheck_shrinker_transparent

  [@@@end]

  type 'a opaque = 'a option [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  let _ = fun (_ : 'a opaque) -> ()

  let quickcheck_generator_opaque _generator__550_ =
    quickcheck_generator_option _generator__550_
  ;;

  let _ = quickcheck_generator_opaque

  let quickcheck_observer_opaque _observer__549_ =
    quickcheck_observer_option _observer__549_
  ;;

  let _ = quickcheck_observer_opaque

  let quickcheck_shrinker_opaque _shrinker__548_ =
    quickcheck_shrinker_option _shrinker__548_
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
            (fun ~size:_size__562_ ~random:_random__563_ () ->
               Can_generate
                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                    quickcheck_generator_bool
                    ~size:_size__562_
                    ~random:_random__563_
                    ())) )
      ]
  ;;

  let _ = quickcheck_generator

  let quickcheck_observer =
    Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
      (fun _f__557_ ~size:_size__558_ ~hash:_hash__559_ ->
         let _x__556_ = _f__557_ () in
         match _x__556_ with
         | Can_generate _x__560_ ->
           let _hash__559_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__559_ 0 in
           let _hash__559_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               quickcheck_observer_bool
               (fun () -> _x__560_)
               ~size:_size__558_
               ~hash:_hash__559_
           in
           _hash__559_
         | Cannot_generate _x__561_ ->
           let _hash__559_ = Ppx_quickcheck_runtime.Base.hash_fold_int _hash__559_ 1 in
           let _hash__559_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               Cannot_generate.quickcheck_observer
               (fun () -> _x__561_)
               ~size:_size__558_
               ~hash:_hash__559_
           in
           _hash__559_)
  ;;

  let _ = quickcheck_observer

  let quickcheck_shrinker =
    Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__551_ ->
      match _f__551_ () with
      | Can_generate _x__552_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 quickcheck_shrinker_bool
                 (fun () -> _x__552_))
              ~f:(fun _f__553_ () ->
                let _x__552_ = _f__553_ () in
                Can_generate _x__552_)
          ]
      | Cannot_generate _x__554_ ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 Cannot_generate.quickcheck_shrinker
                 (fun () -> _x__554_))
              ~f:(fun _f__555_ () ->
                let _x__554_ = _f__555_ () in
                Cannot_generate _x__554_)
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
              (fun ~size:_size__575_ ~random:_random__576_ () ->
                 `Can_generate
                   (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                      quickcheck_generator_bool
                      ~size:_size__575_
                      ~random:_random__576_
                      ())) )
        ]
    ;;

    let _ = quickcheck_generator

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
        (fun _f__570_ ~size:_size__571_ ~hash:_hash__572_ ->
           let _x__569_ = _f__570_ () in
           match _x__569_ with
           | `Can_generate _x__573_ ->
             let _hash__572_ =
               Ppx_quickcheck_runtime.Base.hash_fold_int _hash__572_ (-895996764)
             in
             let _hash__572_ =
               Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                 quickcheck_observer_bool
                 (fun () -> _x__573_)
                 ~size:_size__571_
                 ~hash:_hash__572_
             in
             _hash__572_
           | `Cannot_generate _x__574_ ->
             let _hash__572_ =
               Ppx_quickcheck_runtime.Base.hash_fold_int _hash__572_ 501585681
             in
             let _hash__572_ =
               Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                 Cannot_generate.quickcheck_observer
                 (fun () -> _x__574_)
                 ~size:_size__571_
                 ~hash:_hash__572_
             in
             _hash__572_)
    ;;

    let _ = quickcheck_observer

    let quickcheck_shrinker =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__564_ ->
        match _f__564_ () with
        | `Can_generate _x__565_ ->
          Ppx_quickcheck_runtime.Base.Sequence.round_robin
            [ Ppx_quickcheck_runtime.Base.Sequence.map
                (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                   quickcheck_shrinker_bool
                   (fun () -> _x__565_))
                ~f:(fun _f__566_ () ->
                  let _x__565_ = _f__566_ () in
                  `Can_generate _x__565_)
            ]
        | `Cannot_generate _x__567_ ->
          Ppx_quickcheck_runtime.Base.Sequence.round_robin
            [ Ppx_quickcheck_runtime.Base.Sequence.map
                (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                   Cannot_generate.quickcheck_shrinker
                   (fun () -> _x__567_))
                ~f:(fun _f__568_ () ->
                  let _x__567_ = _f__568_ () in
                  `Cannot_generate _x__567_)
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
                     (fun ~size:_size__586_ ~random:_random__587_ () -> A) )
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
               (fun _f__582_ ~size:_size__583_ ~hash:_hash__584_ ->
                  let _x__581_ = _f__582_ () in
                  match _x__581_ with
                  | A ->
                    let _hash__584_ =
                      Ppx_quickcheck_runtime.Base.hash_fold_int _hash__584_ 0
                    in
                    _hash__584_
                  | B _x__585_ ->
                    let _hash__584_ =
                      Ppx_quickcheck_runtime.Base.hash_fold_int _hash__584_ 1
                    in
                    let _hash__584_ =
                      Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                        quickcheck_observer
                        (fun () -> _x__585_)
                        ~size:_size__583_
                        ~hash:_hash__584_
                    in
                    _hash__584_))
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
               (fun _f__577_ ->
                  match _f__577_ () with
                  | A -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                  | B _x__579_ ->
                    Ppx_quickcheck_runtime.Base.Sequence.round_robin
                      [ Ppx_quickcheck_runtime.Base.Sequence.map
                          (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                           .shrink
                             quickcheck_shrinker
                             (fun () -> _x__579_))
                          ~f:(fun _f__580_ () ->
                            let _x__579_ = _f__580_ () in
                            B _x__579_)
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
