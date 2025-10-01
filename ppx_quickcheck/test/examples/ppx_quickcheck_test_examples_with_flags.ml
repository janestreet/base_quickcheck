open Base
open Base_quickcheck

(* ensure that shadowing doesn't break anything *)
include struct
  module Base = struct end
  module Base_quickcheck = struct end
  module Quickcheckable = struct end
end

module Generator = struct
  module Simple_reference = struct
    type t = bool [@@deriving_inline quickcheck ~generator]

    let _ = fun (_ : t) -> ()
    let quickcheck_generator = quickcheck_generator_bool
    let _ = quickcheck_generator

    [@@@end]
  end

  module Simple_arrow = struct
    type t = unit option -> bool [@@deriving_inline quickcheck ~generator]

    let _ = fun (_ : t) -> ()

    let quickcheck_generator =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
        (quickcheck_observer_option quickcheck_observer_unit)
        quickcheck_generator_bool
    ;;

    let _ = quickcheck_generator

    [@@@end]
  end

  module Simple_higher_order = struct
    type t = (unit option -> bool option) -> bool
    [@@deriving_inline quickcheck ~generator]

    let _ = fun (_ : t) -> ()

    let quickcheck_generator =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
        (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
           (quickcheck_generator_option quickcheck_generator_unit)
           (quickcheck_observer_option quickcheck_observer_bool))
        quickcheck_generator_bool
    ;;

    let _ = quickcheck_generator

    [@@@end]
  end

  module Poly_with_variance = struct
    type (-'a, +'b) t = 'b * ('a -> 'b) [@@deriving_inline quickcheck ~generator]

    let _ = fun (_ : ('a, 'b) t) -> ()

    let quickcheck_generator _observer__001_ _generator__002_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
        (fun ~size:_size__003_ ~random:_random__004_ () ->
           ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
               _generator__002_
               ~size:_size__003_
               ~random:_random__004_
               ()
           , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
               (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
                  _observer__001_
                  _generator__002_)
               ~size:_size__003_
               ~random:_random__004_
               () ))
    ;;

    let _ = quickcheck_generator

    [@@@end]
  end
end

module Observer = struct
  module Simple_reference = struct
    type t = bool [@@deriving_inline quickcheck ~observer]

    let _ = fun (_ : t) -> ()
    let quickcheck_observer = quickcheck_observer_bool
    let _ = quickcheck_observer

    [@@@end]
  end

  module Simple_arrow = struct
    type t = unit option -> bool [@@deriving_inline quickcheck ~observer]

    let _ = fun (_ : t) -> ()

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
        (quickcheck_generator_option quickcheck_generator_unit)
        quickcheck_observer_bool
    ;;

    let _ = quickcheck_observer

    [@@@end]
  end

  module Simple_higher_order = struct
    type t = (unit option -> bool option) -> bool [@@deriving_inline quickcheck ~observer]

    let _ = fun (_ : t) -> ()

    let quickcheck_observer =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
        (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
           (quickcheck_observer_option quickcheck_observer_unit)
           (quickcheck_generator_option quickcheck_generator_bool))
        quickcheck_observer_bool
    ;;

    let _ = quickcheck_observer

    [@@@end]
  end

  module Poly_with_variance = struct
    type (-'a, +'b) t = 'b * ('a -> 'b) [@@deriving_inline quickcheck ~observer]

    let _ = fun (_ : ('a, 'b) t) -> ()

    let quickcheck_observer _generator__005_ _observer__006_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
        (fun _f__008_ ~size:_size__011_ ~hash:_hash__012_ ->
           let _x__007_ = _f__008_ () in
           let _x__009_, _x__010_ = _x__007_ in
           let _hash__012_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               _observer__006_
               (fun () -> _x__009_)
               ~size:_size__011_
               ~hash:_hash__012_
           in
           let _hash__012_ =
             Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
               (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
                  _generator__005_
                  _observer__006_)
               (fun () -> _x__010_)
               ~size:_size__011_
               ~hash:_hash__012_
           in
           _hash__012_)
    ;;

    let _ = quickcheck_observer

    [@@@end]
  end
end

module Shrinker = struct
  module Simple_reference = struct
    type t = bool [@@deriving_inline quickcheck ~shrinker]

    let _ = fun (_ : t) -> ()
    let quickcheck_shrinker = quickcheck_shrinker_bool
    let _ = quickcheck_shrinker

    [@@@end]
  end

  module Simple_arrow = struct
    type t = unit option -> bool [@@deriving_inline quickcheck ~shrinker]

    let _ = fun (_ : t) -> ()
    let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
    let _ = quickcheck_shrinker

    [@@@end]
  end

  module Simple_higher_order = struct
    type t = (unit option -> bool option) -> bool [@@deriving_inline quickcheck ~shrinker]

    let _ = fun (_ : t) -> ()
    let quickcheck_shrinker = Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
    let _ = quickcheck_shrinker

    [@@@end]
  end

  module Poly_with_variance = struct
    type (-'a, +'b) t = 'b * ('a -> 'b) [@@deriving_inline quickcheck ~shrinker]

    let _ = fun (_ : ('a, 'b) t) -> ()

    let quickcheck_shrinker _shrinker__013_ _shrinker__014_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create (fun _f__015_ ->
        let _x__016_, _x__017_ = _f__015_ () in
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 _shrinker__014_
                 (fun () -> _x__016_))
              ~f:(fun _f__018_ () ->
                let _x__016_ = _f__018_ () in
                _x__016_, _x__017_)
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
                 (fun () -> _x__017_))
              ~f:(fun _f__018_ () ->
                let _x__017_ = _f__018_ () in
                _x__016_, _x__017_)
          ])
    ;;

    let _ = quickcheck_shrinker

    [@@@end]
  end

  module Portable_recursive = struct
    type t =
      | A
      | B of u

    and u =
      | C
      | D of t
    [@@deriving_inline quickcheck ~portable]

    let _ = fun (_ : t) -> ()
    let _ = fun (_ : u) -> ()

    include struct
      open struct
        [%%portable
        let rec quickcheck_generator
          : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_generator =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   quickcheck_generator
               and quickcheck_generator_u =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   quickcheck_generator_u
               in
               ignore quickcheck_generator_u;
               ignore quickcheck_generator;
               let _pair__040_ =
                 ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                   .create__portable
                     (fun ~size:_size__044_ ~random:_random__045_ () -> A) )
               and _pair__041_ =
                 ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                     ~f:(fun _size__037_ ->
                       Ppx_quickcheck_runtime.Base_quickcheck.Generator
                       .with_size__portable
                         ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__037_)
                         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                          .create__portable
                            (fun ~size:_size__042_ ~random:_random__043_ () ->
                               B
                                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator_u
                                    ~size:_size__042_
                                    ~random:_random__043_
                                    ())))) )
               in
               let _gen__038_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
                   [ _pair__040_ ]
               and _gen__039_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
                   [ _pair__040_; _pair__041_ ]
               in
               Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(function
                 | 0 -> _gen__038_
                 | _ -> _gen__039_))]

        and quickcheck_generator_u
          : u Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_generator =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   quickcheck_generator
               and quickcheck_generator_u =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   quickcheck_generator_u
               in
               ignore quickcheck_generator_u;
               ignore quickcheck_generator;
               let _pair__049_ =
                 ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                   .create__portable
                     (fun ~size:_size__053_ ~random:_random__054_ () -> C) )
               and _pair__050_ =
                 ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                     ~f:(fun _size__046_ ->
                       Ppx_quickcheck_runtime.Base_quickcheck.Generator
                       .with_size__portable
                         ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__046_)
                         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                          .create__portable
                            (fun ~size:_size__051_ ~random:_random__052_ () ->
                               D
                                 (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                  .Via_thunk
                                  .generate
                                    quickcheck_generator
                                    ~size:_size__051_
                                    ~random:_random__052_
                                    ())))) )
               in
               let _gen__047_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
                   [ _pair__049_ ]
               and _gen__048_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
                   [ _pair__049_; _pair__050_ ]
               in
               Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(function
                 | 0 -> _gen__047_
                 | _ -> _gen__048_))]
        ;;

        let _ = quickcheck_generator
        and _ = quickcheck_generator_u]
      end

      let quickcheck_generator =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
          quickcheck_generator

      and quickcheck_generator_u =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
          quickcheck_generator_u
      ;;

      let _ = quickcheck_generator
      and _ = quickcheck_generator_u
    end

    include struct
      open struct
        [%%portable
        let rec quickcheck_observer
          : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_observer =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   quickcheck_observer
               and quickcheck_observer_u =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   quickcheck_observer_u
               in
               ignore quickcheck_observer_u;
               ignore quickcheck_observer;
               Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create__portable
                 (fun _f__028_ ~size:_size__029_ ~hash:_hash__030_ ->
                    let _x__027_ = _f__028_ () in
                    match _x__027_ with
                    | A ->
                      let _hash__030_ =
                        Ppx_quickcheck_runtime.Base.hash_fold_int _hash__030_ 0
                      in
                      _hash__030_
                    | B _x__031_ ->
                      let _hash__030_ =
                        Ppx_quickcheck_runtime.Base.hash_fold_int _hash__030_ 1
                      in
                      let _hash__030_ =
                        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                          quickcheck_observer_u
                          (fun () -> _x__031_)
                          ~size:_size__029_
                          ~hash:_hash__030_
                      in
                      _hash__030_))]

        and quickcheck_observer_u
          : u Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_observer =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   quickcheck_observer
               and quickcheck_observer_u =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   quickcheck_observer_u
               in
               ignore quickcheck_observer_u;
               ignore quickcheck_observer;
               Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create__portable
                 (fun _f__033_ ~size:_size__034_ ~hash:_hash__035_ ->
                    let _x__032_ = _f__033_ () in
                    match _x__032_ with
                    | C ->
                      let _hash__035_ =
                        Ppx_quickcheck_runtime.Base.hash_fold_int _hash__035_ 0
                      in
                      _hash__035_
                    | D _x__036_ ->
                      let _hash__035_ =
                        Ppx_quickcheck_runtime.Base.hash_fold_int _hash__035_ 1
                      in
                      let _hash__035_ =
                        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                          quickcheck_observer
                          (fun () -> _x__036_)
                          ~size:_size__034_
                          ~hash:_hash__035_
                      in
                      _hash__035_))]
        ;;

        let _ = quickcheck_observer
        and _ = quickcheck_observer_u]
      end

      let quickcheck_observer =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
          quickcheck_observer

      and quickcheck_observer_u =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
          quickcheck_observer_u
      ;;

      let _ = quickcheck_observer
      and _ = quickcheck_observer_u
    end

    include struct
      open struct
        [%%portable
        let rec quickcheck_shrinker
          : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_shrinker =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   quickcheck_shrinker
               and quickcheck_shrinker_u =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   quickcheck_shrinker_u
               in
               ignore quickcheck_shrinker_u;
               ignore quickcheck_shrinker;
               Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create__portable
                 (fun _f__019_ ->
                    match _f__019_ () with
                    | A -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                    | B _x__021_ ->
                      Ppx_quickcheck_runtime.Base.Sequence.round_robin
                        [ Ppx_quickcheck_runtime.Base.Sequence.map
                            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                             .shrink
                               quickcheck_shrinker_u
                               (fun () -> _x__021_))
                            ~f:(fun _f__022_ () ->
                              let _x__021_ = _f__022_ () in
                              B _x__021_)
                        ]))]

        and quickcheck_shrinker_u
          : u Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_shrinker =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   quickcheck_shrinker
               and quickcheck_shrinker_u =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   quickcheck_shrinker_u
               in
               ignore quickcheck_shrinker_u;
               ignore quickcheck_shrinker;
               Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create__portable
                 (fun _f__023_ ->
                    match _f__023_ () with
                    | C -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                    | D _x__025_ ->
                      Ppx_quickcheck_runtime.Base.Sequence.round_robin
                        [ Ppx_quickcheck_runtime.Base.Sequence.map
                            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                             .shrink
                               quickcheck_shrinker
                               (fun () -> _x__025_))
                            ~f:(fun _f__026_ () ->
                              let _x__025_ = _f__026_ () in
                              D _x__025_)
                        ]))]
        ;;

        let _ = quickcheck_shrinker
        and _ = quickcheck_shrinker_u]
      end

      let quickcheck_shrinker =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
          quickcheck_shrinker

      and quickcheck_shrinker_u =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
          quickcheck_shrinker_u
      ;;

      let _ = quickcheck_shrinker
      and _ = quickcheck_shrinker_u
    end

    [@@@end]
  end
end
