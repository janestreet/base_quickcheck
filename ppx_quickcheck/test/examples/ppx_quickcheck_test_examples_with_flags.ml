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
               let _pair__042_ =
                 ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                   .create__portable
                     (fun ~size:_size__047_ ~random:_random__048_ () ->
                        let _x__049_ = (A : t) in
                        _x__049_) )
               and _pair__043_ =
                 ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                     ~f:(fun _size__039_ ->
                       Ppx_quickcheck_runtime.Base_quickcheck.Generator
                       .with_size__portable
                         ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__039_)
                         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                          .create__portable
                            (fun ~size:_size__044_ ~random:_random__045_ () ->
                               let _x__046_ =
                                 (B
                                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                     .Via_thunk
                                     .generate
                                       quickcheck_generator_u
                                       ~size:_size__044_
                                       ~random:_random__045_
                                       ())
                                  : t)
                               in
                               _x__046_))) )
               in
               let _gen__040_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
                   [ _pair__042_ ]
               and _gen__041_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
                   [ _pair__042_; _pair__043_ ]
               in
               Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(function
                 | 0 -> _gen__040_
                 | _ -> _gen__041_))]

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
               let _pair__053_ =
                 ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                   .create__portable
                     (fun ~size:_size__058_ ~random:_random__059_ () ->
                        let _x__060_ = (C : u) in
                        _x__060_) )
               and _pair__054_ =
                 ( 1.
                 , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                     Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                     ~f:(fun _size__050_ ->
                       Ppx_quickcheck_runtime.Base_quickcheck.Generator
                       .with_size__portable
                         ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__050_)
                         (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                          .create__portable
                            (fun ~size:_size__055_ ~random:_random__056_ () ->
                               let _x__057_ =
                                 (D
                                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator
                                     .Via_thunk
                                     .generate
                                       quickcheck_generator
                                       ~size:_size__055_
                                       ~random:_random__056_
                                       ())
                                  : u)
                               in
                               _x__057_))) )
               in
               let _gen__051_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
                   [ _pair__053_ ]
               and _gen__052_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
                   [ _pair__053_; _pair__054_ ]
               in
               Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                 ~f:(function
                 | 0 -> _gen__051_
                 | _ -> _gen__052_))]
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
                 (fun _f__030_ ~size:_size__031_ ~hash:_hash__032_ ->
                    let _x__029_ = _f__030_ () in
                    match _x__029_ with
                    | (A : t) ->
                      let _hash__032_ =
                        Ppx_quickcheck_runtime.Base.hash_fold_int _hash__032_ 0
                      in
                      _hash__032_
                    | (B _x__033_ : t) ->
                      let _hash__032_ =
                        Ppx_quickcheck_runtime.Base.hash_fold_int _hash__032_ 1
                      in
                      let _hash__032_ =
                        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                          quickcheck_observer_u
                          (fun () -> _x__033_)
                          ~size:_size__031_
                          ~hash:_hash__032_
                      in
                      _hash__032_))]

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
                 (fun _f__035_ ~size:_size__036_ ~hash:_hash__037_ ->
                    let _x__034_ = _f__035_ () in
                    match _x__034_ with
                    | (C : u) ->
                      let _hash__037_ =
                        Ppx_quickcheck_runtime.Base.hash_fold_int _hash__037_ 0
                      in
                      _hash__037_
                    | (D _x__038_ : u) ->
                      let _hash__037_ =
                        Ppx_quickcheck_runtime.Base.hash_fold_int _hash__037_ 1
                      in
                      let _hash__037_ =
                        Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                          quickcheck_observer
                          (fun () -> _x__038_)
                          ~size:_size__036_
                          ~hash:_hash__037_
                      in
                      _hash__037_))]
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
                    | (A : t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                    | (B _x__021_ : t) ->
                      Ppx_quickcheck_runtime.Base.Sequence.round_robin
                        [ Ppx_quickcheck_runtime.Base.Sequence.map
                            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                             .shrink
                               quickcheck_shrinker_u
                               (fun () -> _x__021_))
                            ~f:(fun _f__022_ () ->
                              let _x__021_ = _f__022_ () in
                              let _x__023_ = (B _x__021_ : t) in
                              _x__023_)
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
                 (fun _f__024_ ->
                    match _f__024_ () with
                    | (C : u) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
                    | (D _x__026_ : u) ->
                      Ppx_quickcheck_runtime.Base.Sequence.round_robin
                        [ Ppx_quickcheck_runtime.Base.Sequence.map
                            (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk
                             .shrink
                               quickcheck_shrinker
                               (fun () -> _x__026_))
                            ~f:(fun _f__027_ () ->
                              let _x__026_ = _f__027_ () in
                              let _x__028_ = (D _x__026_ : u) in
                              _x__028_)
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

  module Portable_recursive_parameterized = struct
    type ('a, 'b) t =
      | A
      | B of 'a u

    and 'a u =
      | C of 'a
      | D of ('a, string) t
      | E of v

    and v = (int, bool) t [@@deriving_inline quickcheck ~portable]

    let _ = fun (_ : ('a, 'b) t) -> ()
    let _ = fun (_ : 'a u) -> ()
    let _ = fun (_ : v) -> ()

    include struct
      open struct
        [%%portable
        let rec quickcheck_generator__portable
          : 'a 'b.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
          -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
          -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
          =
          fun _generator__157_ _generator__158_ ->
          let quickcheck_generator__portable _recur__218_ _recur__219_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              [%portable lazy (quickcheck_generator__portable _recur__218_ _recur__219_)]
          and quickcheck_generator_u__portable _recur__220_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              [%portable lazy (quickcheck_generator_u__portable _recur__220_)]
          and quickcheck_generator_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              quickcheck_generator_v
          in
          ignore quickcheck_generator_v;
          ignore quickcheck_generator_u__portable;
          ignore quickcheck_generator__portable;
          let _pair__162_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create__portable
                (fun ~size:_size__167_ ~random:_random__168_ () ->
                   let _x__169_ = (A : (_, _) t) in
                   _x__169_) )
          and _pair__163_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__159_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size__portable
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__159_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                     .create__portable
                       (fun ~size:_size__164_ ~random:_random__165_ () ->
                          let _x__166_ =
                            (B
                               (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                                .generate
                                  (quickcheck_generator_u__portable _generator__157_)
                                  ~size:_size__164_
                                  ~random:_random__165_
                                  ())
                             : (_, _) t)
                          in
                          _x__166_))) )
          in
          let _gen__160_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
              [ _pair__162_ ]
          and _gen__161_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
              [ _pair__162_; _pair__163_ ]
          in
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
            ~f:(function
            | 0 -> _gen__160_
            | _ -> _gen__161_)

        and quickcheck_generator_u__portable
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
          =
          fun _generator__183_ ->
          let quickcheck_generator__portable _recur__218_ _recur__219_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              [%portable lazy (quickcheck_generator__portable _recur__218_ _recur__219_)]
          and quickcheck_generator_u__portable _recur__220_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              [%portable lazy (quickcheck_generator_u__portable _recur__220_)]
          and quickcheck_generator_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              quickcheck_generator_v
          in
          ignore quickcheck_generator_v;
          ignore quickcheck_generator_u__portable;
          ignore quickcheck_generator__portable;
          let _pair__187_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create__portable
                (fun ~size:_size__196_ ~random:_random__197_ () ->
                   let _x__198_ =
                     (C
                        (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                         .generate
                           _generator__183_
                           ~size:_size__196_
                           ~random:_random__197_
                           ())
                      : _ u)
                   in
                   _x__198_) )
          and _pair__188_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__184_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size__portable
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__184_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                     .create__portable
                       (fun ~size:_size__190_ ~random:_random__191_ () ->
                          let _x__192_ =
                            (D
                               (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                                .generate
                                  (quickcheck_generator__portable
                                     _generator__183_
                                     quickcheck_generator_string)
                                  ~size:_size__190_
                                  ~random:_random__191_
                                  ())
                             : _ u)
                          in
                          _x__192_))) )
          and _pair__189_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__184_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size__portable
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__184_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                     .create__portable
                       (fun ~size:_size__193_ ~random:_random__194_ () ->
                          let _x__195_ =
                            (E
                               (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                                .generate
                                  quickcheck_generator_v
                                  ~size:_size__193_
                                  ~random:_random__194_
                                  ())
                             : _ u)
                          in
                          _x__195_))) )
          in
          let _gen__185_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
              [ _pair__187_ ]
          and _gen__186_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
              [ _pair__187_; _pair__188_; _pair__189_ ]
          in
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
            ~f:(function
            | 0 -> _gen__185_
            | _ -> _gen__186_)

        and quickcheck_generator_v
          : v Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_generator__portable _recur__218_ _recur__219_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   [%portable
                     lazy (quickcheck_generator__portable _recur__218_ _recur__219_)]
               and quickcheck_generator_u__portable _recur__220_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   [%portable lazy (quickcheck_generator_u__portable _recur__220_)]
               and quickcheck_generator_v =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   quickcheck_generator_v
               in
               ignore quickcheck_generator_v;
               ignore quickcheck_generator_u__portable;
               ignore quickcheck_generator__portable;
               quickcheck_generator__portable
                 quickcheck_generator_int
                 quickcheck_generator_bool)]
        ;;

        let _ = quickcheck_generator__portable
        and _ = quickcheck_generator_u__portable
        and _ = quickcheck_generator_v]
      end

      let quickcheck_generator__portable _recur__224_ _recur__225_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
          [%portable lazy (quickcheck_generator__portable _recur__224_ _recur__225_)]

      and quickcheck_generator_u__portable _recur__226_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
          [%portable lazy (quickcheck_generator_u__portable _recur__226_)]

      and quickcheck_generator_v =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
          quickcheck_generator_v
      ;;

      let _ = quickcheck_generator__portable
      and _ = quickcheck_generator_u__portable
      and _ = quickcheck_generator_v

      open struct
        let rec quickcheck_generator
          : 'a 'b.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
          -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
          -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
          =
          fun _generator__170_ _generator__171_ ->
          let quickcheck_generator _recur__215_ _recur__216_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
              (lazy (quickcheck_generator _recur__215_ _recur__216_))
          and quickcheck_generator_u _recur__217_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
              (lazy (quickcheck_generator_u _recur__217_))
          in
          ignore quickcheck_generator_u;
          ignore quickcheck_generator;
          let _pair__175_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                (fun ~size:_size__180_ ~random:_random__181_ () ->
                   let _x__182_ = (A : (_, _) t) in
                   _x__182_) )
          and _pair__176_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__172_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__172_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                       (fun ~size:_size__177_ ~random:_random__178_ () ->
                          let _x__179_ =
                            (B
                               (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                                .generate
                                  (quickcheck_generator_u _generator__170_)
                                  ~size:_size__177_
                                  ~random:_random__178_
                                  ())
                             : (_, _) t)
                          in
                          _x__179_))) )
          in
          let _gen__173_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
              [ _pair__175_ ]
          and _gen__174_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
              [ _pair__175_; _pair__176_ ]
          in
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
            ~f:(function
            | 0 -> _gen__173_
            | _ -> _gen__174_)

        and quickcheck_generator_u
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
          =
          fun _generator__199_ ->
          let quickcheck_generator _recur__215_ _recur__216_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
              (lazy (quickcheck_generator _recur__215_ _recur__216_))
          and quickcheck_generator_u _recur__217_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
              (lazy (quickcheck_generator_u _recur__217_))
          in
          ignore quickcheck_generator_u;
          ignore quickcheck_generator;
          let _pair__203_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                (fun ~size:_size__212_ ~random:_random__213_ () ->
                   let _x__214_ =
                     (C
                        (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                         .generate
                           _generator__199_
                           ~size:_size__212_
                           ~random:_random__213_
                           ())
                      : _ u)
                   in
                   _x__214_) )
          and _pair__204_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__200_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__200_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                       (fun ~size:_size__206_ ~random:_random__207_ () ->
                          let _x__208_ =
                            (D
                               (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                                .generate
                                  (quickcheck_generator
                                     _generator__199_
                                     quickcheck_generator_string)
                                  ~size:_size__206_
                                  ~random:_random__207_
                                  ())
                             : _ u)
                          in
                          _x__208_))) )
          and _pair__205_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__200_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__200_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                       (fun ~size:_size__209_ ~random:_random__210_ () ->
                          let _x__211_ =
                            (E
                               (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                                .generate
                                  quickcheck_generator_v
                                  ~size:_size__209_
                                  ~random:_random__210_
                                  ())
                             : _ u)
                          in
                          _x__211_))) )
          in
          let _gen__201_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
              [ _pair__203_ ]
          and _gen__202_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
              [ _pair__203_; _pair__204_; _pair__205_ ]
          in
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
            ~f:(function
            | 0 -> _gen__201_
            | _ -> _gen__202_)
        ;;

        let _ = quickcheck_generator
        and _ = quickcheck_generator_u
      end

      let quickcheck_generator _recur__221_ _recur__222_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
          (lazy (quickcheck_generator _recur__221_ _recur__222_))

      and quickcheck_generator_u _recur__223_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
          (lazy (quickcheck_generator_u _recur__223_))
      ;;

      let _ = quickcheck_generator
      and _ = quickcheck_generator_u
    end

    include struct
      open struct
        [%%portable
        let rec quickcheck_observer__portable
          : 'a 'b.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
          -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
          -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
          =
          fun _observer__115_ _observer__116_ ->
          let quickcheck_observer__portable _recur__148_ _recur__149_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              [%portable lazy (quickcheck_observer__portable _recur__148_ _recur__149_)]
          and quickcheck_observer_u__portable _recur__150_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              [%portable lazy (quickcheck_observer_u__portable _recur__150_)]
          and quickcheck_observer_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              quickcheck_observer_v
          in
          ignore quickcheck_observer_v;
          ignore quickcheck_observer_u__portable;
          ignore quickcheck_observer__portable;
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create__portable
            (fun _f__118_ ~size:_size__119_ ~hash:_hash__120_ ->
               let _x__117_ = _f__118_ () in
               match _x__117_ with
               | (A : (_, _) t) ->
                 let _hash__120_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__120_ 0
                 in
                 _hash__120_
               | (B _x__121_ : (_, _) t) ->
                 let _hash__120_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__120_ 1
                 in
                 let _hash__120_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     (quickcheck_observer_u__portable _observer__115_)
                     (fun () -> _x__121_)
                     ~size:_size__119_
                     ~hash:_hash__120_
                 in
                 _hash__120_)

        and quickcheck_observer_u__portable
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
          =
          fun _observer__129_ ->
          let quickcheck_observer__portable _recur__148_ _recur__149_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              [%portable lazy (quickcheck_observer__portable _recur__148_ _recur__149_)]
          and quickcheck_observer_u__portable _recur__150_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              [%portable lazy (quickcheck_observer_u__portable _recur__150_)]
          and quickcheck_observer_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              quickcheck_observer_v
          in
          ignore quickcheck_observer_v;
          ignore quickcheck_observer_u__portable;
          ignore quickcheck_observer__portable;
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create__portable
            (fun _f__131_ ~size:_size__132_ ~hash:_hash__133_ ->
               let _x__130_ = _f__131_ () in
               match _x__130_ with
               | (C _x__134_ : _ u) ->
                 let _hash__133_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__133_ 0
                 in
                 let _hash__133_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     _observer__129_
                     (fun () -> _x__134_)
                     ~size:_size__132_
                     ~hash:_hash__133_
                 in
                 _hash__133_
               | (D _x__135_ : _ u) ->
                 let _hash__133_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__133_ 1
                 in
                 let _hash__133_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     (quickcheck_observer__portable
                        _observer__129_
                        quickcheck_observer_string)
                     (fun () -> _x__135_)
                     ~size:_size__132_
                     ~hash:_hash__133_
                 in
                 _hash__133_
               | (E _x__136_ : _ u) ->
                 let _hash__133_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__133_ 2
                 in
                 let _hash__133_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     quickcheck_observer_v
                     (fun () -> _x__136_)
                     ~size:_size__132_
                     ~hash:_hash__133_
                 in
                 _hash__133_)

        and quickcheck_observer_v
          : v Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_observer__portable _recur__148_ _recur__149_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   [%portable
                     lazy (quickcheck_observer__portable _recur__148_ _recur__149_)]
               and quickcheck_observer_u__portable _recur__150_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   [%portable lazy (quickcheck_observer_u__portable _recur__150_)]
               and quickcheck_observer_v =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   quickcheck_observer_v
               in
               ignore quickcheck_observer_v;
               ignore quickcheck_observer_u__portable;
               ignore quickcheck_observer__portable;
               quickcheck_observer__portable
                 quickcheck_observer_int
                 quickcheck_observer_bool)]
        ;;

        let _ = quickcheck_observer__portable
        and _ = quickcheck_observer_u__portable
        and _ = quickcheck_observer_v]
      end

      let quickcheck_observer__portable _recur__154_ _recur__155_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
          [%portable lazy (quickcheck_observer__portable _recur__154_ _recur__155_)]

      and quickcheck_observer_u__portable _recur__156_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
          [%portable lazy (quickcheck_observer_u__portable _recur__156_)]

      and quickcheck_observer_v =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
          quickcheck_observer_v
      ;;

      let _ = quickcheck_observer__portable
      and _ = quickcheck_observer_u__portable
      and _ = quickcheck_observer_v

      open struct
        let rec quickcheck_observer
          : 'a 'b.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
          -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
          -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
          =
          fun _observer__122_ _observer__123_ ->
          let quickcheck_observer _recur__145_ _recur__146_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
              (lazy (quickcheck_observer _recur__145_ _recur__146_))
          and quickcheck_observer_u _recur__147_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
              (lazy (quickcheck_observer_u _recur__147_))
          in
          ignore quickcheck_observer_u;
          ignore quickcheck_observer;
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
            (fun _f__125_ ~size:_size__126_ ~hash:_hash__127_ ->
               let _x__124_ = _f__125_ () in
               match _x__124_ with
               | (A : (_, _) t) ->
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__127_ 0
                 in
                 _hash__127_
               | (B _x__128_ : (_, _) t) ->
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__127_ 1
                 in
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     (quickcheck_observer_u _observer__122_)
                     (fun () -> _x__128_)
                     ~size:_size__126_
                     ~hash:_hash__127_
                 in
                 _hash__127_)

        and quickcheck_observer_u
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
          =
          fun _observer__137_ ->
          let quickcheck_observer _recur__145_ _recur__146_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
              (lazy (quickcheck_observer _recur__145_ _recur__146_))
          and quickcheck_observer_u _recur__147_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
              (lazy (quickcheck_observer_u _recur__147_))
          in
          ignore quickcheck_observer_u;
          ignore quickcheck_observer;
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
            (fun _f__139_ ~size:_size__140_ ~hash:_hash__141_ ->
               let _x__138_ = _f__139_ () in
               match _x__138_ with
               | (C _x__142_ : _ u) ->
                 let _hash__141_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__141_ 0
                 in
                 let _hash__141_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     _observer__137_
                     (fun () -> _x__142_)
                     ~size:_size__140_
                     ~hash:_hash__141_
                 in
                 _hash__141_
               | (D _x__143_ : _ u) ->
                 let _hash__141_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__141_ 1
                 in
                 let _hash__141_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     (quickcheck_observer _observer__137_ quickcheck_observer_string)
                     (fun () -> _x__143_)
                     ~size:_size__140_
                     ~hash:_hash__141_
                 in
                 _hash__141_
               | (E _x__144_ : _ u) ->
                 let _hash__141_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__141_ 2
                 in
                 let _hash__141_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     quickcheck_observer_v
                     (fun () -> _x__144_)
                     ~size:_size__140_
                     ~hash:_hash__141_
                 in
                 _hash__141_)
        ;;

        let _ = quickcheck_observer
        and _ = quickcheck_observer_u
      end

      let quickcheck_observer _recur__151_ _recur__152_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
          (lazy (quickcheck_observer _recur__151_ _recur__152_))

      and quickcheck_observer_u _recur__153_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
          (lazy (quickcheck_observer_u _recur__153_))
      ;;

      let _ = quickcheck_observer
      and _ = quickcheck_observer_u
    end

    include struct
      open struct
        [%%portable
        let rec quickcheck_shrinker__portable
          : 'a 'b.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
          -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
          -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
          =
          fun _shrinker__067_ _shrinker__068_ ->
          let quickcheck_shrinker__portable _recur__106_ _recur__107_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              [%portable lazy (quickcheck_shrinker__portable _recur__106_ _recur__107_)]
          and quickcheck_shrinker_u__portable _recur__108_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              [%portable lazy (quickcheck_shrinker_u__portable _recur__108_)]
          and quickcheck_shrinker_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              quickcheck_shrinker_v
          in
          ignore quickcheck_shrinker_v;
          ignore quickcheck_shrinker_u__portable;
          ignore quickcheck_shrinker__portable;
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create__portable
            (fun _f__069_ ->
               match _f__069_ () with
               | (A : (_, _) t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
               | (B _x__071_ : (_, _) t) ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          (quickcheck_shrinker_u__portable _shrinker__067_)
                          (fun () -> _x__071_))
                       ~f:(fun _f__072_ () ->
                         let _x__071_ = _f__072_ () in
                         let _x__073_ = (B _x__071_ : (_, _) t) in
                         _x__073_)
                   ])

        and quickcheck_shrinker_u__portable
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
          =
          fun _shrinker__081_ ->
          let quickcheck_shrinker__portable _recur__106_ _recur__107_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              [%portable lazy (quickcheck_shrinker__portable _recur__106_ _recur__107_)]
          and quickcheck_shrinker_u__portable _recur__108_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              [%portable lazy (quickcheck_shrinker_u__portable _recur__108_)]
          and quickcheck_shrinker_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              quickcheck_shrinker_v
          in
          ignore quickcheck_shrinker_v;
          ignore quickcheck_shrinker_u__portable;
          ignore quickcheck_shrinker__portable;
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create__portable
            (fun _f__082_ ->
               match _f__082_ () with
               | (C _x__083_ : _ u) ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          _shrinker__081_
                          (fun () -> _x__083_))
                       ~f:(fun _f__084_ () ->
                         let _x__083_ = _f__084_ () in
                         let _x__085_ = (C _x__083_ : _ u) in
                         _x__085_)
                   ]
               | (D _x__086_ : _ u) ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          (quickcheck_shrinker__portable
                             _shrinker__081_
                             quickcheck_shrinker_string)
                          (fun () -> _x__086_))
                       ~f:(fun _f__087_ () ->
                         let _x__086_ = _f__087_ () in
                         let _x__088_ = (D _x__086_ : _ u) in
                         _x__088_)
                   ]
               | (E _x__089_ : _ u) ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          quickcheck_shrinker_v
                          (fun () -> _x__089_))
                       ~f:(fun _f__090_ () ->
                         let _x__089_ = _f__090_ () in
                         let _x__091_ = (E _x__089_ : _ u) in
                         _x__091_)
                   ])

        and quickcheck_shrinker_v
          : v Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_shrinker__portable _recur__106_ _recur__107_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   [%portable
                     lazy (quickcheck_shrinker__portable _recur__106_ _recur__107_)]
               and quickcheck_shrinker_u__portable _recur__108_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   [%portable lazy (quickcheck_shrinker_u__portable _recur__108_)]
               and quickcheck_shrinker_v =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   quickcheck_shrinker_v
               in
               ignore quickcheck_shrinker_v;
               ignore quickcheck_shrinker_u__portable;
               ignore quickcheck_shrinker__portable;
               quickcheck_shrinker__portable
                 quickcheck_shrinker_int
                 quickcheck_shrinker_bool)]
        ;;

        let _ = quickcheck_shrinker__portable
        and _ = quickcheck_shrinker_u__portable
        and _ = quickcheck_shrinker_v]
      end

      let quickcheck_shrinker__portable _recur__112_ _recur__113_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
          [%portable lazy (quickcheck_shrinker__portable _recur__112_ _recur__113_)]

      and quickcheck_shrinker_u__portable _recur__114_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
          [%portable lazy (quickcheck_shrinker_u__portable _recur__114_)]

      and quickcheck_shrinker_v =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
          quickcheck_shrinker_v
      ;;

      let _ = quickcheck_shrinker__portable
      and _ = quickcheck_shrinker_u__portable
      and _ = quickcheck_shrinker_v

      open struct
        let rec quickcheck_shrinker
          : 'a 'b.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
          -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
          -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
          =
          fun _shrinker__074_ _shrinker__075_ ->
          let quickcheck_shrinker _recur__103_ _recur__104_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
              (lazy (quickcheck_shrinker _recur__103_ _recur__104_))
          and quickcheck_shrinker_u _recur__105_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
              (lazy (quickcheck_shrinker_u _recur__105_))
          in
          ignore quickcheck_shrinker_u;
          ignore quickcheck_shrinker;
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
            (fun _f__076_ ->
               match _f__076_ () with
               | (A : (_, _) t) -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
               | (B _x__078_ : (_, _) t) ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          (quickcheck_shrinker_u _shrinker__074_)
                          (fun () -> _x__078_))
                       ~f:(fun _f__079_ () ->
                         let _x__078_ = _f__079_ () in
                         let _x__080_ = (B _x__078_ : (_, _) t) in
                         _x__080_)
                   ])

        and quickcheck_shrinker_u
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
          =
          fun _shrinker__092_ ->
          let quickcheck_shrinker _recur__103_ _recur__104_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
              (lazy (quickcheck_shrinker _recur__103_ _recur__104_))
          and quickcheck_shrinker_u _recur__105_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
              (lazy (quickcheck_shrinker_u _recur__105_))
          in
          ignore quickcheck_shrinker_u;
          ignore quickcheck_shrinker;
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
            (fun _f__093_ ->
               match _f__093_ () with
               | (C _x__094_ : _ u) ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          _shrinker__092_
                          (fun () -> _x__094_))
                       ~f:(fun _f__095_ () ->
                         let _x__094_ = _f__095_ () in
                         let _x__096_ = (C _x__094_ : _ u) in
                         _x__096_)
                   ]
               | (D _x__097_ : _ u) ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          (quickcheck_shrinker _shrinker__092_ quickcheck_shrinker_string)
                          (fun () -> _x__097_))
                       ~f:(fun _f__098_ () ->
                         let _x__097_ = _f__098_ () in
                         let _x__099_ = (D _x__097_ : _ u) in
                         _x__099_)
                   ]
               | (E _x__100_ : _ u) ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          quickcheck_shrinker_v
                          (fun () -> _x__100_))
                       ~f:(fun _f__101_ () ->
                         let _x__100_ = _f__101_ () in
                         let _x__102_ = (E _x__100_ : _ u) in
                         _x__102_)
                   ])
        ;;

        let _ = quickcheck_shrinker
        and _ = quickcheck_shrinker_u
      end

      let quickcheck_shrinker _recur__109_ _recur__110_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
          (lazy (quickcheck_shrinker _recur__109_ _recur__110_))

      and quickcheck_shrinker_u _recur__111_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
          (lazy (quickcheck_shrinker_u _recur__111_))
      ;;

      let _ = quickcheck_shrinker
      and _ = quickcheck_shrinker_u
    end

    [@@@end]
  end
end
