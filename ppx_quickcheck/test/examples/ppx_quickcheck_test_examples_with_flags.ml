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
          fun _generator__143_ _generator__144_ ->
          let quickcheck_generator__portable _recur__194_ _recur__195_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              [%portable lazy (quickcheck_generator__portable _recur__194_ _recur__195_)]
          and quickcheck_generator_u__portable _recur__196_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              [%portable lazy (quickcheck_generator_u__portable _recur__196_)]
          and quickcheck_generator_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              quickcheck_generator_v
          in
          ignore quickcheck_generator_v;
          ignore quickcheck_generator_u__portable;
          ignore quickcheck_generator__portable;
          let _pair__148_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create__portable
                (fun ~size:_size__152_ ~random:_random__153_ () -> A) )
          and _pair__149_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__145_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size__portable
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__145_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                     .create__portable
                       (fun ~size:_size__150_ ~random:_random__151_ () ->
                          B
                            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                             .generate
                               (quickcheck_generator_u__portable _generator__143_)
                               ~size:_size__150_
                               ~random:_random__151_
                               ())))) )
          in
          let _gen__146_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
              [ _pair__148_ ]
          and _gen__147_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
              [ _pair__148_; _pair__149_ ]
          in
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
            ~f:(function
            | 0 -> _gen__146_
            | _ -> _gen__147_)

        and quickcheck_generator_u__portable
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
          =
          fun _generator__165_ ->
          let quickcheck_generator__portable _recur__194_ _recur__195_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              [%portable lazy (quickcheck_generator__portable _recur__194_ _recur__195_)]
          and quickcheck_generator_u__portable _recur__196_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              [%portable lazy (quickcheck_generator_u__portable _recur__196_)]
          and quickcheck_generator_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
              quickcheck_generator_v
          in
          ignore quickcheck_generator_v;
          ignore quickcheck_generator_u__portable;
          ignore quickcheck_generator__portable;
          let _pair__169_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create__portable
                (fun ~size:_size__176_ ~random:_random__177_ () ->
                   C
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                        _generator__165_
                        ~size:_size__176_
                        ~random:_random__177_
                        ())) )
          and _pair__170_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__166_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size__portable
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__166_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                     .create__portable
                       (fun ~size:_size__172_ ~random:_random__173_ () ->
                          D
                            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                             .generate
                               (quickcheck_generator__portable
                                  _generator__165_
                                  quickcheck_generator_string)
                               ~size:_size__172_
                               ~random:_random__173_
                               ())))) )
          and _pair__171_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__166_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size__portable
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__166_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                     .create__portable
                       (fun ~size:_size__174_ ~random:_random__175_ () ->
                          E
                            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                             .generate
                               quickcheck_generator_v
                               ~size:_size__174_
                               ~random:_random__175_
                               ())))) )
          in
          let _gen__167_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
              [ _pair__169_ ]
          and _gen__168_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union__portable
              [ _pair__169_; _pair__170_; _pair__171_ ]
          in
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind__portable
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
            ~f:(function
            | 0 -> _gen__167_
            | _ -> _gen__168_)

        and quickcheck_generator_v
          : v Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_generator__portable _recur__194_ _recur__195_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   [%portable
                     lazy (quickcheck_generator__portable _recur__194_ _recur__195_)]
               and quickcheck_generator_u__portable _recur__196_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
                   [%portable lazy (quickcheck_generator_u__portable _recur__196_)]
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

      let quickcheck_generator__portable _recur__200_ _recur__201_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
          [%portable lazy (quickcheck_generator__portable _recur__200_ _recur__201_)]

      and quickcheck_generator_u__portable _recur__202_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_portable_lazy
          [%portable lazy (quickcheck_generator_u__portable _recur__202_)]

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
          fun _generator__154_ _generator__155_ ->
          let quickcheck_generator _recur__191_ _recur__192_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
              (lazy (quickcheck_generator _recur__191_ _recur__192_))
          and quickcheck_generator_u _recur__193_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
              (lazy (quickcheck_generator_u _recur__193_))
          in
          ignore quickcheck_generator_u;
          ignore quickcheck_generator;
          let _pair__159_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                (fun ~size:_size__163_ ~random:_random__164_ () -> A) )
          and _pair__160_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__156_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__156_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                       (fun ~size:_size__161_ ~random:_random__162_ () ->
                          B
                            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                             .generate
                               (quickcheck_generator_u _generator__154_)
                               ~size:_size__161_
                               ~random:_random__162_
                               ())))) )
          in
          let _gen__157_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
              [ _pair__159_ ]
          and _gen__158_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
              [ _pair__159_; _pair__160_ ]
          in
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
            ~f:(function
            | 0 -> _gen__157_
            | _ -> _gen__158_)

        and quickcheck_generator_u
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
          =
          fun _generator__178_ ->
          let quickcheck_generator _recur__191_ _recur__192_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
              (lazy (quickcheck_generator _recur__191_ _recur__192_))
          and quickcheck_generator_u _recur__193_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
              (lazy (quickcheck_generator_u _recur__193_))
          in
          ignore quickcheck_generator_u;
          ignore quickcheck_generator;
          let _pair__182_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                (fun ~size:_size__189_ ~random:_random__190_ () ->
                   C
                     (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.generate
                        _generator__178_
                        ~size:_size__189_
                        ~random:_random__190_
                        ())) )
          and _pair__183_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__179_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__179_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                       (fun ~size:_size__185_ ~random:_random__186_ () ->
                          D
                            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                             .generate
                               (quickcheck_generator
                                  _generator__178_
                                  quickcheck_generator_string)
                               ~size:_size__185_
                               ~random:_random__186_
                               ())))) )
          and _pair__184_ =
            ( 1.
            , Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
                Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
                ~f:(fun _size__179_ ->
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.with_size
                    ~size:(Ppx_quickcheck_runtime.Base.Int.pred _size__179_)
                    (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk.create
                       (fun ~size:_size__187_ ~random:_random__188_ () ->
                          E
                            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.Via_thunk
                             .generate
                               quickcheck_generator_v
                               ~size:_size__187_
                               ~random:_random__188_
                               ())))) )
          in
          let _gen__180_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
              [ _pair__182_ ]
          and _gen__181_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
              [ _pair__182_; _pair__183_; _pair__184_ ]
          in
          Ppx_quickcheck_runtime.Base_quickcheck.Generator.bind
            Ppx_quickcheck_runtime.Base_quickcheck.Generator.size
            ~f:(function
            | 0 -> _gen__180_
            | _ -> _gen__181_)
        ;;

        let _ = quickcheck_generator
        and _ = quickcheck_generator_u
      end

      let quickcheck_generator _recur__197_ _recur__198_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
          (lazy (quickcheck_generator _recur__197_ _recur__198_))

      and quickcheck_generator_u _recur__199_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy
          (lazy (quickcheck_generator_u _recur__199_))
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
          fun _observer__101_ _observer__102_ ->
          let quickcheck_observer__portable _recur__134_ _recur__135_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              [%portable lazy (quickcheck_observer__portable _recur__134_ _recur__135_)]
          and quickcheck_observer_u__portable _recur__136_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              [%portable lazy (quickcheck_observer_u__portable _recur__136_)]
          and quickcheck_observer_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              quickcheck_observer_v
          in
          ignore quickcheck_observer_v;
          ignore quickcheck_observer_u__portable;
          ignore quickcheck_observer__portable;
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create__portable
            (fun _f__104_ ~size:_size__105_ ~hash:_hash__106_ ->
               let _x__103_ = _f__104_ () in
               match _x__103_ with
               | A ->
                 let _hash__106_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__106_ 0
                 in
                 _hash__106_
               | B _x__107_ ->
                 let _hash__106_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__106_ 1
                 in
                 let _hash__106_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     (quickcheck_observer_u__portable _observer__101_)
                     (fun () -> _x__107_)
                     ~size:_size__105_
                     ~hash:_hash__106_
                 in
                 _hash__106_)

        and quickcheck_observer_u__portable
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
          =
          fun _observer__115_ ->
          let quickcheck_observer__portable _recur__134_ _recur__135_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              [%portable lazy (quickcheck_observer__portable _recur__134_ _recur__135_)]
          and quickcheck_observer_u__portable _recur__136_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              [%portable lazy (quickcheck_observer_u__portable _recur__136_)]
          and quickcheck_observer_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
              quickcheck_observer_v
          in
          ignore quickcheck_observer_v;
          ignore quickcheck_observer_u__portable;
          ignore quickcheck_observer__portable;
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create__portable
            (fun _f__117_ ~size:_size__118_ ~hash:_hash__119_ ->
               let _x__116_ = _f__117_ () in
               match _x__116_ with
               | C _x__120_ ->
                 let _hash__119_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__119_ 0
                 in
                 let _hash__119_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     _observer__115_
                     (fun () -> _x__120_)
                     ~size:_size__118_
                     ~hash:_hash__119_
                 in
                 _hash__119_
               | D _x__121_ ->
                 let _hash__119_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__119_ 1
                 in
                 let _hash__119_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     (quickcheck_observer__portable
                        _observer__115_
                        quickcheck_observer_string)
                     (fun () -> _x__121_)
                     ~size:_size__118_
                     ~hash:_hash__119_
                 in
                 _hash__119_
               | E _x__122_ ->
                 let _hash__119_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__119_ 2
                 in
                 let _hash__119_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     quickcheck_observer_v
                     (fun () -> _x__122_)
                     ~size:_size__118_
                     ~hash:_hash__119_
                 in
                 _hash__119_)

        and quickcheck_observer_v
          : v Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_observer__portable _recur__134_ _recur__135_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   [%portable
                     lazy (quickcheck_observer__portable _recur__134_ _recur__135_)]
               and quickcheck_observer_u__portable _recur__136_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
                   [%portable lazy (quickcheck_observer_u__portable _recur__136_)]
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

      let quickcheck_observer__portable _recur__140_ _recur__141_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
          [%portable lazy (quickcheck_observer__portable _recur__140_ _recur__141_)]

      and quickcheck_observer_u__portable _recur__142_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_portable_lazy
          [%portable lazy (quickcheck_observer_u__portable _recur__142_)]

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
          fun _observer__108_ _observer__109_ ->
          let quickcheck_observer _recur__131_ _recur__132_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
              (lazy (quickcheck_observer _recur__131_ _recur__132_))
          and quickcheck_observer_u _recur__133_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
              (lazy (quickcheck_observer_u _recur__133_))
          in
          ignore quickcheck_observer_u;
          ignore quickcheck_observer;
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
            (fun _f__111_ ~size:_size__112_ ~hash:_hash__113_ ->
               let _x__110_ = _f__111_ () in
               match _x__110_ with
               | A ->
                 let _hash__113_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__113_ 0
                 in
                 _hash__113_
               | B _x__114_ ->
                 let _hash__113_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__113_ 1
                 in
                 let _hash__113_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     (quickcheck_observer_u _observer__108_)
                     (fun () -> _x__114_)
                     ~size:_size__112_
                     ~hash:_hash__113_
                 in
                 _hash__113_)

        and quickcheck_observer_u
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
          =
          fun _observer__123_ ->
          let quickcheck_observer _recur__131_ _recur__132_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
              (lazy (quickcheck_observer _recur__131_ _recur__132_))
          and quickcheck_observer_u _recur__133_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
              (lazy (quickcheck_observer_u _recur__133_))
          in
          ignore quickcheck_observer_u;
          ignore quickcheck_observer;
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.create
            (fun _f__125_ ~size:_size__126_ ~hash:_hash__127_ ->
               let _x__124_ = _f__125_ () in
               match _x__124_ with
               | C _x__128_ ->
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__127_ 0
                 in
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     _observer__123_
                     (fun () -> _x__128_)
                     ~size:_size__126_
                     ~hash:_hash__127_
                 in
                 _hash__127_
               | D _x__129_ ->
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__127_ 1
                 in
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     (quickcheck_observer _observer__123_ quickcheck_observer_string)
                     (fun () -> _x__129_)
                     ~size:_size__126_
                     ~hash:_hash__127_
                 in
                 _hash__127_
               | E _x__130_ ->
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base.hash_fold_int _hash__127_ 2
                 in
                 let _hash__127_ =
                   Ppx_quickcheck_runtime.Base_quickcheck.Observer.Via_thunk.observe
                     quickcheck_observer_v
                     (fun () -> _x__130_)
                     ~size:_size__126_
                     ~hash:_hash__127_
                 in
                 _hash__127_)
        ;;

        let _ = quickcheck_observer
        and _ = quickcheck_observer_u
      end

      let quickcheck_observer _recur__137_ _recur__138_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
          (lazy (quickcheck_observer _recur__137_ _recur__138_))

      and quickcheck_observer_u _recur__139_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy
          (lazy (quickcheck_observer_u _recur__139_))
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
          fun _shrinker__061_ _shrinker__062_ ->
          let quickcheck_shrinker__portable _recur__092_ _recur__093_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              [%portable lazy (quickcheck_shrinker__portable _recur__092_ _recur__093_)]
          and quickcheck_shrinker_u__portable _recur__094_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              [%portable lazy (quickcheck_shrinker_u__portable _recur__094_)]
          and quickcheck_shrinker_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              quickcheck_shrinker_v
          in
          ignore quickcheck_shrinker_v;
          ignore quickcheck_shrinker_u__portable;
          ignore quickcheck_shrinker__portable;
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create__portable
            (fun _f__063_ ->
               match _f__063_ () with
               | A -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
               | B _x__065_ ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          (quickcheck_shrinker_u__portable _shrinker__061_)
                          (fun () -> _x__065_))
                       ~f:(fun _f__066_ () ->
                         let _x__065_ = _f__066_ () in
                         B _x__065_)
                   ])

        and quickcheck_shrinker_u__portable
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
          =
          fun _shrinker__073_ ->
          let quickcheck_shrinker__portable _recur__092_ _recur__093_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              [%portable lazy (quickcheck_shrinker__portable _recur__092_ _recur__093_)]
          and quickcheck_shrinker_u__portable _recur__094_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              [%portable lazy (quickcheck_shrinker_u__portable _recur__094_)]
          and quickcheck_shrinker_v =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
              quickcheck_shrinker_v
          in
          ignore quickcheck_shrinker_v;
          ignore quickcheck_shrinker_u__portable;
          ignore quickcheck_shrinker__portable;
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create__portable
            (fun _f__074_ ->
               match _f__074_ () with
               | C _x__075_ ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          _shrinker__073_
                          (fun () -> _x__075_))
                       ~f:(fun _f__076_ () ->
                         let _x__075_ = _f__076_ () in
                         C _x__075_)
                   ]
               | D _x__077_ ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          (quickcheck_shrinker__portable
                             _shrinker__073_
                             quickcheck_shrinker_string)
                          (fun () -> _x__077_))
                       ~f:(fun _f__078_ () ->
                         let _x__077_ = _f__078_ () in
                         D _x__077_)
                   ]
               | E _x__079_ ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          quickcheck_shrinker_v
                          (fun () -> _x__079_))
                       ~f:(fun _f__080_ () ->
                         let _x__079_ = _f__080_ () in
                         E _x__079_)
                   ])

        and quickcheck_shrinker_v
          : v Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
              Ppx_quickcheck_runtime.Base.Portable_lazy.t
          =
          [%portable
            lazy
              (let quickcheck_shrinker__portable _recur__092_ _recur__093_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   [%portable
                     lazy (quickcheck_shrinker__portable _recur__092_ _recur__093_)]
               and quickcheck_shrinker_u__portable _recur__094_ =
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
                   [%portable lazy (quickcheck_shrinker_u__portable _recur__094_)]
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

      let quickcheck_shrinker__portable _recur__098_ _recur__099_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
          [%portable lazy (quickcheck_shrinker__portable _recur__098_ _recur__099_)]

      and quickcheck_shrinker_u__portable _recur__100_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_portable_lazy
          [%portable lazy (quickcheck_shrinker_u__portable _recur__100_)]

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
          fun _shrinker__067_ _shrinker__068_ ->
          let quickcheck_shrinker _recur__089_ _recur__090_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
              (lazy (quickcheck_shrinker _recur__089_ _recur__090_))
          and quickcheck_shrinker_u _recur__091_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
              (lazy (quickcheck_shrinker_u _recur__091_))
          in
          ignore quickcheck_shrinker_u;
          ignore quickcheck_shrinker;
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
            (fun _f__069_ ->
               match _f__069_ () with
               | A -> Ppx_quickcheck_runtime.Base.Sequence.round_robin []
               | B _x__071_ ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          (quickcheck_shrinker_u _shrinker__067_)
                          (fun () -> _x__071_))
                       ~f:(fun _f__072_ () ->
                         let _x__071_ = _f__072_ () in
                         B _x__071_)
                   ])

        and quickcheck_shrinker_u
          : 'a.
          'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
          -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
          =
          fun _shrinker__081_ ->
          let quickcheck_shrinker _recur__089_ _recur__090_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
              (lazy (quickcheck_shrinker _recur__089_ _recur__090_))
          and quickcheck_shrinker_u _recur__091_ =
            Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
              (lazy (quickcheck_shrinker_u _recur__091_))
          in
          ignore quickcheck_shrinker_u;
          ignore quickcheck_shrinker;
          Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.create
            (fun _f__082_ ->
               match _f__082_ () with
               | C _x__083_ ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          _shrinker__081_
                          (fun () -> _x__083_))
                       ~f:(fun _f__084_ () ->
                         let _x__083_ = _f__084_ () in
                         C _x__083_)
                   ]
               | D _x__085_ ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          (quickcheck_shrinker _shrinker__081_ quickcheck_shrinker_string)
                          (fun () -> _x__085_))
                       ~f:(fun _f__086_ () ->
                         let _x__085_ = _f__086_ () in
                         D _x__085_)
                   ]
               | E _x__087_ ->
                 Ppx_quickcheck_runtime.Base.Sequence.round_robin
                   [ Ppx_quickcheck_runtime.Base.Sequence.map
                       (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.Via_thunk.shrink
                          quickcheck_shrinker_v
                          (fun () -> _x__087_))
                       ~f:(fun _f__088_ () ->
                         let _x__087_ = _f__088_ () in
                         E _x__087_)
                   ])
        ;;

        let _ = quickcheck_shrinker
        and _ = quickcheck_shrinker_u
      end

      let quickcheck_shrinker _recur__095_ _recur__096_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
          (lazy (quickcheck_shrinker _recur__095_ _recur__096_))

      and quickcheck_shrinker_u _recur__097_ =
        Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy
          (lazy (quickcheck_shrinker_u _recur__097_))
      ;;

      let _ = quickcheck_shrinker
      and _ = quickcheck_shrinker_u
    end

    [@@@end]
  end
end
