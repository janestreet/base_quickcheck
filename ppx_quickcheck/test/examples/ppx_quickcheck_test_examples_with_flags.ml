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
end
