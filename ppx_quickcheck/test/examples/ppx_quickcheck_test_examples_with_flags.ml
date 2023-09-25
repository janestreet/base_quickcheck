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
      Ppx_quickcheck_runtime.Base_quickcheck.Generator.create
        (fun ~size:_size__003_ ~random:_random__004_ ->
        ( Ppx_quickcheck_runtime.Base_quickcheck.Generator.generate
            _generator__002_
            ~size:_size__003_
            ~random:_random__004_
        , Ppx_quickcheck_runtime.Base_quickcheck.Generator.generate
            (Ppx_quickcheck_runtime.Base_quickcheck.Generator.fn
               _observer__001_
               _generator__002_)
            ~size:_size__003_
            ~random:_random__004_ ))
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
      Ppx_quickcheck_runtime.Base_quickcheck.Observer.create
        (fun _x__007_ ~size:_size__010_ ~hash:_hash__011_ ->
        let _x__008_, _x__009_ = _x__007_ in
        let _hash__011_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.observe
            _observer__006_
            _x__008_
            ~size:_size__010_
            ~hash:_hash__011_
        in
        let _hash__011_ =
          Ppx_quickcheck_runtime.Base_quickcheck.Observer.observe
            (Ppx_quickcheck_runtime.Base_quickcheck.Observer.fn
               _generator__005_
               _observer__006_)
            _x__009_
            ~size:_size__010_
            ~hash:_hash__011_
        in
        _hash__011_)
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

    let quickcheck_shrinker _shrinker__012_ _shrinker__013_ =
      Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.create (fun (_x__014_, _x__015_) ->
        Ppx_quickcheck_runtime.Base.Sequence.round_robin
          [ Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.shrink
                 _shrinker__013_
                 _x__014_)
              ~f:(fun _x__014_ -> _x__014_, _x__015_)
          ; Ppx_quickcheck_runtime.Base.Sequence.map
              (Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.shrink
                 Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.atomic
                 _x__015_)
              ~f:(fun _x__015_ -> _x__014_, _x__015_)
          ])
    ;;

    let _ = quickcheck_shrinker

    [@@@end]
  end
end
