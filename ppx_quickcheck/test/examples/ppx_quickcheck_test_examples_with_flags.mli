open Base

(* ensure that shadowing doesn't break anything *)
include module type of struct
  module Base = struct end
  module Base_quickcheck = struct end
  module Quickcheckable = struct end
end

module Generator : sig
  module Simple_reference : sig
    type t = bool [@@deriving_inline quickcheck ~generator]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_generator : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Simple_arrow : sig
    type t = unit option -> bool [@@deriving_inline quickcheck ~generator]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_generator : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Simple_higher_order : sig
    type t = (unit option -> bool option) -> bool
    [@@deriving_inline quickcheck ~generator]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_generator : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Poly_with_variance : sig
    type (-'a, +'b) t = 'b * ('a -> 'b) [@@deriving_inline quickcheck ~generator]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_generator
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Observer : sig
  module Simple_reference : sig
    type t = bool [@@deriving_inline quickcheck ~observer]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_observer : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Simple_arrow : sig
    type t = unit option -> bool [@@deriving_inline quickcheck ~observer]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_observer : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Simple_higher_order : sig
    type t = (unit option -> bool option) -> bool [@@deriving_inline quickcheck ~observer]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_observer : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Poly_with_variance : sig
    type (-'a, +'b) t = 'b * ('a -> 'b) [@@deriving_inline quickcheck ~observer]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_observer
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end
end

module Shrinker : sig
  module Simple_reference : sig
    type t = bool [@@deriving_inline quickcheck ~shrinker]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_shrinker : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Simple_arrow : sig
    type t = unit option -> bool [@@deriving_inline quickcheck ~shrinker]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_shrinker : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Simple_higher_order : sig
    type t = (unit option -> bool option) -> bool [@@deriving_inline quickcheck ~shrinker]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_shrinker : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Poly_with_variance : sig
    type (-'a, +'b) t = 'b * ('a -> 'b) [@@deriving_inline quickcheck ~shrinker]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_shrinker
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Portable_recursive : sig
    type t =
      | A
      | B of u

    and u =
      | C
      | D of t
    [@@deriving_inline quickcheck ~portable]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_generator
        : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        @@ portable

      val quickcheck_generator_u
        : u Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        @@ portable

      val quickcheck_observer
        : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        @@ portable

      val quickcheck_observer_u
        : u Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        @@ portable

      val quickcheck_shrinker
        : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        @@ portable

      val quickcheck_shrinker_u
        : u Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        @@ portable
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end

  module Portable_recursive_parameterized : sig
    type ('a, 'b) t =
      | A
      | B of 'a u

    and 'a u =
      | C of 'a
      | D of ('a, string) t
      | E of v

    and v = (int, bool) t [@@deriving_inline quickcheck ~portable]

    include sig
      [@@@ocaml.warning "-32"]

      val quickcheck_generator
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        @@ portable

      val quickcheck_generator_u
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        @@ portable

      val quickcheck_generator__portable
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
        @@ portable

      val quickcheck_generator_u__portable
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
        -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Generator.t @ portable
        @@ portable

      val quickcheck_generator_v
        : v Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
        @@ portable

      val quickcheck_observer
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        @@ portable

      val quickcheck_observer_u
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        @@ portable

      val quickcheck_observer__portable
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
        @@ portable

      val quickcheck_observer_u__portable
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
        -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Observer.t @ portable
        @@ portable

      val quickcheck_observer_v
        : v Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
        @@ portable

      val quickcheck_shrinker
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        @@ portable

      val quickcheck_shrinker_u
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        @@ portable

      val quickcheck_shrinker__portable
        : 'a 'b.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
        @@ portable

      val quickcheck_shrinker_u__portable
        : 'a.
        'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
        -> 'a u Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t @ portable
        @@ portable

      val quickcheck_shrinker_v
        : v Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        @@ portable
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end
end
