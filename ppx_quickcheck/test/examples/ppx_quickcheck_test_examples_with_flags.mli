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
        :  'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
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
        :  'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
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
        :  'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
        -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end
end
