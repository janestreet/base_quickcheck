open Base

(* ensure that shadowing doesn't break anything *)
include module type of struct
  module Base = struct end
  module Base_quickcheck = struct end
  module Quickcheckable = struct end
end

module Simple_reference : sig
  type t = bool [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Dotted_reference : sig
  type t = Simple_reference.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Nonrec_reference : sig
  open Dotted_reference

  type nonrec t = t [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Application_of_polymorphic_type : sig
  type t = bool option [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Tuple : sig
  type t = bool * unit option
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_variant : sig
  type t =
    [ `A
    | `B
    | `C of bool
    | `D of bool
    | `E of bool * unit option
    | `F of bool * unit option
    ]
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Inherit_poly_variant : sig
  type t =
    [ `X
    | Poly_variant.t
    | `Z of unit option
    ]
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Record_type : sig
  type t =
    { mutable x : bool
    ; y : unit option
    }
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Nullary_and_unary_variant : sig
  type t =
    | A
    | B
    | C of unit
    | D of unit
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Binary_and_record_variant : sig
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

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Simple_arrow : sig
  type t = unit option -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Named_arrow : sig
  type t = x:unit option -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Optional_arrow : sig
  type t = ?x:unit option -> unit -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Curried_arrow : sig
  type t = unit option -> bool option -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Simple_higher_order : sig
  type t = (unit option -> bool option) -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Named_higher_order : sig
  type t = (x:unit option -> bool option) -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Optional_higher_order : sig
  type t = (?x:unit option -> unit -> bool option) -> bool
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_unary : sig
  type 'a t = 'a list [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S1 with type 'a t := 'a t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Instance_of_unary : sig
  type t = bool Poly_unary.t [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_binary : sig
  type ('a, 'b) t = 'a * 'b [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S2 with type ('a, 'b) t := ('a, 'b) t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Instance_of_binary : sig
  type t = (bool, unit option) Poly_binary.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_ternary : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include
      Ppx_quickcheck_runtime.Quickcheckable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Instance_of_ternary : sig
  type t = (bool, unit option, (unit option, bool) Poly_binary.t) Poly_ternary.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_with_variance : sig
  type (-'a, +'b) t = 'b * ('a -> 'b)
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    val quickcheck_generator
      :  'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
      -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
      -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t

    val quickcheck_observer
      :  'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
      -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
      -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t

    val quickcheck_shrinker
      :  'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
      -> 'b Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
      -> ('a, 'b) t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Instance_with_variance : sig
  type t = (bool, unit option) Poly_with_variance.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_with_phantom : sig
    type _ t [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

    include sig
      [@@@ocaml.warning "-32"]

      include Ppx_quickcheck_runtime.Quickcheckable.S1 with type 'a__001_ t := 'a__001_ t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end
  with type _ t = unit option

module Instance_with_phantom : sig
  type t = [ `phantom ] Poly_with_phantom.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Recursive : sig
  type t =
    | Leaf
    | Node of t * t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Recursive_with_indirect_base_case : sig
  type t = { children : t list }
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Mutually_recursive : sig
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

  include sig
    [@@@ocaml.warning "-32"]

    val quickcheck_generator_expr
      : expr Ppx_quickcheck_runtime.Base_quickcheck.Generator.t

    val quickcheck_generator_op : op Ppx_quickcheck_runtime.Base_quickcheck.Generator.t

    val quickcheck_generator_args
      : args Ppx_quickcheck_runtime.Base_quickcheck.Generator.t

    val quickcheck_observer_expr : expr Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
    val quickcheck_observer_op : op Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
    val quickcheck_observer_args : args Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
    val quickcheck_shrinker_expr : expr Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
    val quickcheck_shrinker_op : op Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
    val quickcheck_shrinker_args : args Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Poly_recursive : sig
  type 'a t =
    | Zero
    | Succ of 'a * 'a t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S1 with type 'a t := 'a t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Instance_of_recursive : sig
  type t = bool Poly_recursive.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Murec_poly_mono : sig
  type t =
    | Leaf of bool
    | Node of t node

  and 'a node = 'a list [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    val quickcheck_generator : t Ppx_quickcheck_runtime.Base_quickcheck.Generator.t

    val quickcheck_generator_node
      :  'a Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
      -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Generator.t

    val quickcheck_observer : t Ppx_quickcheck_runtime.Base_quickcheck.Observer.t

    val quickcheck_observer_node
      :  'a Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
      -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Observer.t

    val quickcheck_shrinker : t Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t

    val quickcheck_shrinker_node
      :  'a Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
      -> 'a node Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Polymorphic_recursion : sig
  type 'a t =
    | Single of 'a
    | Double of ('a * 'a) t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S1 with type 'a t := 'a t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Extensions : sig
  type t =
    [ `A
    | `B of bool * unit option
    ]
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Escaped : sig
  type t = int * char * bool option
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Wildcard (Elt : sig
    type t

    val examples : t list
  end) : sig
  type t = Elt.t list [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Attribute_override : sig
  type t =
    | Null
    | Text of string
    | Number of float
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Attribute_override_recursive : sig
  type t =
    | Leaf
    | Node1 of t * int64 * t
    | Node2 of t * int64 * t * int64 * t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Deriving_from_wildcard : sig
  type _ transparent = string
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    val quickcheck_generator_transparent
      :  'a__004_ Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
      -> 'a__004_ transparent Ppx_quickcheck_runtime.Base_quickcheck.Generator.t

    val quickcheck_observer_transparent
      :  'a__003_ Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
      -> 'a__003_ transparent Ppx_quickcheck_runtime.Base_quickcheck.Observer.t

    val quickcheck_shrinker_transparent
      :  'a__002_ Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
      -> 'a__002_ transparent Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  type _ opaque [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    val quickcheck_generator_opaque
      :  'a__007_ Ppx_quickcheck_runtime.Base_quickcheck.Generator.t
      -> 'a__007_ opaque Ppx_quickcheck_runtime.Base_quickcheck.Generator.t

    val quickcheck_observer_opaque
      :  'a__006_ Ppx_quickcheck_runtime.Base_quickcheck.Observer.t
      -> 'a__006_ opaque Ppx_quickcheck_runtime.Base_quickcheck.Observer.t

    val quickcheck_shrinker_opaque
      :  'a__005_ Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
      -> 'a__005_ opaque Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val compare_opaque : ('a -> 'a -> int) -> 'a opaque -> 'a opaque -> int
  val sexp_of_opaque : ('a -> Sexp.t) -> 'a opaque -> Sexp.t
  val opaque_examples : int64 opaque list
end

module Do_not_generate_clauses : sig
  module Cannot_generate : sig
    type t

    val all : t list
    val compare : t -> t -> int
    val sexp_of_t : t -> Sexp.t
  end

  type t =
    | Can_generate of bool
    | Cannot_generate of Cannot_generate.t
  [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  module Poly : sig
    type t =
      [ `Can_generate of bool
      | `Cannot_generate of Cannot_generate.t
      ]
    [@@deriving_inline quickcheck ~generator ~observer ~shrinker]

    include sig
      [@@@ocaml.warning "-32"]

      include Ppx_quickcheck_runtime.Quickcheckable.S with type t := t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end
end
