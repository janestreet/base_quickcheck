@@ portable

(** Generators are sources of random values. Every randomized test needs a generator to
    produce its inputs. *)

open! Base

type (+'a : any) t : value mod contended

(** {2 Basic Generators}

    These are good default generators for tests over types from OCaml and Base. They are
    designed to hit corner cases reasonably often, and also generate reasonably good
    coverage of common cases and arbitrary values. *)

include With_basic_types.S with type 'a t := 'a t (** @inline *)

(** Generates random functions that use the given observer to perturb the pseudo-random
    state that is then used to generate the output value. The resulting functions are
    therefore deterministic, assuming the observer is deterministic. *)
val%template fn : 'a Observer0.t @ p -> 'b t @ p -> ('a -> 'b) t @ p
[@@mode p = (nonportable, portable)]

val map_t_m
  :  ('key, 'cmp) Comparator.Module.t
  -> 'key t
  -> 'data t
  -> ('key, 'data, 'cmp) Map.t t

val set_t_m : ('elt, 'cmp) Comparator.Module.t -> 'elt t -> ('elt, 'cmp) Set.t t

val map_tree_using_comparator
  :  comparator:('key, 'cmp) Comparator.t
  -> 'key t
  -> 'data t
  -> ('key, 'data, 'cmp) Map.Using_comparator.Tree.t t

val set_tree_using_comparator
  :  comparator:('elt, 'cmp) Comparator.t
  -> 'elt t
  -> ('elt, 'cmp) Set.Using_comparator.Tree.t t

(** {2 Combining and Modifying Generators} *)

(** Produces any of the given values, weighted uniformly. *)
val of_list : 'a list -> 'a t

val%template of_list : ('a : value mod contended). 'a list @ portable -> 'a t @ portable
[@@mode portable]

(** Chooses among the given generators, weighted uniformly; then chooses a value from that
    generator. *)
val%template union : 'a t list @ p -> 'a t @ p
[@@mode p = (nonportable, portable)]

include Applicative.S with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

module Portable : sig
  module Let_syntax : sig
    val return : ('a : value mod contended). 'a @ portable -> 'a t @ portable
    val ( >>| ) : 'a t @ portable -> ('a -> 'b) @ portable -> 'b t @ portable
    val ( >>= ) : 'a t @ portable -> ('a -> 'b t) @ portable -> 'b t @ portable

    module Let_syntax : sig
      val return : ('a : value mod contended). 'a @ portable -> 'a t @ portable
      val map : 'a t @ portable -> f:('a -> 'b) @ portable -> 'b t @ portable
      val bind : 'a t @ portable -> f:('a -> 'b t) @ portable -> 'b t @ portable
      val both : 'a t @ portable -> 'b t @ portable -> ('a * 'b) t @ portable

      module Open_on_rhs : sig end
    end
  end
end

module%template [@mode portable] Let_syntax = Portable.Let_syntax

val%template return : ('a : value mod contended). 'a @ portable -> 'a t @ portable
[@@mode portable]

val%template map : 'a t @ portable -> f:('a -> 'b) @ portable -> 'b t @ portable
[@@mode portable]

val%template bind : 'a t @ portable -> f:('a -> 'b t) @ portable -> 'b t @ portable
[@@mode portable]

(** {2 Size of Random Values}

    Base_quickcheck threads a size parameter through generators to limit the size of
    unbounded types. Users of Base_quickcheck often do not need to think about the size
    parameter; the default generators handle it sensibly. Generators of atomic types
    ignore it, generators of bounded-size containers like [both] and [either] thread it
    through unchanged, and generators of unbounded-size containers like [list] and
    [set_t_m] distribute the size they are given among their constituents.

    The bindings below allow direct manipulation of the size parameter in cases where
    users want a custom treatment of sizes. There is no prescribed meaning of the size
    parameter for any given type other than that it must be non-negative. As a general
    guideline, however, the time and space used to generate a value should be proportional
    to the size parameter at most.

    The size parameter should be treated as an upper bound but not as a lower bound, so
    for example a generator given a size parameter of 2 should have a chance to generate
    values of size 0 or 1 as well. If the size parameter is treated as a lower bound, then
    for example members of tuples will always be generated at the same size, and test
    cases for members of different size will not be covered. *)

(** Returns the current size parameter. *)
val size : int t

(** Produces a generator that ignores the size parameter passed in by Base_quickcheck and
    instead uses the given [~size] argument. Most often used with [size] to reduce the
    size when dispatching to generators for subparts of a value.

    For example, here is a use of [with_size] and [size] to create a generator for
    optional lists. We are careful to generate [None] even at non-zero sizes; see the note
    above about not using [size] as a lower bound.

    {[
      let optional_list generator =
        let open Let_syntax in
        match%bind both size bool with
        | 0, _ | _, false -> return None
        | k, _ ->
          let%map elements = with_size ~size:(k - 1) (list generator) in
          Some elements
      ;;
    ]} *)
val with_size : 'a t -> size:int -> 'a t

(** Produces a list of sizes that distribute the current size among list elements. The
    [min_length] and [max_length] parameters can be used to bound the length of the
    result.

    This is the distribution used by generators such as [list] to divide up size among
    elements.

    This function is designed so that elements of [list] are always generated at strictly
    smaller size than the list itself. The technical invariant is: if [size_list] is
    generated by [with_size ~size:n (sizes ~min_length ())], then:

    {[
      List.length size_list - min_length + List.sum (module Int) size_list <= n
    ]} *)
val sizes : ?min_length:int -> ?max_length:int -> unit -> int list t @ portable

(** {2 Filtering Generators} *)

(** Produces values for which [f] returns [true]. If [f] returns [false], retries with
    [size] incremented by 1. This avoids [filter] getting stuck if all values at a given
    size fail [f]; see the note above about not using [size] as a lower bound. *)
val%template filter : 'a t @ p -> f:('a -> bool) @ p -> 'a t @ p
[@@mode p = (nonportable, portable)]

(** When [f] produces [Some x], produces [x]. If [f] returns [None], retries with [size]
    incremented by 1, as with [filter]. *)
val%template filter_map : 'a t @ p -> f:('a -> 'b option) @ p -> 'b t @ p
[@@mode p = (nonportable, portable)]

(** {2 Generating Recursive Values} *)

(** Ties the recursive knot to produce generators for recursive types that have multiple
    clauses, separating base cases from recursive cases. At size 0, only base cases are
    produced; at size [n > 0], the base cases are produced at size [n] along with the
    recursive cases at size [n-1]. Raises if the list of base cases is empty or if the
    list of recursive cases is empty.

    For example, here is a use of [recursive_union] to create a generator for an
    expression datatype.

    {[
      type exp =
        | Int of int
        | Bool of bool
        | If of exp * exp * exp
        | Add of exp * exp

      let exp_generator =
        recursive_union
          [ map int ~f:(fun i -> Int i); map bool ~f:(fun b -> Bool b) ]
          ~f:(fun exp ->
            let open Let_syntax in
            [ (let%map a = exp
               and b = exp
               and c = exp in
               If (a, b, c))
            ; (let%map a = exp
               and b = exp in
               Add (a, b))
            ])
      ;;
    ]} *)
val recursive_union : 'a t list -> f:('a t -> 'a t list) -> 'a t

(** Like [recursive_union], without separate clauses or automatic size management. Useful
    for generating recursive types that don't fit the clause structure of
    [recursive_union].

    For example, here is a use of [fixed_point] to create a generator for N-ary trees. No
    manual size management is needed, as [Generator.list] guarantees to generate list
    elements at strictly smaller sizes than the list itself.

    {[
      type tree = Node of tree list

      let tree_generator =
        fixed_point (fun tree -> map (list tree) ~f:(fun trees -> Node trees))
      ;;
    ]} *)
val fixed_point : ('a t -> 'a t) -> 'a t

(** Creates a [t] that forces the lazy argument as necessary. Can be used to tie
    (mutually) recursive knots. *)
val of_lazy : 'a t Lazy.t -> 'a t

(** {2 Custom Random Distributions} *)

(** Produces one of the given values, chosen with the corresponding weight. Weights must
    be non-negative and must have a strictly positive sum. *)
val of_weighted_list : (float * 'a) list -> 'a t

(** Produces one of the given generators, chosen with the corresponding weight, then
    chooses a value from that generator. Weights must be non-negative and must have a
    strictly positive sum. *)
val%template weighted_union : (float * 'a t) list @ p -> 'a t @ p
[@@mode p = (nonportable, portable)]

(** Like [recursive_union], with explicit weights for each clause. Weights must be
    non-negative and the recursive case weights must have a strictly positive sum. *)
val weighted_recursive_union
  :  (float * 'a t) list
  -> f:('a t -> (float * 'a t) list)
  -> 'a t

(** {3 Integer Distributions} *)

(** Produces an integer between 0 and an unspecified upper bound which is proportional to
    [size]. This is a good generator to use for sizes of values like strings which have a
    variable number of fixed-size elements. *)
val small_positive_or_zero_int : int t

(** Like [small_positive_or_zero_int] but with a minimum of [1]. *)
val small_strictly_positive_int : int t

(** {4 Uniform Unbounded Distributions}

    These generators produce any value of the relevant integer type with uniform weight.
    The default generators for these types differ in that they give higher weight to
    corner cases, e.g. [min_value] and [max_value]. *)

val int_uniform : int t
val int32_uniform : int32 t
val int63_uniform : Int63.t t
val int64_uniform : int64 t
val nativeint_uniform : nativeint t

(** {4 Bounded Distributions}

    These generators produce any value between the given inclusive bounds, which must be
    given in nondecreasing order. Higher weight is given to corner cases, e.g. the bounds
    themselves. *)

val int_inclusive : int -> int -> int t @ portable
val int32_inclusive : int32 -> int32 -> int32 t @ portable
val int63_inclusive : Int63.t -> Int63.t -> Int63.t t @ portable
val int64_inclusive : int64 -> int64 -> int64 t @ portable
val nativeint_inclusive : nativeint -> nativeint -> nativeint t @ portable

(** {4 Uniform Bounded Distributions}

    These generators produce any value between the given inclusive bounds, which must be
    given in nondecreasing order. All values are given equal weight. *)

val int_uniform_inclusive : int -> int -> int t @ portable
val int32_uniform_inclusive : int32 -> int32 -> int32 t @ portable
val int63_uniform_inclusive : Int63.t -> Int63.t -> Int63.t t @ portable
val int64_uniform_inclusive : int64 -> int64 -> int64 t @ portable
val nativeint_uniform_inclusive : nativeint -> nativeint -> nativeint t @ portable

(** {4 Uniform in Log Space Distributions}

    These generators produce any value between the given inclusive, non-negative bounds,
    choosing bit-length in that range uniformly and then uniformly among values with that
    bit-length between the bounds. The bounds must be given in nondecreasing order. *)

val int_log_uniform_inclusive : int -> int -> int t @ portable
val int32_log_uniform_inclusive : int32 -> int32 -> int32 t @ portable
val int63_log_uniform_inclusive : Int63.t -> Int63.t -> Int63.t t @ portable
val int64_log_uniform_inclusive : int64 -> int64 -> int64 t @ portable
val nativeint_log_uniform_inclusive : nativeint -> nativeint -> nativeint t @ portable

(** {4 Log Space Distributions}

    Like the [*_log_uniform_inclusive] bindings above, but giving additional weight to
    corner cases, e.g. the given bounds. *)

val int_log_inclusive : int -> int -> int t @ portable
val int32_log_inclusive : int32 -> int32 -> int32 t @ portable
val int63_log_inclusive : Int63.t -> Int63.t -> Int63.t t @ portable
val int64_log_inclusive : int64 -> int64 -> int64 t @ portable
val nativeint_log_inclusive : nativeint -> nativeint -> nativeint t @ portable

(** {4 Geometric Distributions}

    These generators produce a geometric distribution with a given minimum and probabilty
    [p]. In other words, with probability [p], the minimum is produced. Otherwise, a value
    is effectively produced from a geometric distribution with the same [p] and a minimum
    one higher, although the implementation can be more efficent than this. If the result
    overflows, the function returns [max_value] for the integer type.

    Raises if [p <. 0. || 1. <. p.]. *)

val int_geometric : int -> p:float -> int t @ portable
val int32_geometric : int32 -> p:float -> int32 t @ portable
val int63_geometric : Int63.t -> p:float -> Int63.t t @ portable
val int64_geometric : int64 -> p:float -> int64 t @ portable
val nativeint_geometric : nativeint -> p:float -> nativeint t @ portable

(** {3 Floating Point Distributions} *)

(** Generates values between the given bounds, inclusive, which must be finite and in
    nondecreasing order. Weighted toward boundary values. *)
val float_inclusive : float -> float -> float t @ portable

(** Generates values between the given bounds, exclusive, which must be finite and in
    increasing order, with at least one float value between them. Weighted approximately
    uniformly across the resulting range, rounding error notwithstanding. *)
val float_uniform_exclusive : float -> float -> float t @ portable

val float_without_nan : float t
val float_finite : float t
val float_strictly_positive : float t
val float_strictly_negative : float t
val float_positive_or_zero : float t
val float_negative_or_zero : float t
val float_of_class : Float.Class.t -> float t @ portable

(** {3 Character Distributions} *)

val char_lowercase : char t
val char_uppercase : char t
val char_digit : char t
val char_alpha : char t
val char_alphanum : char t
val char_whitespace : char t
val char_print : char t
val char_uniform_inclusive : char -> char -> char t @ portable

(** {3 String Distributions} *)

val string_non_empty : string t
val string_with_length : length:int -> string t @ portable

val%template string_of : char t @ p -> string t @ p [@@mode p = (nonportable, portable)]

val%template string_non_empty_of : char t @ p -> string t @ p
[@@mode p = (nonportable, portable)]

val%template string_with_length_of : char t @ p -> length:int -> string t @ p
[@@mode p = (nonportable, portable)]

(** Produces strings similar to the input, with some number of edits. *)
val string_like : string -> string t @ portable

(** {3 Sexp Distributions} *)

(** Produces s-expressions whose atoms are chosen from the given string distribution. *)
val sexp_of : string t -> Sexp.t t

(** {3 List Distributions} *)

val%template list_non_empty : 'a t @ p -> 'a list t @ p
[@@mode p = (nonportable, portable)]

val%template list_with_length : 'a t @ p -> length:int -> 'a list t @ p
[@@mode p = (nonportable, portable)]

(** Randomly drops elements from a list. The length of each result is chosen uniformly
    between 0 and the length of the input, inclusive. *)
val list_filtered : 'a list -> 'a list t

(** Produces permutations of the given list, weighted uniformly. *)
val list_permutations : 'a list -> 'a list t

val fold_until
  :  ?min_length:int
  -> ?max_length:int
  -> init:'acc
  -> f:('acc -> ('acc, 'final) Continue_or_stop.t t)
  -> finish:('acc -> 'final)
  -> unit
  -> 'final t

(** {3 Bigarray Distributions} *)
include sig
  open Bigarray

  val bigarray1
    :  'a t
    -> ('a, 'b) kind
    -> 'c layout
    -> length:int option
    -> ('a, 'b, 'c) Array1.t t

  val bigstring_with_length
    :  length:int
    -> (char, int8_unsigned_elt, c_layout) Array1.t t @ portable

  val float32_vec_with_length
    :  length:int
    -> (float, float32_elt, fortran_layout) Array1.t t @ portable

  val float64_vec_with_length
    :  length:int
    -> (float, float64_elt, fortran_layout) Array1.t t @ portable
end
(** @inline *)

(** {2 Low-Level Interface}

    These functions provide direct access to the pseudo-random state threaded through
    Base_quickcheck generators. Most users should not need these functions. *)

(** Passes in additional "salt" used to perturb the pseudo-random state used to generate
    random values. Generators' output is intended to be deterministic for any initial
    pseudorandom state, so [perturb] can be used to generate a new generator with the same
    distribution that nonetheless produces different values from the original for any
    given pseudo-random state. *)
val%template perturb : 'a t @ p -> int -> 'a t @ p
[@@mode p = (nonportable, portable)]

(** Creates a generator that calls the given function with the current size parameter and
    pseudorandom state. *)
val%template create : (size:int -> random:Splittable_random.t -> 'a) @ p -> 'a t @ p
[@@mode p = (nonportable, portable)]

(** Generates a random value using the given size and pseudorandom state. Useful when
    using [create] and dispatching to other existing generators. *)
val generate : 'a t -> size:int -> random:Splittable_random.t -> 'a

module Via_thunk : sig
  type ('a : any) thunk := unit -> 'a

  val%template create
    : ('a : any).
    (size:int -> random:Splittable_random.t -> 'a thunk) @ p -> 'a t @ p
  [@@mode p = (nonportable, portable)]

  val generate : ('a : any). 'a t -> size:int -> random:Splittable_random.t -> 'a thunk

  val%template map
    : ('a : any) ('b : any).
    'a t @ p -> f:('a thunk -> 'b thunk) @ p -> 'b t @ p
  [@@mode p = (nonportable, portable)]
end

module Debug : sig
  (** {3 Helpers for debugging generators} *)

  (** [coverage (module Key) sample] counts how many times each key appears in [sample].

      See [Test.with_sample] for a convenient way to generate [sample]. *)
  val coverage
    :  (module Comparator.S with type t = 'k and type comparator_witness = 'cmp)
    -> 'k Sequence.t
    -> ('k, int, 'cmp) Map.t

  (** [monitor t ~f] returns a generator which gives the same values as [t] and also calls
      [f] for each value. This can help diagnose behavior of generators "hidden" behind
      [map], [filter], etc. One might count the number of values a generator produces, or
      record the set of values that do not satisfy some filter. *)
  val monitor : 'a t -> f:('a -> unit) -> 'a t
end
