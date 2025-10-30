(** Shrinkers produce small values from large values. When a random test case fails, a
    shrinker finds the simplest version of the problem. *)

open! Base

type 'a t

(** {2 Basic Shrinkers} *)

(** This shrinker treats a type as atomic, never attempting to produce smaller values. *)
val atomic : 'a. 'a t

include With_basic_types.S with type 'a t := 'a t (** @inline *)

val%template map_t : 'key 'data 'cmp. 'key t -> 'data t -> ('key, 'data, 'cmp) Map.t t
[@@mode p = (nonportable, portable)]

val set_t : 'elt t -> ('elt, 'cmp) Set.t t

val%template map_tree_using_comparator
  : 'key 'data 'cmp.
  comparator:('key, 'cmp) Comparator.t
  -> 'key t
  -> 'data t
  -> ('key, 'data, 'cmp) Map.Using_comparator.Tree.t t
[@@mode p = (nonportable, portable)]

val set_tree_using_comparator
  :  comparator:('elt, 'cmp) Comparator.t
  -> 'elt t
  -> ('elt, 'cmp) Set.Using_comparator.Tree.t t

(** {2 Modifying Shrinkers} *)

[%%template:
[@@@mode.default p = (nonportable, portable)]

val map : 'a 'b. 'a t -> f:('a -> 'b) -> f_inverse:('b -> 'a) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t

(** Filters and maps according to [f], and provides input to [t] via [f_inverse]. Only the
    [f] direction produces options, intentionally. *)
val filter_map : 'a t -> f:('a -> 'b option) -> f_inverse:('b -> 'a) -> 'b t]

(** {2 Shrinkers for Recursive Types} *)

(** Ties the recursive knot to shrink recursive types.

    For example, here is an shrinker for binary trees:

    {[
      let tree_shrinker leaf_shrinker =
        fixed_point (fun self ->
          either leaf_shrinker (both self self)
          |> map
               ~f:(function
                 | First leaf -> `Leaf leaf
                 | Second (l, r) -> `Node (l, r))
               ~f_inverse:(function
                 | `Leaf leaf -> First leaf
                 | `Node (l, r) -> Second (l, r)))
      ;;
    ]} *)
val fixed_point : ('a t -> 'a t) -> 'a t

(** Creates a [t] that forces the lazy argument as necessary. Can be used to tie
    (mutually) recursive knots. *)
val of_lazy : 'a t Lazy.t -> 'a t

(** Like [of_lazy], but for [Portable_lazy.t]. *)
val of_portable_lazy : 'a t Portable_lazy.t -> 'a t

(** {2 Low-level functions}

    Most users will not need to call these. *)

val%template create : 'a. ('a -> 'a Sequence.t) -> 'a t
[@@mode p = (nonportable, portable)]

val shrink : 'a. 'a t -> 'a -> 'a Sequence.t

module Via_thunk : sig
  type 'a thunk := unit -> 'a

  val%template create : 'a. ('a thunk -> 'a thunk Sequence.t) -> 'a t
  [@@mode p = (nonportable, portable)]

  val shrink : 'a. 'a t -> 'a thunk -> 'a thunk Sequence.t

  val%template map
    : 'a 'b.
    'a t -> f:('a thunk -> 'b thunk) -> f_inverse:('b thunk -> 'a thunk) -> 'b t
  [@@mode p = (nonportable, portable)]
end
