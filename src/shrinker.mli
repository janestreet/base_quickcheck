@@ portable

(** Shrinkers produce small values from large values. When a random test case fails, a
    shrinker finds the simplest version of the problem. *)

open! Base

type ('a : any) t : value mod contended

(** {2 Basic Shrinkers} *)

(** This shrinker treats a type as atomic, never attempting to produce smaller values. *)
val atomic : ('a : any). 'a t

include With_basic_types.S with type ('a : value_or_null) t := 'a t (** @inline *)

val%template map_t
  : 'key 'data ('cmp : value mod p).
  'key t @ p -> 'data t @ p -> ('key, 'data, 'cmp) Map.t t @ p
[@@mode p = (nonportable, portable)]

val set_t : 'elt t -> ('elt, 'cmp) Set.t t

val%template map_tree_using_comparator
  : 'key 'data ('cmp : value mod p).
  comparator:('key, 'cmp) Comparator.t
  -> 'key t @ p
  -> 'data t @ p
  -> ('key, 'data, 'cmp) Map.Using_comparator.Tree.t t @ p
[@@mode p = (nonportable, portable)]

val set_tree_using_comparator
  :  comparator:('elt, 'cmp) Comparator.t
  -> 'elt t
  -> ('elt, 'cmp) Set.Using_comparator.Tree.t t

(** {2 Modifying Shrinkers} *)

[%%template:
[@@@mode.default p = (nonportable, portable)]

val map
  : ('a : value_or_null) ('b : value_or_null).
  'a t @ p -> f:('a -> 'b) @ p -> f_inverse:('b -> 'a) @ p -> 'b t @ p

val filter : 'a t @ p -> f:('a -> bool) @ p -> 'a t @ p

(** Filters and maps according to [f], and provides input to [t] via [f_inverse]. Only the
    [f] direction produces options, intentionally. *)
val filter_map
  :  'a t @ p
  -> f:('a -> 'b option) @ p
  -> f_inverse:('b -> 'a) @ p
  -> 'b t @ p]

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
val of_portable_lazy : 'a t Portable_lazy.t -> 'a t @ portable

(** {2 Low-level functions}

    Most users will not need to call these. *)

val%template create : ('a : value_or_null). ('a -> 'a Sequence.t) @ p -> 'a t @ p
[@@mode p = (nonportable, portable)]

val shrink : ('a : value_or_null). 'a t -> 'a -> 'a Sequence.t

module Via_thunk : sig
  type ('a : any) thunk := unit -> 'a

  val%template create : ('a : any). ('a thunk -> 'a thunk Sequence.t) @ p -> 'a t @ p
  [@@mode p = (nonportable, portable)]

  val shrink : ('a : any). 'a t -> 'a thunk -> 'a thunk Sequence.t

  val%template map
    : ('a : any) ('b : any).
    'a t @ p
    -> f:('a thunk -> 'b thunk) @ p
    -> f_inverse:('b thunk -> 'a thunk) @ p
    -> 'b t @ p
  [@@mode p = (nonportable, portable)]
end
