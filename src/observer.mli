@@ portable

(** Observers create random functions. {!Generator.fn} creates a random function using an
    observer for the input type and a generator for the output type. *)

open! Base

type (-'a : any) t : value mod contended = 'a Observer0.t

(** {2 Basic Observers} *)

(** Produces an observer that treats all values as equivalent. Random functions generated
    using this observer will be constant with respect to the value(s) it observes. *)
val opaque : (_ : any) t

include With_basic_types.S with type 'a t := 'a t (** @inline *)

(** Produces an observer that generates random inputs for a given function, calls the
    function on them, then observes the corresponding outputs. *)
val fn : 'a Generator.t -> 'b t -> ('a -> 'b) t

val map_t : 'key t -> 'data t -> ('key, 'data, 'cmp) Map.t t
val set_t : 'elt t -> ('elt, 'cmp) Set.t t
val map_tree : 'key t -> 'data t -> ('key, 'data, 'cmp) Map.Using_comparator.Tree.t t
val set_tree : 'elt t -> ('elt, 'cmp) Set.Using_comparator.Tree.t t

(** {2 Observers Based on Hash Functions} *)

(** Creates an observer that just calls a hash function. This is a good default for most
    hashable types not covered by the basic observers above. *)
val%template of_hash_fold : (Hash.state -> 'a -> Hash.state) @ p -> 'a t @ p
[@@mode p = (nonportable, portable)]

(** {2 Modifying Observers} *)

val%template unmap : 'a t @ p -> f:('b -> 'a) @ p -> 'b t @ p
[@@mode p = (nonportable, portable)]

(** {2 Observers for Recursive Types} *)

(** Ties the recursive knot to observe recursive types.

    For example, here is an observer for binary trees:

    {[
      let tree_observer leaf_observer =
        fixed_point (fun self ->
          either leaf_observer (both self self)
          |> unmap ~f:(function
            | `Leaf leaf -> First leaf
            | `Node (l, r) -> Second (l, r)))
      ;;
    ]} *)
val fixed_point : ('a t -> 'a t) -> 'a t

(** Creates a [t] that forces the lazy argument as necessary. Can be used to tie
    (mutually) recursive knots. *)
val of_lazy : 'a t Lazy.t -> 'a t

(** {2 Low-Level functions}

    Most users do not need to call these functions. *)

val%template create : ('a -> size:int -> hash:Hash.state -> Hash.state) @ p -> 'a t @ p
[@@mode p = (nonportable, portable)]

val observe : 'a t -> 'a -> size:int -> hash:Hash.state -> Hash.state

module Via_thunk : sig
  type ('a : any) thunk := unit -> 'a

  val%template create
    : ('a : any).
    ('a thunk -> size:int -> hash:Hash.state -> Hash.state) @ p -> 'a t @ p
  [@@mode p = (nonportable, portable)]

  val observe : ('a : any). 'a t -> 'a thunk -> size:int -> hash:Hash.state -> Hash.state

  val%template unmap
    : ('a : any) ('b : any).
    'a t @ p -> f:('b thunk -> 'a thunk) @ p -> 'b t @ p
  [@@mode p = (nonportable, portable)]
end
