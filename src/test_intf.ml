open! Base

module type S = sig
  type t [@@deriving sexp_of]

  val quickcheck_generator : t Generator.t
  val quickcheck_shrinker  : t Shrinker.t
end

module type Test = sig
  module type S = S

  module Config : sig
    module Seed : sig
      type t =
        | Nondeterministic
        | Deterministic of string
      [@@deriving sexp_of]
    end

    type t = {

      (** [seed] is used to initialize the pseudo-random state before running tests of a
          property. *)
      seed : Seed.t;

      (** [test_count] determines how many random values to test a property with. *)
      test_count : int;

      (** [shrink_count] determines the maximum number of attempts to find a smaller
          version of a value that fails a test. *)
      shrink_count : int;

      (** [sizes] determines the progression of value sizes to generate while testing.
          Testing fails if [sizes] is not of length at least [test_count]. *)
      sizes : int Sequence.t;

    } [@@deriving fields, sexp_of]
  end

  (** Defaults to a deterministic seed, [shrink_count] and [test_count] of 10_000 each,
      and sizes ranging from 0 to 30. *)
  val default_config : Config.t

  (** Tests the property [f], failing if it raises or returns [Error _]. Tests [f] first
      with any [examples], then with values from the given generator. Only random values
      count toward the [test_count] total, not values from [examples]. *)
  val run
    :  f         : ('a -> unit Or_error.t)
    -> ?config   : Config.t (** defaults to [default_config] *)
    -> ?examples : 'a list  (** defaults to the empty list *)
    -> (module S with type t = 'a)
    -> unit Or_error.t

  (** Like [run], but raises on failure. *)
  val run_exn
    :  f         : ('a -> unit)
    -> ?config   : Config.t (** defaults to [default_config] *)
    -> ?examples : 'a list  (** defaults to the empty list *)
    -> (module S with type t = 'a)
    -> unit

  (** Calls [f] with the sequence of values that [run] would get in the same
      configuration. *)
  val with_sample
    :  f : ('a Sequence.t -> unit Or_error.t)
    -> ?config   : Config.t (** defaults to [default_config] *)
    -> ?examples : 'a list  (** defaults to the empty list *)
    -> 'a Generator.t
    -> unit Or_error.t

  (** Like [with_sample], but raises on failure. *)
  val with_sample_exn
    :  f : ('a Sequence.t -> unit)
    -> ?config   : Config.t (** defaults to [default_config] *)
    -> ?examples : 'a list  (** defaults to the empty list *)
    -> 'a Generator.t
    -> unit
end
