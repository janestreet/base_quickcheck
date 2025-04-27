@@ portable

(** Provides default generators, observers, and shrinkers for built-in types. Follows
    ppx_quickcheck naming conventions. *)

open! Base

val quickcheck_generator_unit : unit Generator.t
val quickcheck_generator_bool : bool Generator.t
val quickcheck_generator_char : char Generator.t
val quickcheck_generator_string : string Generator.t
val quickcheck_generator_bytes : bytes Generator.t
val quickcheck_generator_int : int Generator.t
val quickcheck_generator_int32 : int32 Generator.t
val quickcheck_generator_int64 : int64 Generator.t
val quickcheck_generator_nativeint : nativeint Generator.t
val quickcheck_generator_float : float Generator.t
val quickcheck_observer_unit : unit Observer.t
val quickcheck_observer_bool : bool Observer.t
val quickcheck_observer_char : char Observer.t
val quickcheck_observer_string : string Observer.t
val quickcheck_observer_bytes : bytes Observer.t
val quickcheck_observer_int : int Observer.t
val quickcheck_observer_int32 : int32 Observer.t
val quickcheck_observer_int64 : int64 Observer.t
val quickcheck_observer_nativeint : nativeint Observer.t
val quickcheck_observer_float : float Observer.t
val quickcheck_shrinker_unit : unit Shrinker.t
val quickcheck_shrinker_bool : bool Shrinker.t
val quickcheck_shrinker_char : char Shrinker.t
val quickcheck_shrinker_string : string Shrinker.t
val quickcheck_shrinker_bytes : bytes Shrinker.t
val quickcheck_shrinker_int : int Shrinker.t
val quickcheck_shrinker_int32 : int32 Shrinker.t
val quickcheck_shrinker_int64 : int64 Shrinker.t
val quickcheck_shrinker_nativeint : nativeint Shrinker.t
val quickcheck_shrinker_float : float Shrinker.t

[%%template:
[@@@mode.default p = (portable, nonportable)]

val quickcheck_generator_option : 'a Generator.t @ p -> 'a option Generator.t @ p
val quickcheck_generator_list : 'a Generator.t @ p -> 'a list Generator.t @ p
val quickcheck_generator_array : 'a Generator.t @ p -> 'a array Generator.t @ p
val quickcheck_generator_ref : 'a Generator.t @ p -> 'a ref Generator.t @ p
val quickcheck_generator_lazy_t : 'a Generator.t @ p -> 'a Lazy.t Generator.t @ p
val quickcheck_observer_option : 'a Observer.t @ p -> 'a option Observer.t @ p
val quickcheck_observer_list : 'a Observer.t @ p -> 'a list Observer.t @ p
val quickcheck_observer_array : 'a Observer.t @ p -> 'a array Observer.t @ p
val quickcheck_observer_ref : 'a Observer.t @ p -> 'a ref Observer.t @ p
val quickcheck_observer_lazy_t : 'a Observer.t @ p -> 'a Lazy.t Observer.t @ p
val quickcheck_shrinker_option : 'a Shrinker.t @ p -> 'a option Shrinker.t @ p
val quickcheck_shrinker_list : 'a Shrinker.t @ p -> 'a list Shrinker.t @ p
val quickcheck_shrinker_array : 'a Shrinker.t @ p -> 'a array Shrinker.t @ p
val quickcheck_shrinker_ref : 'a Shrinker.t @ p -> 'a ref Shrinker.t @ p
val quickcheck_shrinker_lazy_t : 'a Shrinker.t @ p -> 'a Lazy.t Shrinker.t @ p]
