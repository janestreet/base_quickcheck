open! Base

type -'a t

val opaque : 'a. 'a t

val%template create : 'a. ('a -> size:int -> hash:Hash.state -> Hash.state) -> 'a t
[@@mode p = (nonportable, portable)]

val observe : 'a. 'a t -> 'a -> size:int -> hash:Hash.state -> Hash.state

module Via_thunk : sig
  type 'a thunk := unit -> 'a

  val%template create
    : 'a.
    ('a thunk -> size:int -> hash:Hash.state -> Hash.state) -> 'a t
  [@@mode p = (nonportable, portable)]

  val observe : 'a. 'a t -> 'a thunk -> size:int -> hash:Hash.state -> Hash.state
end
