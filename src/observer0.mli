@@ portable

open! Base

type (-'a : any) t : value mod contended

val opaque : ('a : any). 'a t

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
end
