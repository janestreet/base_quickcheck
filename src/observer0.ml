open! Base

type 'a thunk = unit -> 'a
type 'a t = 'a thunk -> size:int -> hash:Hash.state -> Hash.state

module Via_thunk = struct
  let check_size ~size =
    if size < 0
    then raise_s [%message "Base_quickcheck.Observer.observe: size < 0" (size : int)]
  ;;

  let%template create f : _ t = f [@@mode __ = (nonportable, portable)]

  let observe t thunk ~size ~hash =
    check_size ~size;
    t thunk ~size ~hash
  ;;
end

let%template create f =
  (Via_thunk.create [@mode p]) (fun thunk ~size ~hash -> f (thunk ()) ~size ~hash)
[@@mode p = (nonportable, portable)]
;;

let observe t x ~size ~hash = Via_thunk.observe t (fun () -> x) ~size ~hash
let opaque _ ~size:_ ~hash = hash
