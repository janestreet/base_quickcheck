open! Base

module%template Maybe_portable = struct
  let[@mode nonportable] wrap = Fn.id
  let[@mode portable] wrap = Modes.Portable.wrap
  let[@mode nonportable] unwrap = Fn.id
  let[@mode portable] unwrap = Modes.Portable.unwrap
  let[@mode nonportable] wrap_list = Fn.id
  let[@mode portable] wrap_list = Modes.Portable.wrap_list
  let[@mode nonportable] unwrap_list = Fn.id
  let[@mode portable] unwrap_list = Modes.Portable.unwrap_list
  let[@mode nonportable] unwrap_iarray = Fn.id
  let[@mode portable] unwrap_iarray = Modes.Portable.unwrap_iarray
end

type 'a thunk = unit -> 'a

module T : sig
  type +'a t

  val%template create : (size:int -> random:Splittable_random.t -> 'a) -> 'a t
  [@@mode p = (nonportable, portable)]

  val generate : 'a t -> size:int -> random:Splittable_random.t -> 'a

  module Via_thunk : sig
    val%template create : 'a. (size:int -> random:Splittable_random.t -> 'a thunk) -> 'a t
    [@@mode p = (nonportable, portable)]

    val generate : 'a. 'a t -> size:int -> random:Splittable_random.t -> 'a thunk
  end
end = struct
  type 'a t = size:int -> random:Splittable_random.t -> unit -> 'a

  let check_size ~size =
    if size < 0
    then raise_s [%message "Base_quickcheck.Generator.generate: size < 0" (size : int)]
  ;;

  module Via_thunk = struct
    let%template create f = f [@@mode __ = (nonportable, portable)]

    let generate t ~size ~random =
      check_size ~size;
      t ~size ~random
    ;;
  end

  let%template create f =
    (Via_thunk.create [@mode p]) (fun ~size ~random ->
      let x = f ~size ~random in
      fun () -> x)
  [@@mode p = (nonportable, portable)]
  ;;

  let generate t ~size ~random = Via_thunk.generate t ~size ~random ()
end

include T

module Via_thunk = struct
  include Via_thunk

  let%template map t ~f =
    (create [@mode p]) (fun ~size ~random -> f (generate t ~size ~random))
  [@@mode p = (nonportable, portable)]
  ;;
end

let%template size = (create [@mode portable]) (fun ~size ~random:_ -> size)

let%template return (type a) (x : a) = (create [@mode p]) (fun ~size:_ ~random:_ -> x)
[@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]
;;

[%%template
[@@@mode p = (nonportable, portable)]

open struct
  let create = (create [@mode p])
end

[@@@mode.default p]

let fn dom rng =
  create (fun ~size ~random ->
    let random = Splittable_random.split random in
    fun x ->
      let hash = Observer0.observe dom x ~size ~hash:(Hash.alloc ()) in
      let random = Splittable_random.copy random in
      Splittable_random.perturb random (Hash.get_hash_value hash);
      generate rng ~size ~random)
;;

let with_size t ~size = create (fun ~size:_ ~random -> generate t ~size ~random)

let perturb t salt =
  create (fun ~size ~random ->
    Splittable_random.perturb random salt;
    generate t ~size ~random)
;;

let filter_map t ~f =
  let rec loop ~size ~random =
    let x = generate t ~size ~random in
    match f x with
    | Some y -> y
    | None -> loop ~size:(size + 1) ~random
  in
  create loop
;;

let filter t ~f = (filter_map [@mode p]) t ~f:(fun x -> if f x then Some x else None)
let map t ~f = create (fun ~size ~random -> f (generate t ~size ~random))

let apply tf tx =
  create (fun ~size ~random ->
    let f = generate tf ~size ~random in
    let x = generate tx ~size ~random in
    f x)
;;

let bind t ~f =
  create (fun ~size ~random ->
    let x = generate t ~size ~random in
    generate (f x) ~size ~random)
;;

let all (list : _ t list) =
  create (fun ~size ~random -> List.map list ~f:(generate ~size ~random))
;;

let all_unit (list : unit t list) =
  create (fun ~size ~random -> List.iter list ~f:(generate ~size ~random))
;;

let map2 ta tb ~f =
  create (fun ~size ~random ->
    let a = generate ta ~size ~random in
    let b = generate tb ~size ~random in
    f a b)
;;

let map3 ta tb tc ~f =
  create (fun ~size ~random ->
    let a = generate ta ~size ~random in
    let b = generate tb ~size ~random in
    let c = generate tc ~size ~random in
    f a b c)
;;

let both a b = (map2 [@mode p]) a b ~f:(fun x y -> x, y)
let join t = (bind [@mode p]) t ~f:Fn.id
let ignore_m t = (map [@mode p]) t ~f:ignore]

let (_ : _) = apply__portable, all_unit__portable, map3__portable, ignore_m__portable
let ( >>| ) t f = map t ~f
let ( >>= ) t f = bind t ~f
let ( <*> ) = apply
let ( <* ) ta tb = map2 ta tb ~f:(fun a () -> a)
let ( *> ) ta tb = map2 ta tb ~f:(fun () b -> b)

module Applicative_infix = struct
  let ( >>| ) = ( >>| )
  let ( <*> ) = ( <*> )
  let ( <* ) = ( <* )
  let ( *> ) = ( *> )
end

module Monad_infix = struct
  let ( >>| ) = ( >>| )
  let ( >>= ) = ( >>= )
end

let%template of_list (type a) (list : a list) =
  if List.is_empty list
  then Error.raise_s [%message "Base_quickcheck.Generator.of_list: empty list"];
  let list = list |> (Maybe_portable.wrap_list [@mode p]) in
  let iarray = Iarray.of_list list in
  let iarray = iarray |> (Maybe_portable.unwrap_iarray [@mode p]) in
  let lo = 0 in
  let hi = Iarray.length iarray - 1 in
  (create [@mode p]) (fun ~size:_ ~random ->
    let index = Splittable_random.int random ~lo ~hi in
    iarray.:(index))
[@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]
;;

let%template union list = (join [@mode p]) ((of_list [@mode p]) list)
[@@mode p = (nonportable, portable)]
;;

module Syntax = struct
  open struct
    module Open_on_rhs = struct end

    module%template [@mode p = portable] Open_on_rhs = struct
      let map = (map [@mode p])
      let ( >>| ) t f = (map [@mode p]) t ~f
      let of_list = (of_list [@mode p])
      let union = (union [@mode p])
      let filter = (filter [@mode p])
    end
  end

  module%template [@mode p = (nonportable, portable)] Let_syntax = struct
    let return = (return [@mode p])
    let ( >>= ) t f = (bind [@mode p]) t ~f
    let ( >>| ) t f = (map [@mode p]) t ~f

    module Let_syntax = struct
      let return = (return [@mode p])
      let bind = (bind [@mode p])
      let map = (map [@mode p])
      let both = (both [@mode p])

      module%template Open_on_rhs = Open_on_rhs [@mode p]
    end
  end
end

module Portable = struct
  module%template Let_syntax = Syntax.Let_syntax [@mode portable]
end

include Syntax
open Syntax.Let_syntax

let total_weight alist =
  if List.is_empty alist
  then Error.raise_s [%message "Base_quickcheck.Generator.of_weighted_list: empty list"];
  let sum =
    List.fold alist ~init:0. ~f:(fun acc (weight, _) ->
      if not (Float.is_finite weight)
      then
        Error.raise_s
          [%message
            "Base_quickcheck.Generator.of_weighted_list: weight is not finite"
              (weight : float)];
      if Float.( < ) weight 0.
      then
        Error.raise_s
          [%message
            "Base_quickcheck.Generator.of_weighted_list: weight is negative"
              (weight : float)];
      acc +. weight)
  in
  if Float.( <= ) sum 0.
  then
    Error.raise_s
      [%message "Base_quickcheck.Generator.of_weighted_list: total weight is zero"];
  sum
;;

let weighted_find alist choice =
  let rec go alist acc =
    match alist with
    | [] -> assert false
    | (w, x) :: alist -> if Float.( <= ) acc w then x else go alist (acc -. w)
  in
  go alist choice
;;

include struct
  let of_weighted_list alist =
    let total_weight = total_weight alist in
    create (fun ~size:_ ~random ->
      let choice = Splittable_random.float random ~lo:0. ~hi:total_weight in
      weighted_find alist choice)
  ;;

  let%template of_weighted_list (type a) (alist : (float, a) List.Assoc.t) =
    let total_weight = total_weight alist in
    (create [@mode portable]) (fun ~size:_ ~random ->
      let choice = Splittable_random.float random ~lo:0. ~hi:total_weight in
      weighted_find alist choice)
  [@@mode portable]
  ;;
end

let%template weighted_union alist = (join [@mode p]) ((of_weighted_list [@mode p]) alist)
[@@mode p = (nonportable, portable)]
;;

let of_lazy lazy_t = create (fun ~size ~random -> generate (force lazy_t) ~size ~random)

let%template of_portable_lazy portable_lazy_t =
  (create [@mode portable]) (fun ~size ~random ->
    generate (Portable_lazy.force portable_lazy_t) ~size ~random)
;;

include struct
  let fixed_point of_generator =
    let rec lazy_t = lazy (of_generator (of_lazy lazy_t)) in
    force lazy_t
  ;;

  let%template fixed_point of_generator =
    let portable_lazy_t =
      Portable_lazy.from_fun_fixed (fun portable_lazy_t ->
        of_generator (of_portable_lazy portable_lazy_t))
    in
    Portable_lazy.force portable_lazy_t
  [@@mode p = portable]
  ;;
end

let%template weighted_recursive_union (nonrec_list : (float * 'a t) list) ~f =
  let open Syntax.Let_syntax [@mode p] in
  (fixed_point [@mode p]) (fun self ->
    let rec_list : (float * 'a t) list =
      (Maybe_portable.wrap_list [@mode p]) (f self)
      |> List.map ~f:(fun elem ->
        let w, t = (Maybe_portable.unwrap [@mode p]) elem in
        (Maybe_portable.wrap [@mode p])
          ( w
          , let%bind n = size in
            with_size ~size:(n - 1) t ))
      |> (Maybe_portable.unwrap_list [@mode p])
    in
    if List.is_empty nonrec_list || List.is_empty rec_list
    then
      raise_s
        [%message
          "Base_quickcheck.Generator.weighted_recursive_union: lists must be non-empty"];
    let nonrec_gen = (weighted_union [@mode p]) nonrec_list in
    let rec_gen =
      (weighted_union [@mode p])
        ((Maybe_portable.wrap_list [@mode p]) nonrec_list
         @ (Maybe_portable.wrap_list [@mode p]) rec_list
         |> (Maybe_portable.unwrap_list [@mode p]))
    in
    match%bind size with
    | 0 -> nonrec_gen
    | _ -> rec_gen)
[@@mode p = (nonportable, portable)]
;;

let recursive_union nonrec_list ~f =
  let weighted list = List.map list ~f:(fun t -> 1., t) in
  weighted_recursive_union (weighted nonrec_list) ~f:(fun self -> weighted (f self))
;;

let%template sizes ?(min_length = 0) ?(max_length = Int.max_value) () =
  (create [@mode portable]) (fun ~size ~random ->
    assert (min_length <= max_length);
    let upper_bound = min_length + size in
    let max_length =
      if upper_bound >= min_length (* guard against overflow *)
      then min max_length upper_bound
      else max_length
    in
    (* pick a length, weighted low so that most of the size is spent on elements *)
    let len = Splittable_random.Log_uniform.int random ~lo:min_length ~hi:max_length in
    (* if there are no elements return an empty array, otherwise return a non-empty array
       with the size distributed among the elements *)
    if len = 0
    then []
    else (
      let sizes = Array.init len ~f:(fun _ -> 0) in
      let remaining = size - (len - min_length) in
      let max_index = len - 1 in
      for _ = 1 to remaining do
        (* pick an index, weighted low so that we see unbalanced distributions often *)
        let index = Splittable_random.Log_uniform.int random ~lo:0 ~hi:max_index in
        sizes.(index) <- sizes.(index) + 1
      done;
      (* permute the array so that no index is favored over another *)
      for i = 0 to max_index - 1 do
        let j = Splittable_random.int random ~lo:i ~hi:max_index in
        Array.swap sizes i j
      done;
      assert (Array.sum (module Int) sizes ~f:Fn.id + (len - min_length) = size);
      Array.to_list sizes))
;;

let%template unit = (return [@mode portable]) ()

let%template bool =
  (create [@mode portable]) (fun ~size:_ ~random -> Splittable_random.bool random)
;;

[%%template
[@@@mode.default p = (nonportable, portable)]

let option value_t =
  (union [@mode p])
    [ (map [@mode p]) unit ~f:(fun () -> None); (map [@mode p]) value_t ~f:Option.return ]
;;

let either fst_t snd_t =
  (union [@mode p])
    [ (map [@mode p]) fst_t ~f:Either.first; (map [@mode p]) snd_t ~f:Either.second ]
;;

let result ok_t err_t =
  (map [@mode p]) ((either [@mode p]) ok_t err_t) ~f:Result.of_either
;;

let list_generic ?min_length ?max_length elt_gen =
  let open Syntax.Let_syntax [@mode p] in
  let%bind sizes = sizes ?min_length ?max_length () in
  List.map sizes ~f:(fun size ->
    (with_size [@mode p]) ~size elt_gen |> (Maybe_portable.wrap [@mode p]))
  |> (Maybe_portable.unwrap_list [@mode p])
  |> (all [@mode p])
;;

let list elt_gen = (list_generic [@mode p]) elt_gen
let list_non_empty elt_gen = (list_generic [@mode p]) ~min_length:1 elt_gen

let list_with_length elt_gen ~length =
  (list_generic [@mode p]) ~min_length:length ~max_length:length elt_gen
;;]

let list_filtered elts =
  let elts = Array.of_list elts in
  let length_of_input = Array.length elts in
  create (fun ~size:_ ~random ->
    let length_of_output = Splittable_random.int random ~lo:0 ~hi:length_of_input in
    let indices = Array.init length_of_input ~f:Fn.id in
    (* Choose [length_of_output] random values in the prefix of [indices]. *)
    for i = 0 to length_of_output - 1 do
      let j = Splittable_random.int random ~lo:i ~hi:(length_of_input - 1) in
      Array.swap indices i j
    done;
    (* Sort the chosen indices because we don't want to reorder them. *)
    Array.sort indices ~pos:0 ~len:length_of_output ~compare:Int.compare;
    (* Return the chosen elements. *)
    List.init length_of_output ~f:(fun i -> elts.(indices.(i))))
;;

let list_permutations list =
  create (fun ~size:_ ~random ->
    let array = Array.of_list list in
    for i = 1 to Array.length array - 1 do
      let j = Splittable_random.int random ~lo:0 ~hi:i in
      Array.swap array i j
    done;
    Array.to_list array)
;;

let fold_until ?min_length ?max_length ~init ~f ~finish () =
  let%bind sizes = sizes ?min_length ?max_length () in
  let rec loop acc sizes =
    match sizes with
    | [] -> return (finish acc)
    | size :: sizes ->
      (match%bind with_size ~size (f acc) with
       | (Stop acc : _ Continue_or_stop.t) -> return acc
       | Continue acc -> loop acc sizes)
  in
  loop init sizes
;;

[%%template
[@@@mode.default p = (nonportable, portable)]

let array t = (map [@mode p]) ((list [@mode p]) t) ~f:Array.of_list
let ref t = (map [@mode p]) t ~f:Ref.create
let lazy_t t = (map [@mode p]) t ~f:Lazy.from_val]

let%template char_uniform_inclusive lo hi =
  (create [@mode portable]) (fun ~size:_ ~random ->
    Splittable_random.int random ~lo:(Char.to_int lo) ~hi:(Char.to_int hi)
    |> Char.unsafe_of_int)
;;

let char_uppercase = char_uniform_inclusive 'A' 'Z'
let char_lowercase = char_uniform_inclusive 'a' 'z'
let char_digit = char_uniform_inclusive '0' '9'
let char_print_uniform = char_uniform_inclusive ' ' '~'
let char_uniform = char_uniform_inclusive Char.min_value Char.max_value
let%template char_alpha = (union [@mode portable]) [ char_lowercase; char_uppercase ]

let%template char_alphanum =
  (weighted_union [@mode portable])
    (* Most people probably expect this to be a uniform distribution, not weighted
       toward digits like we would get with [union] (since there are fewer digits than
       letters). *)
    [ 52., char_alpha; 10., char_digit ]
;;

let%template char_whitespace =
  (of_list [@mode portable]) (List.filter Char.all ~f:Char.is_whitespace)
;;

let%template char_print =
  (weighted_union [@mode portable]) [ 10., char_alphanum; 1., char_print_uniform ]
;;

let%template char =
  (weighted_union [@mode portable])
    [ 100., char_print
    ; 10., char_uniform
    ; 1., (return [@mode portable]) Char.min_value
    ; 1., (return [@mode portable]) Char.max_value
    ]
;;

(* Produces a number from 0 or 1 to size + 1, weighted high. We have found this
   distribution empirically useful for string lengths. *)
let%template small_int ~allow_zero =
  (create [@mode portable]) (fun ~size ~random ->
    let lower_bound = if allow_zero then 0 else 1 in
    let upper_bound = size + 1 in
    let weighted_low =
      Splittable_random.Log_uniform.int random ~lo:0 ~hi:(upper_bound - lower_bound)
    in
    let weighted_high = upper_bound - weighted_low in
    weighted_high)
;;

let small_positive_or_zero_int = small_int ~allow_zero:true
let small_strictly_positive_int = small_int ~allow_zero:false

module type Int_with_random = sig
  type t

  include Int.S with type t := t

  val uniform : Splittable_random.t -> lo:t -> hi:t -> t
  val log_uniform : Splittable_random.t -> lo:t -> hi:t -> t
end

module%template For_integer (Integer : Int_with_random) = struct
  let geometric (lo : Integer.t) ~p =
    if Float.equal p 1.
    then (return [@mode portable]) lo
    else if Float.equal p 0.
    then (return [@mode portable]) Integer.max_value
    else if Float.( < ) p 0. || Float.( > ) p 1. || Float.is_nan p
    then (
      match
        raise_s [%message "geometric distribution: p must be between 0 and 1" (p : float)]
      with
      | (_ : Nothing.t) -> .)
    else (
      (* We start with a uniform distribution. We convert to exponential distribution
         using [log]. We convert to geometric with [round_down]. Then we bounds check and
         return. *)
      let denominator = Float.log1p (-.p) in
      (create [@mode portable]) (fun ~size:_ ~random ->
        let uniform = Splittable_random.unit_float random in
        let exponential = Float.log uniform /. denominator in
        let float = Float.round_down exponential in
        match Integer.of_float float with
        | exception Invalid_argument _ -> Integer.max_value
        | int ->
          let int = Integer.( + ) lo int in
          if Integer.( < ) int lo then Integer.max_value else int))
  ;;

  let uniform_inclusive lo hi =
    (create [@mode portable]) (fun ~size:_ ~random -> Integer.uniform random ~lo ~hi)
  ;;

  let log_uniform_inclusive lo hi =
    (create [@mode portable]) (fun ~size:_ ~random -> Integer.log_uniform random ~lo ~hi)
  ;;

  let non_uniform f lo hi =
    (weighted_union [@mode portable])
      [ 0.05, (return [@mode portable]) lo
      ; 0.05, (return [@mode portable]) hi
      ; 0.9, f lo hi
      ]
  ;;

  let inclusive = non_uniform uniform_inclusive
  let log_inclusive = non_uniform log_uniform_inclusive
  let uniform_all = uniform_inclusive Integer.min_value Integer.max_value

  let all =
    let%map.Portable negative = bool
    and magnitude = log_inclusive Integer.zero Integer.max_value in
    if negative then Integer.bit_not magnitude else magnitude
  ;;
end

module For_int = For_integer (struct
    include Int

    let uniform = Splittable_random.int
    let log_uniform = Splittable_random.Log_uniform.int
  end)

let int = For_int.all
let int_uniform = For_int.uniform_all
let int_inclusive = For_int.inclusive
let int_uniform_inclusive = For_int.uniform_inclusive
let int_log_inclusive = For_int.log_inclusive
let int_log_uniform_inclusive = For_int.log_uniform_inclusive
let int_geometric = For_int.geometric

module For_int32 = For_integer (struct
    include Int32

    let uniform = Splittable_random.int32
    let log_uniform = Splittable_random.Log_uniform.int32
  end)

let int32 = For_int32.all
let int32_uniform = For_int32.uniform_all
let int32_inclusive = For_int32.inclusive
let int32_uniform_inclusive = For_int32.uniform_inclusive
let int32_log_inclusive = For_int32.log_inclusive
let int32_log_uniform_inclusive = For_int32.log_uniform_inclusive
let int32_geometric = For_int32.geometric

module For_int63 = For_integer (struct
    include Int63

    let uniform = Splittable_random.int63
    let log_uniform = Splittable_random.Log_uniform.int63
  end)

let int63 = For_int63.all
let int63_uniform = For_int63.uniform_all
let int63_inclusive = For_int63.inclusive
let int63_uniform_inclusive = For_int63.uniform_inclusive
let int63_log_inclusive = For_int63.log_inclusive
let int63_log_uniform_inclusive = For_int63.log_uniform_inclusive
let int63_geometric = For_int63.geometric

module For_int64 = For_integer (struct
    include Int64

    let uniform = Splittable_random.int64
    let log_uniform = Splittable_random.Log_uniform.int64
  end)

let int64 = For_int64.all
let int64_uniform = For_int64.uniform_all
let int64_inclusive = For_int64.inclusive
let int64_uniform_inclusive = For_int64.uniform_inclusive
let int64_log_inclusive = For_int64.log_inclusive
let int64_log_uniform_inclusive = For_int64.log_uniform_inclusive
let int64_geometric = For_int64.geometric

module For_nativeint = For_integer (struct
    include Nativeint

    let uniform = Splittable_random.nativeint
    let log_uniform = Splittable_random.Log_uniform.nativeint
  end)

let nativeint = For_nativeint.all
let nativeint_uniform = For_nativeint.uniform_all
let nativeint_inclusive = For_nativeint.inclusive
let nativeint_uniform_inclusive = For_nativeint.uniform_inclusive
let nativeint_log_inclusive = For_nativeint.log_inclusive
let nativeint_log_uniform_inclusive = For_nativeint.log_uniform_inclusive
let nativeint_geometric = For_nativeint.geometric
let float_zero_exponent = Float.ieee_exponent 0.
let float_zero_mantissa = Float.ieee_mantissa 0.

let float_max_positive_subnormal_value =
  Float.one_ulp `Down Float.min_positive_normal_value
;;

let float_subnormal_exponent = Float.ieee_exponent Float.min_positive_subnormal_value
let float_min_subnormal_mantissa = Float.ieee_mantissa Float.min_positive_subnormal_value
let float_max_subnormal_mantissa = Float.ieee_mantissa float_max_positive_subnormal_value
let float_max_positive_normal_value = Float.max_finite_value
let float_min_normal_exponent = Float.ieee_exponent Float.min_positive_normal_value
let float_max_normal_exponent = Float.ieee_exponent float_max_positive_normal_value
let float_max_normal_mantissa = Float.ieee_mantissa float_max_positive_normal_value
let float_inf_exponent = Float.ieee_exponent Float.infinity
let float_inf_mantissa = Float.ieee_mantissa Float.infinity
let float_nan_exponent = Float.ieee_exponent Float.nan
let float_min_nan_mantissa = Int63.succ float_inf_mantissa
let float_max_nan_mantissa = float_max_normal_mantissa
let float_num_mantissa_bits = 52

(* We weight mantissas so that "integer-like" values, and values with only a few digits
   past the decimal, are reasonably common. *)
let float_normal_mantissa =
  let%bind.Portable num_bits = For_int.uniform_inclusive 0 float_num_mantissa_bits in
  let%map.Portable bits =
    For_int63.inclusive Int63.zero (Int63.pred (Int63.shift_left Int63.one num_bits))
  in
  Int63.shift_left bits (Int.( - ) float_num_mantissa_bits num_bits)
;;

let float_exponent_weighted_low lower_bound upper_bound =
  let%map.Portable offset = For_int.log_inclusive 0 (Int.( - ) upper_bound lower_bound) in
  Int.( + ) lower_bound offset
;;

let float_exponent_weighted_high lower_bound upper_bound =
  let%map.Portable offset = For_int.log_inclusive 0 (Int.( - ) upper_bound lower_bound) in
  Int.( - ) upper_bound offset
;;

(* We weight exponents such that values near 1 are more likely. *)
let%template float_exponent =
  let midpoint = Float.ieee_exponent 1. in
  (union [@mode portable])
    [ float_exponent_weighted_high float_min_normal_exponent midpoint
    ; float_exponent_weighted_low midpoint float_max_normal_exponent
    ]
;;

let float_zero =
  let%map.Portable negative = bool in
  Float.create_ieee_exn
    ~negative
    ~exponent:float_zero_exponent
    ~mantissa:float_zero_mantissa
;;

let%template float_subnormal =
  let%map.Portable negative = bool
  and exponent = (return [@mode portable]) float_subnormal_exponent
  and mantissa =
    For_int63.log_inclusive float_min_subnormal_mantissa float_max_subnormal_mantissa
  in
  Float.create_ieee_exn ~negative ~exponent ~mantissa
;;

let float_normal =
  let%map.Portable negative = bool
  and exponent = float_exponent
  and mantissa = float_normal_mantissa in
  Float.create_ieee_exn ~negative ~exponent ~mantissa
;;

let float_infinite =
  let%map.Portable negative = bool in
  Float.create_ieee_exn
    ~negative
    ~exponent:float_inf_exponent
    ~mantissa:float_inf_mantissa
;;

let%template float_nan =
  let%map.Portable negative = bool
  and exponent = (return [@mode portable]) float_nan_exponent
  and mantissa = For_int63.inclusive float_min_nan_mantissa float_max_nan_mantissa in
  Float.create_ieee_exn ~negative ~exponent ~mantissa
;;

let float_of_class c =
  match (c : Float.Class.t) with
  | Zero -> float_zero
  | Subnormal -> float_subnormal
  | Normal -> float_normal
  | Infinite -> float_infinite
  | Nan -> float_nan
;;

let float_weight_of_class c =
  match (c : Float.Class.t) with
  | Zero -> 1.
  | Subnormal -> 10.
  | Normal -> 100.
  | Infinite -> 1.
  | Nan -> 1.
;;

let%template float_matching_classes filter =
  List.filter_map Float.Class.all ~f:(fun c ->
    if filter c
    then Some { Modes.Portable.portable = float_weight_of_class c, float_of_class c }
    else None)
  |> Modes.Portable.unwrap_list
  |> (weighted_union [@mode portable])
;;

let float_finite =
  float_matching_classes (function
    | Zero | Subnormal | Normal -> true
    | Infinite | Nan -> false)
;;

let float_without_nan =
  float_matching_classes (function
    | Zero | Subnormal | Normal | Infinite -> true
    | Nan -> false)
;;

let float = float_matching_classes (fun _ -> true)

let float_finite_non_zero =
  float_matching_classes (function
    | Subnormal | Normal -> true
    | Zero | Infinite | Nan -> false)
;;

let float_strictly_positive =
  let%map.Portable t = float_finite_non_zero in
  Float.abs t
;;

let float_strictly_negative =
  let%map.Portable t = float_finite_non_zero in
  ~-.(Float.abs t)
;;

let float_positive_or_zero =
  let%map.Portable t = float_finite in
  Float.abs t
;;

let float_negative_or_zero =
  let%map.Portable t = float_finite in
  ~-.(Float.abs t)
;;

let%template float_uniform_exclusive lower_bound upper_bound =
  let open Float.O in
  if (not (Float.is_finite lower_bound)) || not (Float.is_finite upper_bound)
  then
    raise_s
      [%message
        "Float.uniform_exclusive: bounds are not finite"
          (lower_bound : float)
          (upper_bound : float)];
  let lower_inclusive = Float.one_ulp `Up lower_bound in
  let upper_inclusive = Float.one_ulp `Down upper_bound in
  if lower_inclusive > upper_inclusive
  then
    raise_s
      [%message
        "Float.uniform_exclusive: requested range is empty"
          (lower_bound : float)
          (upper_bound : float)];
  (create [@mode portable]) (fun ~size:_ ~random ->
    Splittable_random.float random ~lo:lower_inclusive ~hi:upper_inclusive)
;;

let%template float_inclusive lower_bound upper_bound =
  if Float.equal lower_bound upper_bound
  then (return [@mode portable]) lower_bound
  else if Float.( = ) (Float.one_ulp `Up lower_bound) upper_bound
  then
    (union [@mode portable])
      [ (return [@mode portable]) lower_bound; (return [@mode portable]) upper_bound ]
  else
    (weighted_union [@mode portable])
      [ 0.05, (return [@mode portable]) lower_bound
      ; 0.05, (return [@mode portable]) upper_bound
      ; 0.9, float_uniform_exclusive lower_bound upper_bound
      ]
;;

[%%template
[@@@mode.default p = (nonportable, portable)]

let string_with_length_of char_gen ~length =
  (list_with_length [@mode p]) char_gen ~length |> (map [@mode p]) ~f:String.of_char_list
;;

let string_of char_gen =
  (bind [@mode p]) small_positive_or_zero_int ~f:(fun length ->
    (string_with_length_of [@mode p]) char_gen ~length)
;;

let string_non_empty_of char_gen =
  (bind [@mode p]) small_strictly_positive_int ~f:(fun length ->
    (string_with_length_of [@mode p]) char_gen ~length)
;;]

let%template string = (string_of [@mode portable]) char
let%template string_non_empty = (string_non_empty_of [@mode portable]) char

let%template string_with_length ~length =
  (string_with_length_of [@mode portable]) char ~length
;;

module Edit_string = struct
  let edit_insert string =
    let%bind pos = int_uniform_inclusive 0 (String.length string) in
    let%bind len = int_geometric 1 ~p:0.5 in
    let%bind str = string_with_length ~length:len in
    [ String.prefix string pos; str; String.drop_prefix string pos ]
    |> String.concat
    |> return
  ;;

  let edit_remove string =
    let%bind len = int_log_uniform_inclusive 1 (String.length string) in
    let%bind pos = int_uniform_inclusive 0 (String.length string - len) in
    [ String.prefix string pos; String.drop_prefix string (pos + len) ]
    |> String.concat
    |> return
  ;;

  let edit_replace string =
    let%bind len = int_log_uniform_inclusive 1 (String.length string) in
    let%bind pos = int_uniform_inclusive 0 (String.length string - len) in
    let%bind str = string_with_length ~length:len in
    [ String.prefix string pos; str; String.drop_prefix string (pos + len) ]
    |> String.concat
    |> return
  ;;

  let edit_double string =
    let%bind len = int_log_uniform_inclusive 1 (String.length string) in
    let%bind pos = int_uniform_inclusive 0 (String.length string - len) in
    [ String.prefix string (pos + len); String.drop_prefix string pos ]
    |> String.concat
    |> return
  ;;

  let edit_nonempty string =
    [ edit_insert string; edit_remove string; edit_replace string; edit_double string ]
    |> union
  ;;

  let rec edit string n_times =
    if n_times <= 0
    then return string
    else (
      let%bind string =
        if String.is_empty string then edit_insert string else edit_nonempty string
      in
      edit string (n_times - 1))
  ;;
end

let string_like string =
  let%bind.Portable n_times = int_geometric 0 ~p:0.5 in
  Edit_string.edit string n_times
;;

let%template bytes = (map [@mode portable]) string ~f:Bytes.of_string

let%template sexp_of atom =
  (fixed_point [@mode p]) (fun self ->
    let open Syntax.Let_syntax [@mode p] in
    let%bind size in
    (* choose a number weighted low so we have a decreasing, but not vanishing, chance
       to generate atoms as size grows *)
    let open Syntax.Let_syntax in
    match%bind For_int.log_uniform_inclusive 0 (size + 1) with
    (* generate an atom using the given size *)
    | 0 ->
      let%map atom in
      Sexp.Atom atom
    (* relying on [List.gen] to distribute [size] over sub-sexps *)
    | _ ->
      let%map list = list self in
      Sexp.List list)
[@@mode p = (nonportable, portable)]
;;

let%template sexp = (sexp_of [@mode portable]) string

let%template map_tree_using_comparator ~comparator key_gen data_gen =
  (list [@mode p]) key_gen
  |> (bind [@mode p]) ~f:(fun keys ->
    let keys = List.dedup_and_sort keys ~compare:(Comparator.compare comparator) in
    let%bind data = (list_with_length [@mode p]) data_gen ~length:(List.length keys) in
    return (Map.Using_comparator.Tree.of_alist_exn ~comparator (List.zip_exn keys data)))
[@@mode p = (nonportable, portable)]
;;

let set_tree_using_comparator ~comparator elt_gen =
  map (list elt_gen) ~f:(Set.Using_comparator.Tree.of_list ~comparator)
;;

let%template map_t_m (type cmp) m key_gen data_gen =
  let (comparator : (_, cmp) Comparator.t) = Comparator.of_module m in
  (map_tree_using_comparator [@mode p]) ~comparator key_gen data_gen
  |> (map [@mode p]) ~f:(fun tree -> Map.Using_comparator.of_tree ~comparator tree)
[@@mode p = (nonportable, portable)]
;;

let set_t_m m elt_gen =
  let comparator = Comparator.of_module m in
  set_tree_using_comparator ~comparator elt_gen
  |> map ~f:(Set.Using_comparator.of_tree ~comparator)
;;

let%template bigarray1 t kind layout ~length =
  let open Syntax.Let_syntax [@mode p] in
  let%map elts =
    match length with
    | None -> (list [@mode p]) t
    | Some length -> (list_with_length [@mode p]) t ~length
  in
  let elts = Array.of_list elts in
  let dim = Array.length elts in
  let offset = Bigarray_helpers.Layout.offset layout in
  Bigarray_helpers.Array1.init kind layout dim ~f:(fun i -> elts.(i - offset))
[@@mode p = (nonportable, portable)]
;;

let%template bigstring_gen = (bigarray1 [@mode portable]) char Char C_layout
let%template float32_vec_gen = (bigarray1 [@mode portable]) float Float32 Fortran_layout
let%template float64_vec_gen = (bigarray1 [@mode portable]) float Float64 Fortran_layout
let bigstring = bigstring_gen ~length:None
let float32_vec = float32_vec_gen ~length:None
let float64_vec = float64_vec_gen ~length:None
let bigstring_with_length ~length = bigstring_gen ~length:(Some length)
let float32_vec_with_length ~length = float32_vec_gen ~length:(Some length)
let float64_vec_with_length ~length = float64_vec_gen ~length:(Some length)

let%template bigarray2_dim =
  match%bind.Portable size with
  | 0 -> return (0, 0)
  | max_total_size ->
    let%bind a =
      (* choose a dimension up to [max_total_size], weighted low to give the other
         dimension a good chance of being comparatively high *)
      int_log_uniform_inclusive 1 max_total_size
    in
    let%bind b =
      (* choose a dimension up to [max_total_size / a], weighted high to reach close to
         [max_total_size] most of the time *)
      let max_b = max_total_size / a in
      let%map b_weighted_low = int_log_uniform_inclusive 0 max_b in
      max_b - b_weighted_low
    in
    (* avoid any skew of a vs b by randomly swapping *)
    if%map bool then a, b else b, a
;;

let%template bigarray2 t kind layout =
  let%bind.Portable dim1, dim2 = bigarray2_dim in
  let%map.Portable elts =
    (list_with_length [@mode portable])
      ~length:dim1
      ((list_with_length [@mode portable]) ~length:dim2 t)
  in
  let elts = Array.of_list_map ~f:Array.of_list elts in
  let offset = Bigarray_helpers.Layout.offset layout in
  Bigarray_helpers.Array2.init kind layout dim1 dim2 ~f:(fun i j ->
    elts.(i - offset).(j - offset))
;;

let float32_mat = bigarray2 float Float32 Fortran_layout
let float64_mat = bigarray2 float Float64 Fortran_layout

module Debug = struct
  let coverage
    (type k cmp)
    (module Cmp : Comparator.S with type t = k and type comparator_witness = cmp)
    sample
    =
    Sequence.fold
      sample
      ~init:(Map.empty (module Cmp))
      ~f:(fun counts value ->
        Map.update counts value ~f:(function
          | None -> 1
          | Some prev -> prev + 1))
  ;;

  let monitor t ~f =
    map t ~f:(fun value ->
      f value;
      value)
  ;;
end
