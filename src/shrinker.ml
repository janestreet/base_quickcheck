open! Base

type ('a : any) thunk = unit -> 'a

module T : sig @@ portable
  type ('a : any) t : value mod contended

  val atomic : ('a : any). 'a t

  val%template create : ('a -> 'a Sequence.t) @ p -> 'a t @ p
  [@@mode p = (nonportable, portable)]

  val shrink : 'a t -> 'a -> 'a Sequence.t

  module Via_thunk : sig
    val%template create : ('a : any). ('a thunk -> 'a thunk Sequence.t) @ p -> 'a t @ p
    [@@mode p = (nonportable, portable)]

    val shrink : ('a : any). 'a t -> 'a thunk -> 'a thunk Sequence.t
  end
end = struct
  type ('a : any) t = 'a thunk -> 'a thunk Sequence.t

  let atomic _ = Sequence.get_empty ()

  let%template create (f @ p) thunk = Sequence.map (f (thunk ())) ~f:(fun x () -> x)
  [@@mode p = (nonportable, portable)]
  ;;

  let shrink t x = Sequence.map (t (fun () -> x)) ~f:(fun thunk -> thunk ())

  module Via_thunk = struct
    let%template create t = t [@@mode __ = (nonportable, portable)]
    let shrink = Fn.id
  end
end

include T

module Via_thunk = struct
  include Via_thunk

  let%template map t ~f ~f_inverse =
    (create [@mode p]) (fun thunk -> Sequence.map ~f (shrink t (f_inverse thunk)))
  [@@mode p = (nonportable, portable)]
  ;;
end

[%%template
[@@@mode.default p = (nonportable, portable)]

let map t ~f ~f_inverse =
  (create [@mode p]) (fun x -> Sequence.map ~f (shrink t (f_inverse x)))
;;

let filter t ~f = (create [@mode p]) (fun x -> Sequence.filter ~f (shrink t x))

let filter_map t ~f ~f_inverse =
  (create [@mode p]) (fun x -> Sequence.filter_map ~f (shrink t (f_inverse x)))
;;

let both fst_t snd_t =
  (create [@mode p]) (fun (fst, snd) ->
    Sequence.round_robin
      [ Sequence.map (shrink fst_t fst) ~f:(fun fst -> fst, snd)
      ; Sequence.map (shrink snd_t snd) ~f:(fun snd -> fst, snd)
      ])
;;]

let of_lazy lazy_t = create (fun x -> Sequence.of_lazy (lazy (shrink (force lazy_t) x)))

let%template of_portable_lazy portable_lazy_t =
  (create [@mode portable]) (fun x -> shrink (Portable_lazy.force portable_lazy_t) x)
;;

include struct
  let fixed_point of_shrinker =
    let rec lazy_t = lazy (of_shrinker (of_lazy lazy_t)) in
    of_lazy lazy_t
  ;;

  let%template fixed_point of_shrinker =
    of_portable_lazy
      (Portable_lazy.from_fun_fixed (fun portable_lazy_t ->
         of_shrinker (of_portable_lazy portable_lazy_t)))
  [@@mode portable]
  ;;
end

let unit = atomic
let bool = atomic
let char = atomic
let int = atomic
let int32 = atomic
let int63 = atomic
let int64 = atomic
let nativeint = atomic
let float = atomic

let bigarray1 src =
  let dim = Bigarray.Array1.dim src in
  match dim with
  | 0 -> Sequence.get_empty ()
  | _ ->
    let kind = Bigarray.Array1.kind src in
    let layout = Bigarray.Array1.layout src in
    let offset = Bigarray_helpers.Layout.offset layout in
    Sequence.init dim ~f:(fun to_skip ->
      let to_skip = to_skip + offset in
      Bigarray_helpers.Array1.init kind layout (dim - 1) ~f:(fun i ->
        src.{if i < to_skip then i else i + 1}))
;;

let%template bigstring = (create [@mode portable]) bigarray1
let%template float32_vec = (create [@mode portable]) bigarray1
let%template float64_vec = (create [@mode portable]) bigarray1

let bigarray2 =
  let module Dims = struct
    type t =
      { dim1 : int
      ; dim2 : int
      }
    [@@deriving fields ~fields]

    let create a = Bigarray.Array2.{ dim1 = dim1 a; dim2 = dim2 a }
  end
  in
  let shrink field src =
    let dims = Dims.create src in
    match Field.get field dims with
    | 0 -> Sequence.get_empty ()
    | _ ->
      let kind = Bigarray.Array2.kind src in
      let layout = Bigarray.Array2.layout src in
      let offset = Bigarray_helpers.Layout.offset layout in
      let ({ dim1; dim2 } : Dims.t) = Field.map field dims ~f:Int.pred in
      Sequence.init (Field.get field dims) ~f:(fun to_skip ->
        let to_skip = to_skip + offset in
        let skip i = if i < to_skip then i else i + 1 in
        Bigarray_helpers.Array2.init kind layout dim1 dim2 ~f:(fun dim1 dim2 ->
          let ({ dim1; dim2 } : Dims.t) = Field.map field { dim1; dim2 } ~f:skip in
          src.{dim1, dim2}))
  in
  fun src ->
    Sequence.round_robin [ shrink Dims.Fields.dim1 src; shrink Dims.Fields.dim2 src ]
;;

let%template float32_mat = (create [@mode portable]) bigarray2
let%template float64_mat = (create [@mode portable]) bigarray2

[%%template
[@@@mode.default p = (nonportable, portable)]

let option value_t =
  (create [@mode p]) (function
    | None -> Sequence.get_empty ()
    | Some value ->
      Sequence.append
        (Sequence.singleton None)
        (Sequence.map ~f:Option.return (shrink value_t value)))
;;

let list elt_t =
  (fixed_point [@mode p]) (fun list_t ->
    (create [@mode p]) (function
      | [] -> Sequence.get_empty ()
      | head :: tail ->
        Sequence.round_robin
          [ Sequence.singleton tail
          ; Sequence.map (shrink elt_t head) ~f:(fun head -> head :: tail)
          ; Sequence.map (shrink list_t tail) ~f:(fun tail -> head :: tail)
          ]))
;;

let array t =
  (map [@mode p]) ((list [@mode p]) t) ~f:Array.of_list ~f_inverse:Array.to_list
;;

let ref t = (map [@mode p]) t ~f:Ref.create ~f_inverse:Ref.( ! )
let lazy_t t = (map [@mode p]) t ~f:Lazy.from_val ~f_inverse:Lazy.force

let either fst_t snd_t =
  (create [@mode p]) (fun either ->
    match (either : _ Either.t) with
    | First fst -> Sequence.map (shrink fst_t fst) ~f:Either.first
    | Second snd -> Sequence.map (shrink snd_t snd) ~f:Either.second)
;;

let result ok_t err_t =
  (map [@mode p])
    ((either [@mode p]) ok_t err_t)
    ~f:Result.of_either
    ~f_inverse:Result.to_either
;;]

let%template string =
  (map [@mode portable])
    ((list [@mode portable]) char)
    ~f:String.of_char_list
    ~f_inverse:String.to_list
;;

let%template bytes =
  (map [@mode portable]) string ~f:Bytes.of_string ~f_inverse:Bytes.to_string
;;

let%template sexp =
  (fixed_point [@mode portable]) (fun shrinker ->
    (create [@mode portable]) (function
      | Sexp.Atom _ -> Sequence.get_empty ()
      | Sexp.List l ->
        let shrink_list =
          shrink (list shrinker) l |> Sequence.map ~f:(fun l -> Sexp.List l)
        in
        let shrink_tree = Sequence.of_list l in
        Sequence.round_robin [ shrink_list; shrink_tree ]))
;;

let%template map_tree_using_comparator ~comparator key_t data_t =
  (create [@mode p]) (fun tree ->
    let alist = Map.Using_comparator.Tree.to_alist tree in
    let drop_keys =
      Sequence.map (Sequence.of_list alist) ~f:(fun (k, _) ->
        Map.Using_comparator.Tree.remove ~comparator tree k)
    in
    let shrink_keys =
      Sequence.round_robin
        (List.map alist ~f:(fun (key, data) ->
           let tree = Map.Using_comparator.Tree.remove ~comparator tree key in
           Sequence.filter_map (shrink key_t key) ~f:(fun smaller_key ->
             match
               Map.Using_comparator.Tree.add ~comparator tree ~key:smaller_key ~data
             with
             | `Ok tree -> Some tree
             | `Duplicate -> None)))
    in
    let shrink_data =
      Sequence.round_robin
        (List.map alist ~f:(fun (key, data) ->
           Sequence.map (shrink data_t data) ~f:(fun smaller_data ->
             Map.Using_comparator.Tree.set ~comparator tree ~key ~data:smaller_data)))
    in
    Sequence.round_robin [ drop_keys; shrink_keys; shrink_data ])
[@@mode p = (nonportable, portable)]
;;

let set_tree_using_comparator ~comparator elt_t =
  create (fun tree ->
    let list = Set.Using_comparator.Tree.to_list tree in
    let drop_elts =
      Sequence.map (Sequence.of_list list) ~f:(fun elt ->
        Set.Using_comparator.Tree.remove ~comparator tree elt)
    in
    let shrink_elts =
      Sequence.round_robin
        (List.map list ~f:(fun elt ->
           let tree = Set.Using_comparator.Tree.remove ~comparator tree elt in
           Sequence.filter_map (shrink elt_t elt) ~f:(fun smaller_elt ->
             match Set.Using_comparator.Tree.mem ~comparator tree smaller_elt with
             | true -> None
             | false -> Some (Set.Using_comparator.Tree.add tree ~comparator smaller_elt))))
    in
    Sequence.round_robin [ drop_elts; shrink_elts ])
;;

let%template map_t (type cmp : value mod p) key_t data_t =
  (create [@mode p]) (fun (map_t : (_, _, cmp) Map.t) ->
    let comparator = Map.comparator map_t in
    let t =
      (map [@mode p])
        ((map_tree_using_comparator [@mode p]) ~comparator key_t data_t)
        ~f:(fun tree -> Map.Using_comparator.of_tree tree ~comparator)
        ~f_inverse:Map.Using_comparator.to_tree
    in
    shrink t map_t)
[@@mode p = (nonportable, portable)]
;;

let set_t elt_t =
  create (fun set_t ->
    let comparator = Set.comparator set_t in
    let t =
      map
        (set_tree_using_comparator ~comparator elt_t)
        ~f:(Set.Using_comparator.of_tree ~comparator)
        ~f_inverse:Set.Using_comparator.to_tree
    in
    shrink t set_t)
;;
