open! Base

include Observer0

let unmap t ~f =
  create (fun x ~size ~hash ->
    observe t (f x) ~size ~hash)

let of_hash_fold f =
  create (fun x ~size:_ ~hash ->
    f hash x)

let fixed_point wrap =
  let rec f x ~size ~hash =
    observe (wrap (create f)) x ~size ~hash
  in
  create f

let unit = opaque

let bool      = of_hash_fold Bool.hash_fold_t
let char      = of_hash_fold Char.hash_fold_t
let int       = of_hash_fold Int.hash_fold_t
let int32     = of_hash_fold Int32.hash_fold_t
let int63     = of_hash_fold Int63.hash_fold_t
let int64     = of_hash_fold Int64.hash_fold_t
let nativeint = of_hash_fold Nativeint.hash_fold_t
let float     = of_hash_fold Float.hash_fold_t

let either fst_t snd_t =
  create (fun either ~size ~hash ->
    match (either : _ Either.t) with
    | First  fst -> observe fst_t fst ~size ~hash:(hash_fold_int hash 1)
    | Second snd -> observe snd_t snd ~size ~hash:(hash_fold_int hash 2))

let result ok_t err_t =
  unmap (either ok_t err_t)
    ~f:(function
      | Ok    ok  -> First  ok
      | Error err -> Second err)

let both fst_t snd_t =
  create (fun (fst, snd) ~size ~hash ->
    let hash = observe fst_t fst ~size ~hash in
    let hash = observe snd_t snd ~size ~hash in
    hash)

let option value_t =
  unmap (either opaque value_t)
    ~f:(function
      | None -> First ()
      | Some value -> Second value)

let list elt_t =
  fixed_point (fun self_t ->
    unmap (either opaque (both elt_t self_t))
      ~f:(function
        | [] -> First ()
        | hd :: tl -> Second (hd, tl)))

let string =
  unmap (list char) ~f:String.to_list

let sexp =
  fixed_point (fun self ->
    unmap (either string (list self))
      ~f:(function
        | Sexp.Atom atom -> First atom
        | Sexp.List list -> Second list))

let fn dom rng =
  create (fun f ~size ~hash ->
    let random = Splittable_random.State.of_int (Hash.get_hash_value hash) in
    let sizes = Generator.generate (Generator.sizes ()) ~size ~random in
    List.fold sizes ~init:hash ~f:(fun hash size ->
      let x = Generator.generate dom ~size ~random in
      observe rng (f x) ~size ~hash))

let map_tree key_obs data_obs =
  unmap (list (both key_obs data_obs)) ~f:Map.Using_comparator.Tree.to_alist

let set_tree elt_obs =
  unmap (list elt_obs) ~f:Set.Using_comparator.Tree.to_list

let map_t key_obs data_obs =
  unmap (map_tree key_obs data_obs) ~f:Map.Using_comparator.to_tree

let set_t elt_obs =
  unmap (set_tree elt_obs) ~f:Set.Using_comparator.to_tree
