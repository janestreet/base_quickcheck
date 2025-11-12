open! Base

module type S_bigarray = sig
  (** This helper module type exists separately just to [open Bigarray] in its scope. *)
  open Bigarray

  type 'a t

  val bigstring : (char, int8_unsigned_elt, c_layout) Array1.t t
  val float32_vec : (float, float32_elt, fortran_layout) Array1.t t
  val float64_vec : (float, float64_elt, fortran_layout) Array1.t t
  val float32_mat : (float, float32_elt, fortran_layout) Array2.t t
  val float64_mat : (float, float64_elt, fortran_layout) Array2.t t
end

module type S = sig
  type ('a : value_or_null) t

  val unit : unit t
  val bool : bool t
  val char : char t
  val string : string t
  val bytes : bytes t
  val int : int t
  val int32 : int32 t
  val int63 : Int63.t t
  val int64 : int64 t
  val nativeint : nativeint t
  val float : float t
  val sexp : Sexp.t t

  [%%template:
  [@@@mode.default p = (portable, nonportable)]

  val option : 'a t @ p -> 'a option t @ p
  val or_null : 'a t @ p -> 'a or_null t @ p
  val list : 'a t @ p -> 'a list t @ p
  val array : 'a t @ p -> 'a array t @ p
  val ref : 'a t @ p -> 'a ref t @ p
  val lazy_t : 'a t @ p -> 'a Lazy.t t @ p
  val both : 'a t @ p -> 'b t @ p -> ('a * 'b) t @ p
  val either : 'a t @ p -> 'b t @ p -> ('a, 'b) Either.t t @ p
  val result : 'a t @ p -> 'b t @ p -> ('a, 'b) Result.t t @ p]

  include S_bigarray with type 'a t := 'a t (** @inline *)
end
