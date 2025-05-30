open! Import

module type S = sig
  type ast
  type t

  val create : ast -> t

  (** location of the field declaration *)
  val location : t -> location

  (** type of the field's contents *)
  val core_type : t -> core_type

  (** constructing a pattern to match all fields of the type *)
  val pattern : t list -> loc:location -> pattern list -> pattern

  (** constructing an expression filling in all fields of the type *)
  val expression : t list -> loc:location -> expression list -> expression
end

module type Field_syntax = sig
  module type S = S

  module Tuple : S with type ast = core_type
  module Unboxed_tuple : S with type ast = core_type
  module Labeled_tuple : S with type ast = string option * core_type
  module Labeled_unboxed_tuple : S with type ast = string option * core_type
  module Record : S with type ast = label_declaration
  module Unboxed_record : S with type ast = label_declaration
end
