open! Import

val sig_type_decl
  : (signature_item list, rec_flag * type_declaration list) Deriving.Generator.t

val str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t

val generator_extension
  :  portable:bool
  -> loc:location
  -> path:string
  -> core_type
  -> expression

val observer_extension
  :  portable:bool
  -> loc:location
  -> path:string
  -> core_type
  -> expression

val shrinker_extension
  :  portable:bool
  -> loc:location
  -> path:string
  -> core_type
  -> expression
