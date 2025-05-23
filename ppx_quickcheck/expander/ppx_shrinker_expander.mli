open! Import

val any : loc:location -> expression
val arrow : loc:location -> expression

val compound
  :  shrinker_of_core_type:(core_type -> expression)
  -> loc:location
  -> fields:'a list
  -> portable_value:bool
  -> (module Field_syntax.S with type ast = 'a)
  -> expression

val variant
  :  shrinker_of_core_type:(core_type -> expression)
  -> loc:location
  -> variant_type:core_type
  -> clauses:'a list
  -> portable_value:bool
  -> (module Clause_syntax.S with type ast = 'a)
  -> expression
