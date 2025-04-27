open! Import

val any : loc:location -> expression

val compound
  :  observer_of_core_type:(core_type -> expression)
  -> loc:location
  -> fields:'a list
  -> portable_value:bool
  -> (module Field_syntax.S with type ast = 'a)
  -> expression

val variant
  :  observer_of_core_type:(core_type -> expression)
  -> loc:location
  -> clauses:'a list
  -> portable_value:bool
  -> (module Clause_syntax.S with type ast = 'a)
  -> expression

val arrow
  :  observer_of_core_type:(core_type -> expression)
  -> generator_of_core_type:(core_type -> expression)
  -> loc:location
  -> arg_label:arg_label
  -> input_type:core_type
  -> output_type:core_type
  -> portable_value:bool
  -> expression
