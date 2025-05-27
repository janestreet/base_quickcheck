open! Import

let custom_extension ~loc tag payload =
  match String.equal tag.txt "custom" with
  | false -> unsupported ~loc "unknown extension: %s" tag.txt
  | true ->
    (match payload with
     | PStr [ { pstr_desc = Pstr_eval (expr, attributes); _ } ] ->
       assert_no_attributes attributes;
       expr
     | _ -> invalid ~loc "[%%custom] extension expects a single expression as its payload")
;;

let generator_attribute =
  Attribute.declare
    "quickcheck.generator"
    Attribute.Context.core_type
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let shrinker_attribute =
  Attribute.declare
    "quickcheck.shrinker"
    Attribute.Context.core_type
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let observer_attribute =
  Attribute.declare
    "quickcheck.observer"
    Attribute.Context.core_type
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let rec generator_of_core_type core_type ~gen_env ~obs_env ~portable_value =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  match Attribute.get generator_attribute core_type with
  | Some expr -> expr
  | None ->
    (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
     | Ptyp_constr (constr, args) ->
       type_constr_conv
         ~loc
         ~f:(generator_name ~name_is_portable:(name_is_portable ~portable_value args))
         constr
         (List.map args ~f:(generator_of_core_type ~gen_env ~obs_env ~portable_value))
     | Ptyp_var (tyvar, _) -> Environment.lookup gen_env ~loc ~tyvar
     | Ptyp_arrow (arg_label, input_type, output_type, _, _) ->
       Ppx_generator_expander.arrow
         ~generator_of_core_type:
           (generator_of_core_type ~gen_env ~obs_env ~portable_value)
         ~observer_of_core_type:(observer_of_core_type ~gen_env ~obs_env ~portable_value)
         ~loc
         ~arg_label
         ~input_type
         ~output_type
         ~portable_value
     | Ptyp_tuple labeled_fields ->
       (match Ppxlib_jane.as_unlabeled_tuple labeled_fields with
        | Some fields ->
          Ppx_generator_expander.compound
            ~generator_of_core_type:
              (generator_of_core_type ~gen_env ~obs_env ~portable_value)
            ~loc
            ~fields
            ~portable_value
            (module Field_syntax.Tuple)
        | None ->
          Ppx_generator_expander.compound
            ~generator_of_core_type:
              (generator_of_core_type ~gen_env ~obs_env ~portable_value)
            ~loc
            ~fields:labeled_fields
            ~portable_value
            (module Field_syntax.Labeled_tuple))
     | Ptyp_unboxed_tuple labeled_fields ->
       (match Ppxlib_jane.as_unlabeled_tuple labeled_fields with
        | Some fields ->
          Ppx_generator_expander.compound
            ~generator_of_core_type:
              (generator_of_core_type ~gen_env ~obs_env ~portable_value)
            ~loc
            ~fields
            ~portable_value
            (module Field_syntax.Unboxed_tuple)
        | None ->
          Ppx_generator_expander.compound
            ~generator_of_core_type:
              (generator_of_core_type ~gen_env ~obs_env ~portable_value)
            ~loc
            ~fields:labeled_fields
            ~portable_value
            (module Field_syntax.Labeled_unboxed_tuple))
     | Ptyp_variant (clauses, Closed, None) ->
       Ppx_generator_expander.variant
         ~generator_of_core_type:
           (generator_of_core_type ~gen_env ~obs_env ~portable_value)
         ~loc
         ~variant_type:core_type
         ~clauses
         ~rec_names:(Set.empty (module String))
         ~portable_value
         (module Clause_syntax.Polymorphic_variant)
     | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant type with [>]"
     | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant type with [<]"
     | Ptyp_extension (tag, payload) -> custom_extension ~loc tag payload
     | core_type ->
       unsupported
         ~loc
         "%s"
         (Ppxlib_jane.Language_feature_name.of_core_type_desc core_type))

and observer_of_core_type core_type ~obs_env ~gen_env ~portable_value =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  match Attribute.get observer_attribute core_type with
  | Some expr -> expr
  | None ->
    (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
     | Ptyp_constr (constr, args) ->
       type_constr_conv
         ~loc
         ~f:(observer_name ~name_is_portable:(name_is_portable ~portable_value args))
         constr
         (List.map args ~f:(observer_of_core_type ~obs_env ~gen_env ~portable_value))
     | Ptyp_var (tyvar, _) -> Environment.lookup obs_env ~loc ~tyvar
     | Ptyp_arrow (arg_label, input_type, output_type, _, _) ->
       Ppx_observer_expander.arrow
         ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env ~portable_value)
         ~generator_of_core_type:
           (generator_of_core_type ~obs_env ~gen_env ~portable_value)
         ~loc
         ~arg_label
         ~input_type
         ~output_type
         ~portable_value
     | Ptyp_tuple labeled_fields ->
       (match Ppxlib_jane.as_unlabeled_tuple labeled_fields with
        | Some fields ->
          Ppx_observer_expander.compound
            ~observer_of_core_type:
              (observer_of_core_type ~obs_env ~gen_env ~portable_value)
            ~loc
            ~fields
            ~portable_value
            (module Field_syntax.Tuple)
        | None ->
          Ppx_observer_expander.compound
            ~observer_of_core_type:
              (observer_of_core_type ~obs_env ~gen_env ~portable_value)
            ~loc
            ~fields:labeled_fields
            ~portable_value
            (module Field_syntax.Labeled_tuple))
     | Ptyp_unboxed_tuple labeled_fields ->
       (match Ppxlib_jane.as_unlabeled_tuple labeled_fields with
        | Some fields ->
          Ppx_observer_expander.compound
            ~observer_of_core_type:
              (observer_of_core_type ~obs_env ~gen_env ~portable_value)
            ~loc
            ~fields
            ~portable_value
            (module Field_syntax.Unboxed_tuple)
        | None ->
          Ppx_observer_expander.compound
            ~observer_of_core_type:
              (observer_of_core_type ~obs_env ~gen_env ~portable_value)
            ~loc
            ~fields:labeled_fields
            ~portable_value
            (module Field_syntax.Labeled_unboxed_tuple))
     | Ptyp_variant (clauses, Closed, None) ->
       Ppx_observer_expander.variant
         ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env ~portable_value)
         ~loc
         ~clauses
         ~portable_value
         (module Clause_syntax.Polymorphic_variant)
     | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant type with [>]"
     | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant type with [<]"
     | Ptyp_extension (tag, payload) -> custom_extension ~loc tag payload
     | Ptyp_any _ -> Ppx_observer_expander.any ~loc
     | core_type ->
       unsupported
         ~loc
         "%s"
         (Ppxlib_jane.Language_feature_name.of_core_type_desc core_type))
;;

let rec shrinker_of_core_type core_type ~env ~portable_value =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  match Attribute.get shrinker_attribute core_type with
  | Some expr -> expr
  | None ->
    (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
     | Ptyp_constr (constr, args) ->
       type_constr_conv
         ~loc
         ~f:(shrinker_name ~name_is_portable:(name_is_portable ~portable_value args))
         constr
         (List.map args ~f:(shrinker_of_core_type ~env ~portable_value))
     | Ptyp_var (tyvar, _) -> Environment.lookup env ~loc ~tyvar
     | Ptyp_arrow _ -> Ppx_shrinker_expander.arrow ~loc
     | Ptyp_tuple labeled_fields ->
       (match Ppxlib_jane.as_unlabeled_tuple labeled_fields with
        | Some fields ->
          Ppx_shrinker_expander.compound
            ~shrinker_of_core_type:(shrinker_of_core_type ~env ~portable_value)
            ~loc
            ~fields
            ~portable_value
            (module Field_syntax.Tuple)
        | None ->
          Ppx_shrinker_expander.compound
            ~shrinker_of_core_type:(shrinker_of_core_type ~env ~portable_value)
            ~loc
            ~fields:labeled_fields
            ~portable_value
            (module Field_syntax.Labeled_tuple))
     | Ptyp_unboxed_tuple labeled_fields ->
       (match Ppxlib_jane.as_unlabeled_tuple labeled_fields with
        | Some fields ->
          Ppx_shrinker_expander.compound
            ~shrinker_of_core_type:(shrinker_of_core_type ~env ~portable_value)
            ~loc
            ~fields
            ~portable_value
            (module Field_syntax.Unboxed_tuple)
        | None ->
          Ppx_shrinker_expander.compound
            ~shrinker_of_core_type:(shrinker_of_core_type ~env ~portable_value)
            ~loc
            ~fields:labeled_fields
            ~portable_value
            (module Field_syntax.Labeled_unboxed_tuple))
     | Ptyp_variant (clauses, Closed, None) ->
       Ppx_shrinker_expander.variant
         ~shrinker_of_core_type:(shrinker_of_core_type ~env ~portable_value)
         ~loc
         ~variant_type:core_type
         ~clauses
         ~portable_value
         (module Clause_syntax.Polymorphic_variant)
     | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant type with [>]"
     | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant type with [<]"
     | Ptyp_extension (tag, payload) -> custom_extension ~loc tag payload
     | Ptyp_any _ -> Ppx_shrinker_expander.any ~loc
     | core_type ->
       unsupported
         ~loc
         "%s"
         (Ppxlib_jane.Language_feature_name.of_core_type_desc core_type))
;;

type impl =
  { loc : location
  ; typ : core_type
  ; pat : pattern
  ; var : expression
  ; exp : expression
  }

let generator_impl ~rec_names ~portable_value type_decl =
  let loc = type_decl.ptype_loc in
  let typ =
    combinator_type_of_type_declaration type_decl ~f:(fun ~loc ty ->
      [%type: [%t ty] Ppx_quickcheck_runtime.Base_quickcheck.Generator.t])
  in
  let name_is_portable = name_is_portable ~portable_value type_decl.ptype_params in
  let pat = pgenerator type_decl.ptype_name ~name_is_portable in
  let var = egenerator type_decl.ptype_name ~name_is_portable in
  let exp =
    let pat_list, `Covariant gen_env, `Contravariant obs_env =
      Environment.create_with_variance
        ~loc
        ~covariant:"generator"
        ~contravariant:"observer"
        type_decl.ptype_params
    in
    let body =
      match Ppxlib_jane.Shim.Type_kind.of_parsetree type_decl.ptype_kind with
      | Ptype_open -> unsupported ~loc "open type"
      | Ptype_variant clauses ->
        Ppx_generator_expander.variant
          ~generator_of_core_type:
            (generator_of_core_type ~gen_env ~obs_env ~portable_value)
          ~loc
          ~variant_type:[%type: _]
          ~clauses
          ~rec_names
          ~portable_value
          (module Clause_syntax.Variant)
      | Ptype_record fields ->
        Ppx_generator_expander.compound
          ~generator_of_core_type:
            (generator_of_core_type ~gen_env ~obs_env ~portable_value)
          ~loc
          ~fields
          ~portable_value
          (module Field_syntax.Record)
      | Ptype_record_unboxed_product fields ->
        Ppx_generator_expander.compound
          ~generator_of_core_type:
            (generator_of_core_type ~gen_env ~obs_env ~portable_value)
          ~loc
          ~fields
          ~portable_value
          (module Field_syntax.Unboxed_record)
      | Ptype_abstract ->
        (match type_decl.ptype_manifest with
         | Some core_type ->
           generator_of_core_type core_type ~gen_env ~obs_env ~portable_value
         | None -> unsupported ~loc "abstract type")
    in
    List.fold_right pat_list ~init:body ~f:(fun pat body ->
      [%expr fun [%p pat] -> [%e body]])
  in
  { loc; typ; pat; var; exp }
;;

let observer_impl ~rec_names:_ ~portable_value type_decl =
  let loc = type_decl.ptype_loc in
  let typ =
    combinator_type_of_type_declaration type_decl ~f:(fun ~loc ty ->
      [%type: [%t ty] Ppx_quickcheck_runtime.Base_quickcheck.Observer.t])
  in
  let name_is_portable = name_is_portable ~portable_value type_decl.ptype_params in
  let pat = pobserver type_decl.ptype_name ~name_is_portable in
  let var = eobserver type_decl.ptype_name ~name_is_portable in
  let exp =
    let pat_list, `Covariant obs_env, `Contravariant gen_env =
      Environment.create_with_variance
        ~loc
        ~covariant:"observer"
        ~contravariant:"generator"
        type_decl.ptype_params
    in
    let body =
      match Ppxlib_jane.Shim.Type_kind.of_parsetree type_decl.ptype_kind with
      | Ptype_open -> unsupported ~loc "open type"
      | Ptype_variant clauses ->
        Ppx_observer_expander.variant
          ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env ~portable_value)
          ~loc
          ~clauses
          ~portable_value
          (module Clause_syntax.Variant)
      | Ptype_record fields ->
        Ppx_observer_expander.compound
          ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env ~portable_value)
          ~loc
          ~fields
          ~portable_value
          (module Field_syntax.Record)
      | Ptype_record_unboxed_product fields ->
        Ppx_observer_expander.compound
          ~observer_of_core_type:(observer_of_core_type ~gen_env ~obs_env ~portable_value)
          ~loc
          ~fields
          ~portable_value
          (module Field_syntax.Unboxed_record)
      | Ptype_abstract ->
        (match type_decl.ptype_manifest with
         | Some core_type ->
           observer_of_core_type core_type ~obs_env ~gen_env ~portable_value
         | None -> unsupported ~loc "abstract type")
    in
    List.fold_right pat_list ~init:body ~f:(fun pat body ->
      [%expr fun [%p pat] -> [%e body]])
  in
  { loc; typ; pat; var; exp }
;;

let shrinker_impl ~rec_names:_ ~portable_value type_decl =
  let loc = type_decl.ptype_loc in
  let typ =
    combinator_type_of_type_declaration type_decl ~f:(fun ~loc ty ->
      [%type: [%t ty] Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.t])
  in
  let name_is_portable = name_is_portable ~portable_value type_decl.ptype_params in
  let pat = pshrinker type_decl.ptype_name ~name_is_portable in
  let var = eshrinker type_decl.ptype_name ~name_is_portable in
  let exp =
    let pat_list, env =
      Environment.create ~loc ~prefix:"shrinker" type_decl.ptype_params
    in
    let body =
      match Ppxlib_jane.Shim.Type_kind.of_parsetree type_decl.ptype_kind with
      | Ptype_open -> unsupported ~loc "open type"
      | Ptype_variant clauses ->
        Ppx_shrinker_expander.variant
          ~shrinker_of_core_type:(shrinker_of_core_type ~env ~portable_value)
          ~loc
          ~variant_type:[%type: _]
          ~clauses
          ~portable_value
          (module Clause_syntax.Variant)
      | Ptype_record fields ->
        Ppx_shrinker_expander.compound
          ~shrinker_of_core_type:(shrinker_of_core_type ~env ~portable_value)
          ~loc
          ~fields
          ~portable_value
          (module Field_syntax.Record)
      | Ptype_record_unboxed_product fields ->
        Ppx_shrinker_expander.compound
          ~shrinker_of_core_type:(shrinker_of_core_type ~env ~portable_value)
          ~loc
          ~fields
          ~portable_value
          (module Field_syntax.Unboxed_record)
      | Ptype_abstract ->
        (match type_decl.ptype_manifest with
         | Some core_type -> shrinker_of_core_type core_type ~env ~portable_value
         | None -> unsupported ~loc "abstract type")
    in
    List.fold_right pat_list ~init:body ~f:(fun pat body ->
      [%expr fun [%p pat] -> [%e body]])
  in
  { loc; typ; pat; var; exp }
;;

let close_the_loop ~of_lazy decl impl =
  let loc = impl.loc in
  let exp = impl.var in
  match decl.ptype_params with
  | [] -> eapply ~loc of_lazy [ exp ]
  | params ->
    let pats, exps =
      gensyms "recur" (List.map params ~f:(fun (core_type, _) -> core_type.ptyp_loc))
    in
    eabstract
      ~loc
      pats
      (eapply
         ~loc
         of_lazy
         [ [%expr
             lazy
               [%e
                 eapply
                   ~loc
                   (eapply ~loc [%expr Ppx_quickcheck_runtime.Base.Lazy.force] [ exp ])
                   exps]]
         ])
;;

let each decls ~portable_export ~f =
  List.concat_map decls ~f:(fun decl ->
    List.map
      (if name_is_portable ~portable_value:portable_export decl.ptype_params
       then [ true; false ]
       else [ portable_export ])
      ~f:(fun portable_value -> f ~portable_export ~portable_value decl))
;;

let maybe_mutually_recursive decls ~loc ~portable_export ~rec_flag ~of_lazy ~impl =
  let decls = List.map decls ~f:name_type_params_in_td in
  let rec_names =
    match rec_flag with
    | Nonrecursive -> Set.empty (module String)
    | Recursive ->
      Set.of_list (module String) (List.map decls ~f:(fun decl -> decl.ptype_name.txt))
  in
  let impls =
    each decls ~portable_export ~f:(fun ~portable_export:_ ~portable_value decl ->
      impl ~rec_names ~portable_value decl)
  in
  let modes =
    if portable_export then [ { loc; txt = Ppxlib_jane.Mode "portable" } ] else []
  in
  match rec_flag with
  | Nonrecursive ->
    pstr_value_list
      ~loc
      Nonrecursive
      (List.map impls ~f:(fun impl ->
         Ppxlib_jane.Ast_builder.Default.value_binding
           ~loc:impl.loc
           ~pat:impl.pat
           ~expr:impl.exp
           ~modes))
  | Recursive ->
    let recursive_bindings =
      let inner_bindings =
        List.map2_exn decls impls ~f:(fun decl inner ->
          value_binding
            ~loc:inner.loc
            ~pat:inner.pat
            ~expr:(close_the_loop ~of_lazy decl inner))
      in
      let wrap_body exp =
        List.fold impls ~init:exp ~f:(fun acc impl ->
          let ign = [%expr ignore [%e impl.var]] in
          pexp_sequence ~loc ign acc)
        |> pexp_let ~loc Nonrecursive inner_bindings
      in
      let wrap exp =
        match
          Ppxlib_jane.Shim.Pexp_function.of_parsetree exp.pexp_desc ~loc:exp.pexp_loc
        with
        | Some (params, function_constraint, Pfunction_body body) ->
          let body = wrap_body body in
          Ppxlib_jane.Ast_builder.Default.Latest.pexp_function
            params
            function_constraint
            (Pfunction_body body)
            ~loc:exp.pexp_loc
            ~attrs:exp.pexp_attributes
        | Some (_, _, Pfunction_cases _) | None -> wrap_body exp
      in
      List.map2_exn decls impls ~f:(fun decl impl ->
        let body = wrap impl.exp in
        let lazy_expr = [%expr lazy [%e body]] in
        let typed_pat =
          [%type: [%t impl.typ] Ppx_quickcheck_runtime.Base.Lazy.t]
          |> ptyp_poly ~loc (List.map decl.ptype_params ~f:get_type_param_name)
          |> ppat_constraint ~loc impl.pat
        in
        value_binding ~loc:impl.loc ~pat:typed_pat ~expr:lazy_expr)
    in
    [%str
      include struct
        open [%m pmod_structure ~loc (pstr_value_list ~loc Recursive recursive_bindings)]

        [%%i
          pstr_value
            ~loc
            Nonrecursive
            (List.map2_exn decls impls ~f:(fun decl impl ->
               Ppxlib_jane.Ast_builder.Default.value_binding
                 ~loc
                 ~pat:impl.pat
                 ~expr:(close_the_loop ~of_lazy decl impl)
                 ~modes))]
      end]
;;

let generator_impl_list decls ~loc ~rec_flag ~portable_export =
  maybe_mutually_recursive
    decls
    ~loc
    ~rec_flag
    ~portable_export
    ~of_lazy:[%expr Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy]
    ~impl:generator_impl
;;

let observer_impl_list decls ~loc ~rec_flag ~portable_export =
  maybe_mutually_recursive
    decls
    ~loc
    ~rec_flag
    ~portable_export
    ~of_lazy:[%expr Ppx_quickcheck_runtime.Base_quickcheck.Observer.of_lazy]
    ~impl:observer_impl
;;

let shrinker_impl_list decls ~loc ~rec_flag ~portable_export =
  maybe_mutually_recursive
    decls
    ~loc
    ~rec_flag
    ~portable_export
    ~of_lazy:[%expr Ppx_quickcheck_runtime.Base_quickcheck.Shrinker.of_lazy]
    ~impl:shrinker_impl
;;

let intf ~f ~covar ~contravar ~portable_export ~portable_value type_decl =
  let name_is_portable = name_is_portable ~portable_value type_decl.ptype_params in
  let covar =
    Longident.parse ("Ppx_quickcheck_runtime.Base_quickcheck." ^ covar ^ ".t")
  in
  let contravar =
    Longident.parse ("Ppx_quickcheck_runtime.Base_quickcheck." ^ contravar ^ ".t")
  in
  let type_decl = name_type_params_in_td type_decl in
  let loc = type_decl.ptype_loc in
  let name = loc_map type_decl.ptype_name ~f:(f ~name_is_portable) in
  let result =
    ptyp_constr
      ~loc
      { loc; txt = covar }
      [ ptyp_constr
          ~loc
          (lident_loc type_decl.ptype_name)
          (List.map type_decl.ptype_params ~f:fst)
      ]
  in
  let type_ =
    let modes =
      if portable_value then [ { loc; txt = Ppxlib_jane.Mode "portable" } ] else []
    in
    let result =
      List.fold_right
        type_decl.ptype_params
        ~init:{ result_type = result; result_modes = modes }
        ~f:(fun (core_type, (variance, _)) result ->
          let id =
            match variance with
            | NoVariance | Covariant -> covar
            | Contravariant -> contravar
          in
          let arg = ptyp_constr ~loc { loc; txt = id } [ core_type ] in
          { Ppxlib_jane.Shim.result_type =
              Ppxlib_jane.Ast_builder.Default.ptyp_arrow
                ~loc
                { arg_type = arg; arg_label = Nolabel; arg_modes = modes }
                result
          ; result_modes = []
          })
    in
    result.result_type
  in
  psig_value
    ~loc
    (Ppxlib_jane.Ast_builder.Default.value_description
       ~loc
       ~name
       ~type_
       ~modalities:(if portable_export then [ Ppxlib_jane.Modality "portable" ] else [])
       ~prim:[])
;;

let shrinker_intf = intf ~f:shrinker_name ~covar:"Shrinker" ~contravar:"Shrinker"
let generator_intf = intf ~f:generator_name ~covar:"Generator" ~contravar:"Observer"
let observer_intf = intf ~f:observer_name ~covar:"Observer" ~contravar:"Generator"

let generator_intf_list type_decl_list ~portable_export =
  each type_decl_list ~portable_export ~f:generator_intf
;;

let observer_intf_list type_decl_list ~portable_export =
  each type_decl_list ~portable_export ~f:observer_intf
;;

let shrinker_intf_list type_decl_list ~portable_export =
  each type_decl_list ~portable_export ~f:shrinker_intf
;;

let try_include_decl
  type_decl_list
  ~loc
  ~incl_generator
  ~incl_observer
  ~incl_shrinker
  ~portable_export
  =
  match type_decl_list with
  | [ type_decl ] ->
    let has_contravariant_arg =
      List.exists type_decl.ptype_params ~f:(fun (_type, (variance, _inj)) ->
        match variance with
        | Contravariant -> true
        | NoVariance | Covariant -> false)
    in
    if (portable_export && not (List.is_empty type_decl.ptype_params))
       || has_contravariant_arg
       || not (incl_generator && incl_observer && incl_shrinker)
    then None
    else (
      let sg_name = "Ppx_quickcheck_runtime.Quickcheckable.S" in
      mk_named_sig ~loc ~sg_name ~handle_polymorphic_variant:true type_decl_list
      |> Option.map ~f:(fun include_info ->
        Ppxlib_jane.Ast_builder.Default.psig_include
          ~loc
          ~modalities:
            (if portable_export
             then [ Loc.make ~loc (Ppxlib_jane.Modality "portable") ]
             else [])
          include_info))
  | _ ->
    (* Don't bother testing anything since [mk_named_sig] will definitely return
       [None] anyway *)
    None
;;

let args () =
  Deriving.Args.(
    empty +> flag "generator" +> flag "observer" +> flag "shrinker" +> flag "portable")
;;

let flags ~incl_generator ~incl_observer ~incl_shrinker =
  if not (incl_generator || incl_observer || incl_shrinker)
  then (* If no flags are provided, include everything. *)
    true, true, true
  else incl_generator, incl_observer, incl_shrinker
;;

let create
  ~incl_generator
  ~incl_observer
  ~incl_shrinker
  ~make_generator_list
  ~make_observer_list
  ~make_shrinker_list
  decls
  =
  List.concat
    [ (if incl_generator then make_generator_list decls else [])
    ; (if incl_observer then make_observer_list decls else [])
    ; (if incl_shrinker then make_shrinker_list decls else [])
    ]
;;

let sig_type_decl ~portable =
  Deriving.Generator.make
    (args ())
    (fun
        ~loc
        ~path:_
        (_, decls)
        incl_generator
        incl_observer
        incl_shrinker
        portable_export
      ->
       let portable_export = portable_export || portable in
       let incl_generator, incl_observer, incl_shrinker =
         flags ~incl_generator ~incl_observer ~incl_shrinker
       in
       let items =
         match
           try_include_decl
             ~loc
             ~incl_generator
             ~incl_observer
             ~incl_shrinker
             decls
             ~portable_export
         with
         | Some decl -> [ decl ]
         | None ->
           create
             ~incl_generator
             ~incl_observer
             ~incl_shrinker
             ~make_generator_list:(generator_intf_list ~portable_export)
             ~make_observer_list:(observer_intf_list ~portable_export)
             ~make_shrinker_list:(shrinker_intf_list ~portable_export)
             decls
       in
       Ppx_template.Export.Monomorphize.t#signature_items
         Ppx_template.Export.Monomorphize.Context.top
         items)
;;

let str_type_decl ~portable =
  Deriving.Generator.make
    (args ())
    (fun
        ~loc
        ~path:_
        (rec_flag, decls)
        incl_generator
        incl_observer
        incl_shrinker
        portable_export
      ->
       let portable_export = portable_export || portable in
       let rec_flag = really_recursive rec_flag decls in
       let incl_generator, incl_observer, incl_shrinker =
         flags ~incl_generator ~incl_observer ~incl_shrinker
       in
       create
         ~incl_generator
         ~incl_observer
         ~incl_shrinker
         ~make_generator_list:(generator_impl_list ~rec_flag ~loc ~portable_export)
         ~make_observer_list:(observer_impl_list ~rec_flag ~loc ~portable_export)
         ~make_shrinker_list:(shrinker_impl_list ~rec_flag ~loc ~portable_export)
         decls
       |> Ppx_template.Export.Monomorphize.t#structure
            Ppx_template.Export.Monomorphize.Context.top)
;;

let generator_extension ~portable:portable_value ~loc:_ ~path:_ core_type =
  generator_of_core_type
    core_type
    ~gen_env:Environment.empty
    ~obs_env:Environment.empty
    ~portable_value
  |> Ppx_template.Export.Monomorphize.t#expression
       Ppx_template.Export.Monomorphize.Context.top
;;

let observer_extension ~portable:portable_value ~loc:_ ~path:_ core_type =
  observer_of_core_type
    core_type
    ~obs_env:Environment.empty
    ~gen_env:Environment.empty
    ~portable_value
  |> Ppx_template.Export.Monomorphize.t#expression
       Ppx_template.Export.Monomorphize.Context.top
;;

let shrinker_extension ~portable:portable_value ~loc:_ ~path:_ core_type =
  shrinker_of_core_type core_type ~env:Environment.empty ~portable_value
  |> Ppx_template.Export.Monomorphize.t#expression
       Ppx_template.Export.Monomorphize.Context.top
;;
