module%template Ppx_template_interop = struct
  [@@@kind.default k = (bits64, value)]

  type ('a : k) t = { box : 'a }

  let quickcheck_generator g =
    Base_quickcheck.Generator.Via_thunk.map g ~f:(fun a () -> { box = a () })
  ;;
end
