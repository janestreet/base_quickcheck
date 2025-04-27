open! Base

let quickcheck_generator_unit = Generator.unit
let quickcheck_generator_bool = Generator.bool
let quickcheck_generator_char = Generator.char
let quickcheck_generator_string = Generator.string
let quickcheck_generator_bytes = Generator.bytes
let quickcheck_generator_int = Generator.int
let quickcheck_generator_int32 = Generator.int32
let quickcheck_generator_int64 = Generator.int64
let quickcheck_generator_nativeint = Generator.nativeint
let quickcheck_generator_float = Generator.float
let quickcheck_observer_unit = Observer.unit
let quickcheck_observer_bool = Observer.bool
let quickcheck_observer_char = Observer.char
let quickcheck_observer_string = Observer.string
let quickcheck_observer_bytes = Observer.bytes
let quickcheck_observer_int = Observer.int
let quickcheck_observer_int32 = Observer.int32
let quickcheck_observer_int64 = Observer.int64
let quickcheck_observer_nativeint = Observer.nativeint
let quickcheck_observer_float = Observer.float
let quickcheck_shrinker_unit = Shrinker.unit
let quickcheck_shrinker_bool = Shrinker.bool
let quickcheck_shrinker_char = Shrinker.char
let quickcheck_shrinker_string = Shrinker.string
let quickcheck_shrinker_bytes = Shrinker.bytes
let quickcheck_shrinker_int = Shrinker.int
let quickcheck_shrinker_int32 = Shrinker.int32
let quickcheck_shrinker_int64 = Shrinker.int64
let quickcheck_shrinker_nativeint = Shrinker.nativeint
let quickcheck_shrinker_float = Shrinker.float

[%%template
[@@@mode.default p = (portable, nonportable)]

let quickcheck_generator_option = (Generator.option [@mode p])
let quickcheck_generator_list = (Generator.list [@mode p])
let quickcheck_generator_array = (Generator.array [@mode p])
let quickcheck_generator_ref = (Generator.ref [@mode p])
let quickcheck_generator_lazy_t = (Generator.lazy_t [@mode p])
let quickcheck_observer_option = (Observer.option [@mode p])
let quickcheck_observer_list = (Observer.list [@mode p])
let quickcheck_observer_array = (Observer.array [@mode p])
let quickcheck_observer_ref = (Observer.ref [@mode p])
let quickcheck_observer_lazy_t = (Observer.lazy_t [@mode p])
let quickcheck_shrinker_option = (Shrinker.option [@mode p])
let quickcheck_shrinker_list = (Shrinker.list [@mode p])
let quickcheck_shrinker_array = (Shrinker.array [@mode p])
let quickcheck_shrinker_ref = (Shrinker.ref [@mode p])
let quickcheck_shrinker_lazy_t = (Shrinker.lazy_t [@mode p])]
