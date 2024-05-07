## Release v0.17.0

- Add Bigarray distribution functions to `Generator`:
  * `Generator.bigarray1`
  * `Generator.bigstring_with_length`
  * `Generator.float32_vec_with_length`
  * `Generator.float64_vec_with_length`

- Update the type of `Generator.create` and `Generator.generate` to use `Splittable_random.t` instead of `Splittable_random.State.t`. The former is simply a shorter alias for the latter.

- Remove `[@@deriving fields]` from `Test.Config.t` to reduce bloat.

- Add flags for individual components to `ppx_quickcheck` e.g. `[@@deriving quickcheck ~generator ~observer ~shrinker]`.

## Release v0.16.0

- Add new geometric distributions for integer types to `Generator`:
  * All functions take a minimum value and a probability `p` as parameters, producing a
    geometric distribution
  * Raise an error if `p <. 0. || 1. <. p`

- Add `string_like` function to `Generator`:
  * Produces strings similar to a given input with some number of edits
