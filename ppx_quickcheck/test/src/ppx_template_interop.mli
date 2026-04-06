module%template Ppx_template_interop : sig
  type ('a : k) t [@@kind k = (bits64, value)] [@@deriving quickcheck ~generator]
end
