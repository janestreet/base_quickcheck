module%template Ppx_template_interop : sig
  type 'a t [@@kind k = (bits64, value)] [@@deriving quickcheck ~generator]
end
