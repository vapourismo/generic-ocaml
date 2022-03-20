module type S = Signatures.Adt

module type Maker = Signatures.AdtMaker

module Make (Field : Wrappers.S) : S with type 'a field = 'a Field.t = struct
  type 'a field = 'a Field.t

  module Constructor = Product.Make (Field)
  include Sum.Make (Constructor)
end

module MakeCompact (Field : Wrappers.S) : S with type 'a field = 'a Field.t = struct
  type 'a field = 'a Field.t

  module Constructor = Product.MakeCompact (Field)
  include Sum.MakeCompact (Constructor)
end
