module type S = Signatures.ADT

module type Maker = Signatures.ADTMaker

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
