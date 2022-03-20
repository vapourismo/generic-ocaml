module type S = Signatures.Product

module type Maker = Signatures.ProductMaker

module Make : Maker

module MakeCompact : Maker

module Utils (Product : S) : sig
  type 'r extractor = { extract : 'x. 'x Product.wrapper -> 'r }

  val fold_left : 'r extractor -> 'r -> ('r -> 'r -> 'r) -> 'xs Product.t -> 'r
end
