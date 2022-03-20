module type S = Signatures.Product

module type Maker = Signatures.ProductMaker

module Make : Maker

module MakeCompact : Maker
