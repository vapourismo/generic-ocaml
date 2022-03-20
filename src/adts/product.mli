module type S = Signatures.ProductExt

module type Maker = Signatures.ProductExtMaker

module Make : Maker

module MakeCompact : Maker
