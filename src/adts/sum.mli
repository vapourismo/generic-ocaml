module type S = Signatures.Sum

module type Maker = Signatures.SumMaker

module Make : Maker

module MakeCompact : Maker
