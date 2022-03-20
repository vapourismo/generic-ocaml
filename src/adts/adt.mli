module type S = Signatures.Adt

module type Maker = Signatures.AdtMaker

module Make : Maker

module MakeCompact : Maker
