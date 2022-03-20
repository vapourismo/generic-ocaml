module type S = Signatures.ADT

module type Maker = Signatures.ADTMaker

module Make : Maker

module MakeCompact : Maker
