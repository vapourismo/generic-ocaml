module Product = Product
module Sum = Sum
module Adt = Adt
module Wrappers = Wrappers

(**
  Natural transformation shape

  Modules of this type determine the [src] and [dest] for a natural transformation
  ['x. 'x src -> 'x dest].
*)
module type NatTrans = Signatures.NatTrans

include Utils
