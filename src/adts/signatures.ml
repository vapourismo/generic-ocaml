open Utils

module type NatTrans = sig
  type 'a src

  type 'a dest
end

module type Sum = sig
  type _ wrapper

  type _ t

  val zero : 'a wrapper -> ('a * 'b) t

  val succ : 'b t -> ('a * 'b) t

  val ( !+ ) : 'b t -> ('a * 'b) t

  type 'r unpacker = { unpack : 'x. 'x wrapper -> 'r }

  val unpack : 'r unpacker -> 'xs t -> 'r

  type ('xs, 'r) folder =
    { on_zero : 'y 'ys. ('xs, 'y * 'ys) refl -> 'y wrapper -> 'r
    ; on_succ : 'y 'ys. ('xs, 'y * 'ys) refl -> ('ys, 'r) folder
    }

  val fold : ('xs, 'r) folder -> 'xs t -> 'r
end

module type SumMaker = functor (Wrapper : Wrappers.S) ->
  Sum with type 'a wrapper = 'a Wrapper.t

module type Product = sig
  type _ wrapper

  type _ t

  val nil : void t

  val cons : 'a wrapper -> 'b t -> ('a * 'b) t

  val ( ** ) : 'a wrapper -> 'b t -> ('a * 'b) t
end

module type ProductMaker = functor (Wrapper : Wrappers.S) ->
  Product with type 'a wrapper = 'a Wrapper.t

module type ProductExt = sig
  include Product

  module MakeNatTrans (Dest : Product) : sig
    type 'a src = 'a t

    type 'a dest = 'a Dest.t

    type 'r handler = { run : 'x. 'x wrapper -> 'x Dest.wrapper }

    val map : 'a handler -> 'xs src -> 'xs dest
  end

  module MakeSelect (Sum : Sum) : sig
    type 'a handler = { run : 'x. 'x Sum.wrapper -> 'x wrapper -> 'a }

    val select : 'a handler -> 'xs Sum.t -> 'xs t -> 'a
  end
end

module type ProductExtMaker = functor (Wrapper : Wrappers.S) ->
  ProductExt with type 'a wrapper = 'a Wrapper.t
