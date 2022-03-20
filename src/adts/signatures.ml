open Utils

module type NatTrans = sig
  (** Source functor *)
  type 'a src

  (** Destination functor *)
  type 'a dest
end

(** Sum type *)
module type Sum = sig
  (** Wrapper type for elements in the sum *)
  type _ wrapper

  (** Sum type *)
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

(** Maker of a Sum module *)
module type SumMaker = functor (Wrapper : Wrappers.S) ->
  Sum with type 'a wrapper = 'a Wrapper.t

(** Product type *)
module type Product = sig
  (** Wrapper type for elements in the product *)
  type _ wrapper

  (** Product type *)
  type _ t

  val nil : void t

  val cons : 'a wrapper -> 'b t -> ('a * 'b) t

  val ( ** ) : 'a wrapper -> 'b t -> ('a * 'b) t

  type ('xs, 'r) folder =
    { on_nil : ('xs, void) refl -> 'r
    ; on_cons : 'y 'ys. ('xs, 'y * 'ys) refl -> 'y wrapper -> ('ys, 'r) folder
    }

  val fold : ('xs, 'r) folder -> 'xs t -> 'r

  type any = Any : 'a wrapper -> any

  module Unsafe : sig
    val of_any_array : any array -> 'xs t
  end
end

(** Maker of a Product module *)
module type ProductMaker = functor (Wrapper : Wrappers.S) ->
  Product with type 'a wrapper = 'a Wrapper.t

(** Extended Product type *)
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

(** Maker of an extended Product module *)
module type ProductExtMaker = functor (Wrapper : Wrappers.S) ->
  ProductExt with type 'a wrapper = 'a Wrapper.t
