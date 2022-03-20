open Utils

module type S = Signatures.ProductExt

module Make (Wrapper : Wrappers.S) : S with type 'a wrapper = 'a Wrapper.t = struct
  type 'a wrapper = 'a Wrapper.t

  type _ t =
    | Nil : void t
    | Cons : 'a wrapper * 'b t -> ('a * 'b) t

  let nil = Nil

  let cons x xs = Cons (x, xs)

  let ( ** ) x xs = cons x xs

  module MakeNatTrans (Dest : Signatures.Product) = struct
    type 'a src = 'a t

    type 'a dest = 'a Dest.t

    type 'r handler = { run : 'x. 'x wrapper -> 'x Dest.wrapper }

    let map handler value =
      let rec go : type xs r. xs t -> (xs Dest.t -> r) -> r =
       fun value k ->
         match value with
         | Nil -> k Dest.nil
         | Cons (x, xs) -> go xs (fun xs -> k (Dest.cons (handler.run x) xs))
      in
      go value Fun.id
    ;;
  end

  module MakeSelect (Sum : Signatures.Sum) = struct
    type 'a handler = { run : 'x. 'x Sum.wrapper -> 'x wrapper -> 'a }

    let make_folder (type r) (handler : r handler) =
      let rec go : type xs. xs t -> (xs, r) Sum.folder =
       fun value ->
         match value with
         | Cons (x, xs) ->
           let on_zero : type y ys. (xs, y * ys) refl -> y Sum.wrapper -> r =
            fun Refl wrapper -> handler.run wrapper x
           in
           let on_succ : type y ys. (xs, y * ys) refl -> (ys, r) Sum.folder =
            fun Refl -> go xs
           in
           { Sum.on_zero; Sum.on_succ }
         | Nil ->
           (* In theory this can't happen because you can't construct a [void Sum.t].
            We could prove this to the compiler by only accepting [('z * 'zs) t] products.
            However, this only works for the top call because we have to prove this inductively
            given this function is recursive.
         *)
           failwith "Select with empty sum!"
      in
      go
    ;;

    let select handler sum product = Sum.fold (make_folder handler product) sum
  end
end
