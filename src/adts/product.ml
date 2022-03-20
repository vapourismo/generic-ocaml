open Utils

module type S = Signatures.Product

module type Maker = Signatures.ProductMaker

module Make (Wrapper : Wrappers.S) : S with type 'a wrapper = 'a Wrapper.t = struct
  type 'a wrapper = 'a Wrapper.t

  type _ t =
    | Nil : void t
    | Cons : 'a wrapper * 'b t -> ('a * 'b) t

  let nil = Nil

  let cons x xs = Cons (x, xs)

  let ( ** ) x xs = cons x xs

  type ('xs, 'r) folder =
    { on_nil : ('xs, void) refl -> 'r
    ; on_cons : 'y 'ys. ('xs, 'y * 'ys) refl -> 'y wrapper -> ('ys, 'r) folder
    }

  let rec fold : type xs r. (xs, r) folder -> xs t -> r =
   fun folder value ->
     match value with
     | Nil -> folder.on_nil Refl
     | Cons (x, xs) -> fold (folder.on_cons Refl x) xs
 ;;

  type any = Any : 'a wrapper -> any

  module Unsafe = struct
    let of_any_array values =
      let result =
        Array.fold_right (fun (Any l) r -> Obj.magic (cons l r)) values (Obj.magic Nil)
      in
      result
    ;;
  end

  module MakeNatTrans (Dest : Signatures.ProductBase) = struct
    type 'a src = 'a t

    type 'a dest = 'a Dest.t

    type mapper = { run : 'x. 'x wrapper -> 'x Dest.wrapper }

    let map mapper value =
      let rec go : type xs r. xs t -> (xs Dest.t -> r) -> r =
       fun value k ->
         match value with
         | Nil -> k Dest.nil
         | Cons (x, xs) -> go xs (fun xs -> k (Dest.cons (mapper.run x) xs))
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

module MakeCompact (Wrapper : Wrappers.S) : S with type 'a wrapper = 'a Wrapper.t = struct
  type 'a wrapper = 'a Wrapper.t

  type any = Any : 'a wrapper -> any

  type _ t = any array

  let nil = Array.of_list []

  let cons x xs = Array.append (Array.make 1 (Any x)) xs

  let ( ** ) = cons

  type ('xs, 'r) folder =
    { on_nil : ('xs, void) refl -> 'r
    ; on_cons : 'y 'ys. ('xs, 'y * 'ys) refl -> 'y wrapper -> ('ys, 'r) folder
    }

  let fold : type xs r. (xs, r) folder -> xs t -> r =
   fun folder values ->
     let length = Array.length values in
     let rec go : type ys. int -> (ys, r) folder -> r =
      fun offset folder ->
        if offset >= length
        then folder.on_nil (Obj.magic Refl)
        else (
          let (Any wrapper) = Array.unsafe_get values offset in
          go (offset + 1) (folder.on_cons (Obj.magic Refl) wrapper))
     in
     go 0 folder
 ;;

  module Unsafe = struct
    let of_any_array xs = xs
  end

  module MakeNatTrans (Dest : Signatures.ProductBase) = struct
    type 'a src = 'a t

    type 'a dest = 'a Dest.t

    type mapper = { run : 'x. 'x wrapper -> 'x Dest.wrapper }

    let map mapper values =
      Dest.Unsafe.of_any_array
        (Array.map (fun (Any wrapper) -> Dest.Any (mapper.run wrapper)) values)
    ;;
  end

  module MakeSelect (Sum : Signatures.Sum) = struct
    type 'a handler = { run : 'x. 'x Sum.wrapper -> 'x wrapper -> 'a }

    let select handler sum product =
      let (Any prod_wrapper) = Array.unsafe_get product (Sum.tag sum) in
      Sum.unpack
        { unpack = (fun sum_wrapper -> handler.run sum_wrapper (Obj.magic prod_wrapper)) }
        sum
    ;;
  end
end
