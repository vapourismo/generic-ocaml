open Utils

module type S = Signatures.Sum

module type Maker = Signatures.SumMaker

module Make (Wrapper : Wrappers.S) : S with type 'a wrapper = 'a Wrapper.t = struct
  type 'a wrapper = 'a Wrapper.t

  type _ t =
    | Zero : 'a Wrapper.t -> ('a * 'b) t
    | Succ : 'b t -> ('a * 'b) t

  let zero x = Zero x

  let succ x = Succ x

  let ( !+ ) x = succ x

  type 'r unpacker = { unpack : 'x. 'x wrapper -> 'r }

  let unpack (type r) (unpacker : r unpacker) value =
    let rec go : type xs. xs t -> r =
     fun value ->
       match value with
       | Zero x -> unpacker.unpack x
       | Succ xs -> go xs
    in
    go value
  ;;

  type ('xs, 'r) folder =
    { on_zero : 'y 'ys. ('xs, 'y * 'ys) refl -> 'y wrapper -> 'r
    ; on_succ : 'y 'ys. ('xs, 'y * 'ys) refl -> ('ys, 'r) folder
    }

  let rec fold : type xs r. (xs, r) folder -> xs t -> r =
   fun folder value ->
     match value with
     | Zero x -> folder.on_zero Refl x
     | Succ x -> fold (folder.on_succ Refl) x
 ;;

  let tag value =
    let rec go : type xs. int -> xs t -> int =
     fun acc value ->
       match value with
       | Zero _ -> acc
       | Succ tail -> go (acc + 1) tail
    in
    go 0 value
  ;;
end

module MakeCompact (Wrapper : Wrappers.S) : S with type 'a wrapper = 'a Wrapper.t = struct
  type 'a wrapper = 'a Wrapper.t

  type _ t = Sum : int * 'a wrapper -> 'xs t

  let zero x = Sum (0, x)

  let succ (Sum (tag, value)) = Sum (tag + 1, value)

  let ( !+ ) x = succ x

  type 'r unpacker = { unpack : 'x. 'x wrapper -> 'r }

  let unpack unpacker (Sum (_, value)) = unpacker.unpack value

  type ('xs, 'r) folder =
    { on_zero : 'y 'ys. ('xs, 'y * 'ys) refl -> 'y wrapper -> 'r
    ; on_succ : 'y 'ys. ('xs, 'y * 'ys) refl -> ('ys, 'r) folder
    }

  let fold (type r) folder (Sum (tag, value)) =
    let rec reduce : type xs. int -> (xs, r) folder -> r =
     fun tag folder ->
       if tag > 0
       then reduce (tag - 1) (folder.on_succ (Obj.magic Refl))
       else folder.on_zero (Obj.magic Refl) value
    in
    reduce tag folder
  ;;

  let tag (Sum (tag, _)) = tag
end
