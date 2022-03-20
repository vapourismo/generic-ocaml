open Utils

module Make (Wrapper : Wrappers.S) : Signatures.Sum with type 'a wrapper = 'a Wrapper.t =
struct
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
end
