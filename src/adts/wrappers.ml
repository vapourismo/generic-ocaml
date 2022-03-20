module type S = sig
  type _ t
end

module Unit = struct
  type 'a t = unit
end

module Identity = struct
  type 'a t = 'a
end

module List = struct
  type 'a t = 'a list
end

module Array = struct
  type 'a t = 'a array
end

module To (T : sig
  type t
end) =
struct
  type 'a t = 'a -> T.t
end
