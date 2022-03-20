module type S = sig
  type _ t
end

module Unit : S with type 'a t = unit = struct
  type 'a t = unit
end

module Identity : S with type 'a t = 'a = struct
  type 'a t = 'a
end

module List : S with type 'a t = 'a list = struct
  type 'a t = 'a list
end

module Array : S with type 'a t = 'a array = struct
  type 'a t = 'a array
end

module To (T : sig
  type t
end) : S with type 'a t = 'a -> T.t = struct
  type 'a t = 'a -> T.t
end

module Const (T : sig
  type t
end) : S with type 'a t = T.t = struct
  type 'a t = T.t
end
