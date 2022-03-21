module type S = Signatures.Adt

module type Maker = Signatures.AdtMaker

module Make (ConstructorWrapper : Wrappers.S) (FieldWrapper : Wrappers.S) :
  S
    with type 'a field = 'a FieldWrapper.t
     and type 'a constructor = 'a ConstructorWrapper.t = struct
  type 'a field = 'a FieldWrapper.t

  module Constructor = Product.Make (FieldWrapper)

  type 'a constructor = 'a ConstructorWrapper.t

  include Sum.Make (struct
    type 'a t = 'a Constructor.t constructor
  end)
end

module MakeCompact (ConstructorWrapper : Wrappers.S) (FieldWrapper : Wrappers.S) :
  S
    with type 'a field = 'a FieldWrapper.t
     and type 'a constructor = 'a ConstructorWrapper.t = struct
  type 'a field = 'a FieldWrapper.t

  module Constructor = Product.MakeCompact (FieldWrapper)

  type 'a constructor = 'a ConstructorWrapper.t

  include Sum.MakeCompact (struct
    type 'a t = 'a Constructor.t constructor
  end)
end
