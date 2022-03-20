open Alcotest
open Adts

let test_product_select
    (module MakeProduct : Product.Maker)
    (module MakeSum : Sum.Maker)
    ()
  =
  let module P =
    MakeProduct (Wrappers.Const (struct
      type t = int
    end))
  in
  let module S = MakeSum (Wrappers.Unit) in
  let module Select = P.MakeSelect (S) in
  let handler : int Select.handler =
    let run : type x. x S.wrapper -> x P.wrapper -> int = fun () i -> i in
    { run }
  in
  let product = P.(13 ** 37 ** 73 ** 31 ** nil) in
  let sum = S.(zero ()) in
  let value = Select.select handler sum product in
  check int "must be equal" 13 value;
  let sum = S.(succ @@ succ @@ zero ()) in
  let value = Select.select handler sum product in
  check int "must be equal" 73 value
;;

let test_product_fold (module Make : Product.Maker) () =
  let module P =
    Make (Wrappers.Const (struct
      type t = int
    end))
  in
  let value = P.(13 ** 37 ** 73 ** 31 ** nil) in
  let rec folder : type xs. int -> (xs, int) P.folder =
   fun accum ->
     let on_nil _refl = accum in
     let on_cons : type y ys. (xs, y * ys) refl -> y P.wrapper -> (ys, int) P.folder =
      fun Refl value -> folder (accum + value)
     in
     { P.on_nil; P.on_cons }
  in
  let sum = P.fold (folder 0) value in
  check int "sums must be equal" 154 sum
;;

let test_sum_tag (module Make : Sum.Maker) () =
  let module S = Make (Wrappers.Unit) in
  let sum = S.zero () in
  check int "tag zero must be 0" 0 (S.tag sum);
  let sum = S.succ sum in
  check int "tag succ must be 1" 1 (S.tag sum);
  let sum = S.succ sum in
  check int "tag succ succ must be 2" 2 (S.tag sum)
;;

(**************************************************************************************************)

module type Flavoured = sig
  val flavour : string
end

let flavoured_test_case (module F : Flavoured) name =
  test_case (Printf.sprintf "%s (%s)" name F.flavour) `Quick
;;

let flavoured_test_case_2 (module F : Flavoured) (module G : Flavoured) name =
  test_case (Printf.sprintf "%s (%s, %s)" name F.flavour G.flavour) `Quick
;;

let product_makers : (string * (module Product.Maker)) list =
  [ "normal", (module Product.Make); "compact", (module Product.MakeCompact) ]
;;

module type MakeProduct = sig
  module Make : Product.Maker

  include Flavoured
end

let product_tests (f : (module MakeProduct) -> 'a) =
  let module W =
    Wrappers.Const (struct
      type t = int
    end)
  in
  List.map
    (fun (flavour, (module Make : Product.Maker)) ->
      let module P = struct
        module Make = Make

        let flavour = flavour
      end
      in
      f (module P))
    product_makers
;;

let sum_makers : (string * (module Sum.Maker)) list =
  [ "normal", (module Sum.Make); "compact", (module Sum.MakeCompact) ]
;;

module type MakeSum = sig
  module Make : Sum.Maker

  include Flavoured
end

let sum_tests (f : (module MakeSum) -> 'a) =
  List.map
    (fun (flavour, (module Make : Sum.Maker)) ->
      let module S = struct
        module Make = Make

        let flavour = flavour
      end
      in
      f (module S))
    sum_makers
;;

(**************************************************************************************************)

let product_select_tests =
  List.concat
  @@ product_tests
  @@ fun (module Product) ->
  sum_tests
  @@ fun (module Sum) ->
  flavoured_test_case_2
    (module Product)
    (module Sum)
    "select"
    (test_product_select (module Product.Make) (module Sum.Make))
;;

let product_fold_tests =
  product_tests
  @@ fun (module P) ->
  flavoured_test_case (module P) "fold" (test_product_fold (module P.Make))
;;

let product_tests = product_select_tests @ product_fold_tests

let sum_tests =
  sum_tests
  @@ fun (module S) -> flavoured_test_case (module S) "tag" (test_sum_tag (module S.Make))
;;

let () = run "adts" [ "product", product_tests; "sum", sum_tests ]
