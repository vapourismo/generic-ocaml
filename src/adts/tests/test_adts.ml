open Alcotest
open Adts

module type Flavoured = sig
  val flavour : string
end

let flavoured_test_case (module F : Flavoured) name =
  test_case (Printf.sprintf "%s (%s)" name F.flavour) `Quick
;;

let flavoured_test_case_2 (module F : Flavoured) (module G : Flavoured) name =
  test_case (Printf.sprintf "%s (%s, %s)" name F.flavour G.flavour) `Quick
;;

module type IntProduct = sig
  include Product.S with type 'a wrapper = int

  include Flavoured
end

let product_makers : (string * (module Product.Maker)) list =
  [ "normal", (module Product.Make); "compact", (module Product.MakeCompact) ]
;;

let int_product_tests (f : (module IntProduct) -> 'a) =
  let module W =
    Wrappers.Const (struct
      type t = int
    end)
  in
  List.map
    (fun (flavour, (module Maker : Product.Maker)) ->
      let module P = struct
        include Maker (W)

        let flavour = flavour
      end
      in
      f (module P))
    product_makers
;;

module type UnitSum = sig
  include Sum.S with type 'a wrapper = unit

  include Flavoured
end

let sum_makers : (string * (module Sum.Maker)) list =
  [ "normal", (module Sum.Make); "compact", (module Sum.MakeCompact) ]
;;

let unit_sum_tests (f : (module UnitSum) -> 'a) =
  List.map
    (fun (flavour, (module Maker : Sum.Maker)) ->
      let module S = struct
        include Maker (Wrappers.Unit)

        let flavour = flavour
      end
      in
      f (module S))
    sum_makers
;;

let test_product_select (module P : IntProduct) (module S : UnitSum) () =
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

let test_product_fold (module P : IntProduct) () =
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

let test_sum_tag (module S : UnitSum) () =
  let sum = S.zero () in
  check int "tag zero must be 0" 0 (S.tag sum);
  let sum = S.succ sum in
  check int "tag succ must be 1" 1 (S.tag sum);
  let sum = S.succ sum in
  check int "tag succ succ must be 2" 2 (S.tag sum)
;;

let product_select_tests =
  List.concat
  @@ int_product_tests
  @@ fun (module P) ->
  unit_sum_tests
  @@ fun (module S) ->
  flavoured_test_case_2
    (module P)
    (module S)
    "select"
    (test_product_select (module P) (module S))
;;

let product_fold_tests =
  int_product_tests
  @@ fun (module P) ->
  flavoured_test_case (module P) "fold" (test_product_fold (module P))
;;

let product_tests = product_select_tests @ product_fold_tests

let sum_tests =
  unit_sum_tests
  @@ fun (module S) -> flavoured_test_case (module S) "tag" (test_sum_tag (module S))
;;

let () = run "adts" [ "product", product_tests; "sum", sum_tests ]
