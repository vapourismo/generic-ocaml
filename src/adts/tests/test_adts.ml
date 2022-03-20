open Alcotest
open Adts

let test_fast_product_fold () =
  let module P =
    Product.MakeFast (Adts.Wrappers.Const (struct
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
  check int "sums must be equal" sum 154
;;

let fast_product_tests = [ test_case "test_fast_fold" `Quick test_fast_product_fold ]

let () = run "adts" [ "fast_product", fast_product_tests ]
