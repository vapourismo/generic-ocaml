open Alcotest
open Adts

let test_compact_product_fold () =
  let module P =
    Product.MakeCompact (Adts.Wrappers.Const (struct
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

let compact_product_tests =
  [ test_case "test_compact_fold" `Quick test_compact_product_fold ]
;;

let () = run "adts" [ "compact_product", compact_product_tests ]
