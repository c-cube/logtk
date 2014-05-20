
open Logtk
open OUnit

module M = Multiset.Make(struct
  type t = int
  let compare i j=Pervasives.compare i j
end)

let f x y =
  if x = y then Comparison.Eq
  else if x < y then Comparison.Lt
  else Comparison.Gt

let test_max () =
  let m = M.of_list [1;2;2;3;1] in
  assert_equal (M.of_list [3]) (M.max f m)

let test_compare () =
  let m1 = M.of_list [1;1;2;3] in
  let m2 = M.of_list [1;2;2;3] in
  assert_equal ~printer:Comparison.to_string Comparison.Lt (M.compare f m1 m2);
  ()

let test_cardinal_size () =
  let m = M.of_coeffs [1, Z.(~$ 2); 3, Z.(~$ 40)] in
  assert_equal 2 (M.size m);
  assert_equal ~cmp:Z.equal ~printer:Z.to_string Z.(~$ 42) (M.cardinal m);
  ()

let suite =
  "test_multiset" >:::
    [ "max" >:: test_max
    ; "compare" >:: test_compare
    ; "cardinal_size" >:: test_cardinal_size
    ]
