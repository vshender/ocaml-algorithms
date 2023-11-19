open Algorithms_search

(** [find_test (module OT) pp_elem arr x expected] generates a test case that
    checks that [find (module OT) arr x = expected].  [pp_elem] is a
    pretty-printer for elements of [arr].

    Example: [find (module Int) Fmt.Int [| 1; 2; 3 |] 1 (Some 0)].
*)
let find_test
    (type a)
    (module OT : OrderedType.S with type t = a) pp_elem arr x expected =
  let open Alcotest in
  test_case
    Fmt.(str "find %a %a" (Dump.array pp_elem) arr pp_elem x)
    `Quick
    (fun () ->
       check
         (option int)
         Fmt.(str "find %a %a = %a"
                (Dump.array pp_elem) arr pp_elem x (Dump.option int) expected)
         expected
         (BinarySearch.find (module OT) arr x))

(** Test cases for [find] on an array of integers. *)
let find_int_tests =
  let find_int_test = find_test (module Int) Fmt.int
  in [
    find_int_test [| |] 42 None;
    find_int_test [| 7 |] 42 None;
    find_int_test [| 7 |] 7 (Some 0);
    find_int_test [| 1; 2 |] 0 None;
    find_int_test [| 1; 2 |] 1 (Some 0);
    find_int_test [| 1; 2 |] 2 (Some 1);
    find_int_test [| 1; 2 |] 3 None;
    find_int_test [| 1; 2; 3 |] 0 None;
    find_int_test [| 1; 2; 3 |] 1 (Some 0);
    find_int_test [| 1; 2; 3 |] 2 (Some 1);
    find_int_test [| 1; 2; 3 |] 3 (Some 2);
    find_int_test [| 1; 2; 3 |] 4 None;
    find_int_test [| 1; 2; 3; 4; 5 |] 1 (Some 0);
    find_int_test [| 1; 2; 3; 4; 5 |] 2 (Some 1);
    find_int_test [| 1; 2; 3; 4; 5 |] 3 (Some 2);
    find_int_test [| 1; 2; 3; 4; 5 |] 4 (Some 3);
    find_int_test [| 1; 2; 3; 4; 5 |] 5 (Some 4);
    find_int_test [| 1; 2; 3; 4; 5 |] 0 None;
    find_int_test [| 1; 2; 3; 4; 5 |] 6 None;
    find_int_test [| 1; 2; 3; 4; 5 |] 42 None;
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 0 None;
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 1 (Some 0);
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 10 None;
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 12 (Some 1);
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 13 None;
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 14 (Some 2);
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 15 None;
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 16 (Some 3);
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 17 None;
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 19 (Some 4);
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 21 None;
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 23 (Some 5);
    find_int_test [| 1; 12; 14; 16; 19; 23 |] 27 None;
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 0 None;
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 1 (Some 0);
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 2 None;
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 3 (Some 1);
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 5 None;
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 7 (Some 2);
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 12 None;
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 13 (Some 3);
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 15 None;
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 17 (Some 4);
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 20 None;
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 23 (Some 5);
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 25 None;
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 27 (Some 6);
    find_int_test [| 1; 3; 7; 13; 17; 23; 27 |] 42 None;
  ]

(** Test cases for [find] on an array of strings. *)
let find_string_tests =
  let find_string_test = find_test (module String) Fmt.Dump.string
  in [
    find_string_test [| "one"; "three"; "two" |] "one" (Some 0);
    find_string_test [| "one"; "three"; "two" |] "two" (Some 2);
    find_string_test [| "one"; "three"; "two" |] "three" (Some 1);
    find_string_test [| "one"; "three"; "two" |] "zero" None;
    find_string_test [| "one"; "three"; "two" |] "forty two" None;
  ]

(** Check that [find (module Int) arr x = Some _] if [x] in [arr]. *)
let find_existing_element_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"find (module Int) arr x = Some _ if x in arr"
    ~count:50
    ~print:Fmt.(str "%a" (Dump.array int))
    Gen.(array int)
    (fun arr ->
       Array.sort compare arr;
       Array.for_all
         (fun x -> BinarySearch.find (module Int) arr x |> Option.is_some)
         arr)

(** Check that [find (module Int) arr x = None] if [x] not in [arr]. *)
let find_non_existent_element_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"find (module Int) arr x = None if x not in arr"
    ~count:250
    ~print:Fmt.(str "%a" (Dump.pair (Dump.array int) int))
    Gen.(pair (array int) int)
    (fun (arr, x) ->
       assume (not (Array.mem x arr));
       BinarySearch.find (module Int) arr x |> Option.is_none)

let () =
  Alcotest.run ~compact:true "BinarySearch" [
    "find int in array", find_int_tests;
    "find string in array", find_string_tests;
    "find existing element", [find_existing_element_test];
    "find non-existent element", [find_non_existent_element_test];
  ]
