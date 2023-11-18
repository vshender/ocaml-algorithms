open Algorithms_search

(** [find_in_array_test (module OT) pp_elem arr x expected] generates a test
    case that checks that [find_in_array (module OT) arr x = expected].
    [pp_elem] is a pretty-printer for elements of [arr].

    Example: [find_in_array_test (module Int) Fmt.Int [| 1; 2; 3 |] 1 (Some 0)].
*)
let find_in_array_test
    (type a)
    (module OT : OrderedType.S with type t = a) pp_elem arr x expected =
  let open Alcotest in
  test_case
    Fmt.(str "find_in_array %a %a" (Dump.array pp_elem) arr pp_elem x)
    `Quick
    (fun () ->
       check
         (option int)
         Fmt.(str "find_in_array %a %a = %a"
                (Dump.array pp_elem) arr pp_elem x (Dump.option int) expected)
         expected
         (LinearSearch.find_in_array (module OT) arr x))

(** [find_in_list_test (module OT) pp_elem lst x expected] generates a test
    case that checks that [find_in_list (module OT) lst x = expected].
    [pp_elem] is a pretty-printer for elements of [lst].

    Example: [find_in_list_test (module Int) Fmt.Int [ 1; 2; 3 ] 1 (Some 0)].
*)
let find_in_list_test
    (type a)
    (module OT : OrderedType.S with type t = a) pp_elem lst x expected =
  let open Alcotest in
  test_case
    Fmt.(str "find_in_list %a %a" (Dump.list pp_elem) lst pp_elem x)
    `Quick
    (fun () ->
       check
         (option int)
         Fmt.(str "find_in_list %a %a = %a"
                (Dump.list pp_elem) lst pp_elem x (Dump.option int) expected)
         expected
         (LinearSearch.find_in_list (module OT) lst x))

(** Test cases for [find_in_array] on an array of integers. *)
let find_int_in_array_tests =
  let find_int_in_array_test = find_in_array_test (module Int) Fmt.int
  in [
    find_int_in_array_test [| |] 42 None;
    find_int_in_array_test [| 0; 0; 1; 2; 1 |] 1 (Some 2);
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 1 (Some 0);
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 2 (Some 1);
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 3 (Some 2);
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 4 (Some 3);
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 5 (Some 4);
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 0 None;
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 6 None;
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 42 None;
    find_int_in_array_test [| 4; 3; 1; 5; 2 |] 1 (Some 2);
    find_int_in_array_test [| 4; 3; 1; 5; 2 |] 2 (Some 4);
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 0 None;
    find_int_in_array_test [| 1; 2; 3; 4; 5 |] 6 None;
  ]

(** Test cases for [find_in_array] on an array of strings. *)
let find_string_in_array_tests =
  let find_string_in_array_test = find_in_array_test (module String) Fmt.Dump.string
  in [
    find_string_in_array_test [| "one"; "two"; "three" |] "one" (Some 0);
    find_string_in_array_test [| "one"; "two"; "three" |] "two" (Some 1);
    find_string_in_array_test [| "one"; "two"; "three" |] "three" (Some 2);
    find_string_in_array_test [| "one"; "two"; "three" |] "zero" None;
    find_string_in_array_test [| "one"; "two"; "three" |] "forty two" None;
  ]

(** Test cases for [find_in_list] on a list of ints. *)
let find_int_in_list_tests =
  let find_int_in_list_test = find_in_list_test (module Int) Fmt.int
  in [
    find_int_in_list_test [ ] 42 None;
    find_int_in_list_test [ 0; 0; 1; 2; 1 ] 1 (Some 2);
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 1 (Some 0);
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 2 (Some 1);
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 3 (Some 2);
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 4 (Some 3);
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 5 (Some 4);
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 0 None;
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 6 None;
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 42 None;
    find_int_in_list_test [ 4; 3; 1; 5; 2 ] 1 (Some 2);
    find_int_in_list_test [ 4; 3; 1; 5; 2 ] 2 (Some 4);
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 0 None;
    find_int_in_list_test [ 1; 2; 3; 4; 5 ] 6 None;
  ]

(** Test cases for [find_in_list] on a list of strings. *)
let find_string_in_list_tests =
  let find_string_in_list_test = find_in_list_test (module String) Fmt.Dump.string
  in [
    find_string_in_list_test [ "one"; "two"; "three" ] "one" (Some 0);
    find_string_in_list_test [ "one"; "two"; "three" ] "two" (Some 1);
    find_string_in_list_test [ "one"; "two"; "three" ] "three" (Some 2);
    find_string_in_list_test [ "one"; "two"; "three" ] "zero" None;
    find_string_in_list_test [ "one"; "two"; "three" ] "forty two" None;
  ]

(** Check that [find_in_array (module Int) arr x = Some _] if [x] in [arr]. *)
let find_existing_element_in_array_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"find_in_array (module Int) arr x = Some _ if x in arr"
    ~count:50
    ~print:Fmt.(str "%a" (Dump.array int))
    Gen.(array int)
    (fun arr ->
       Array.for_all
         (fun x -> LinearSearch.find_in_array (module Int) arr x |> Option.is_some)
         arr)

(** Check that [find_in_array (module Int) arr x = None] if [x] not in [arr]. *)
let find_non_existent_element_in_array_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"find_in_array (module Int) arr x = None if x not in arr"
    ~count:250
    ~print:Fmt.(str "%a" (Dump.pair (Dump.array int) int))
    Gen.(pair (array int) int)
    (fun (arr, x) ->
       assume (not (Array.mem x arr));
       LinearSearch.find_in_array (module Int) arr x |> Option.is_none)

(** Check that [find_in_list (module Int) lst x = Some _] if [x] in [lst]. *)
let find_existing_element_in_list_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"find_in_list (module Int) lst x = Some _ if x in lst"
    ~count:50
    ~print:Fmt.(str "%a" (Dump.list int))
    Gen.(list int)
    (fun lst ->
       List.for_all
         (fun x -> LinearSearch.find_in_list (module Int) lst x |> Option.is_some)
         lst)

(** Check that [find_in_list (module Int) lst x = None] if [x] not in [lst]. *)
let find_non_existent_element_in_list_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"find_in_list (module Int) lst x = None if x not in lst"
    ~count:250
    ~print:Fmt.(str "%a" (Dump.pair (Dump.list int) int))
    Gen.(pair (list int) int)
    (fun (lst, x) ->
       assume (not (List.mem x lst));
       LinearSearch.find_in_list (module Int) lst x |> Option.is_none)

let () =
  Alcotest.run ~compact:true "LinearSearch" [
    "find int in array", find_int_in_array_tests;
    "find string in array", find_string_in_array_tests;
    "find int in list", find_int_in_list_tests;
    "find string in list", find_string_in_list_tests;
    "find existing element in array", [find_existing_element_in_array_test];
    "find non-existent element in array", [find_non_existent_element_in_array_test];
    "find existing element in list", [find_existing_element_in_list_test];
    "find non-existent element in list", [find_non_existent_element_in_list_test];
  ]
