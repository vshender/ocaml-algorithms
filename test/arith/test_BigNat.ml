open Algorithms_arith.BigNat

(* {{{ Util
   ----------------------------------------------------------------------------
*)

(** The implementation of [Alcotest.testable] for [BigNat.t]. *)
let bignat_testable = Alcotest.testable
    (fun fmt x -> Fmt.string fmt (to_string x))
    equal

(** The implementation of [QCheck2.Gen.t] for [BigNat.t]. *)
let bignat_gen =
  let open QCheck2.Gen in
  (string_of numeral) >|= fun s -> if s = "" then zero else of_string s

(** [unop_bignat_test f fname x expected] generates a test case that checks
    that [f x = expected].
    [x] and [expected] are string representations of big natural numbers.

    Example: [unop_bignat_test succ "succ" "42" "43"]. *)
let unop_bignat_test f fname x expected =
  let open Alcotest in
  test_case
    (Fmt.str "%s %s" fname x)
    `Quick
    (fun () ->
       let expected = of_string expected in
       check
         bignat_testable
         (Fmt.str "%s %s = %a" fname x (pp bignat_testable) expected)
         expected
         (f (of_string x)))

(** [binop_any_test f fname expected_testable x y expected] generates a test
    case that checks that [f x y = expected].
    [x] and [y] are string representations of big natural numbers,
    [expected_testable] is an [Alcotest.testable] for [expected].

    Example: [binop_any_test compare "compare" Alcotest.int "42" "42" 0]. *)
let binop_any_test f fname expected_testable x y expected =
  let open Alcotest in
  test_case
    (Fmt.str "%s %s %s" fname x y)
    `Quick
    (fun () ->
       check
         expected_testable
         (Fmt.str "%s %s %s = %a" fname x y (pp expected_testable) expected)
         expected
         (f (of_string x) (of_string y)))

(** [binop_bignat_test f fname x y expected] generates a test case that checks
    that [f x y = expected].
    [x], [y], and [expected] are string representations of big natural
    numbers.

    Example: [binop_bignat_test add "add" "42" "27" "69"]. *)
let binop_bignat_test f fname x y expected =
  binop_any_test f fname bignat_testable x y (of_string expected)

(** [remove_leading_zeros s] removes all leading zeros from [s].
    If [s] consists only of zeros, the function returns ["0"].  This function
    is useful for normalizing string representations of numbers. *)
let remove_leading_zeros s =
  s
  |> String.to_seq
  |> Seq.drop_while (( = ) '0')
  |> String.of_seq
  |> (fun s -> if String.length s = 0 then "0" else s)

(** [is_num s] checks that [s] is a valid representation of a natural
    number. *)
let is_num s =
  String.(length s > 0 && for_all (fun c -> '0' <= c && c <= '9') s)

(* }}} *)

(* {{{ Comparisons
   ----------------------------------------------------------------------------
*)

(** Test cases for [compare]. *)
let compare_tests =
  let compare_test = binop_any_test compare "compare" Alcotest.int
  in [
    compare_test "0" "0" 0;
    compare_test "0" "1" (-1);
    compare_test "1" "0" 1;
    compare_test "1" "1" 0;
    compare_test "42" "999999999999999999999999999" (-1);
    compare_test "999999999999999999999999999" "42" 1;
    compare_test "999999999999999999999999998" "999999999999999999999999999" (-1);
    compare_test "999999999999999999999999999" "999999999999999999999999998" 1;
    compare_test "999999999999999999999999999" "999999999999999999999999999" 0;
  ]


(** Test cases for [equal]. *)
let equal_tests =
  let equal_test = binop_any_test equal "equal" Alcotest.bool
  in [
    equal_test "0" "0" true;
    equal_test "0" "1" false;
    equal_test "1" "0" false;
    equal_test "1" "1" true;
    equal_test "42" "999999999999999999999999999" false;
    equal_test "999999999999999999999999999" "42" false;
    equal_test "999999999999999999999999998" "999999999999999999999999999" false;
    equal_test "999999999999999999999999999" "999999999999999999999999998" false;
    equal_test "999999999999999999999999999" "999999999999999999999999999" true;
  ]

(** Test cases for [min]. *)
let min_tests =
  let min_test = binop_bignat_test min "min"
  in [
    min_test "0" "0" "0";
    min_test "0" "1" "0";
    min_test "1" "0" "0";
    min_test "1" "1" "1";
    min_test "42" "999999999999999999999999999" "42";
    min_test "999999999999999999999999999" "42" "42";
    min_test "999999999999999999999999998" "999999999999999999999999999" "999999999999999999999999998";
    min_test "999999999999999999999999999" "999999999999999999999999998" "999999999999999999999999998";
    min_test "999999999999999999999999999" "999999999999999999999999999" "999999999999999999999999999";
  ]

(** Test cases for [max]. *)
let max_tests =
  let max_test = binop_bignat_test max "max"
  in [
    max_test "0" "0" "0";
    max_test "0" "1" "1";
    max_test "1" "0" "1";
    max_test "1" "1" "1";
    max_test "42" "999999999999999999999999999" "999999999999999999999999999";
    max_test "999999999999999999999999999" "42" "999999999999999999999999999";
    max_test "999999999999999999999999998" "999999999999999999999999999" "999999999999999999999999999";
    max_test "999999999999999999999999999" "999999999999999999999999998" "999999999999999999999999999";
    max_test "999999999999999999999999999" "999999999999999999999999999" "999999999999999999999999999";
  ]

(* }}} *)

(* {{{ Hash function
   ----------------------------------------------------------------------------
*)

(** Check that [hash] returns the same value for two equal natural numbers,
    regardless of the internal representation of these numbers (the test uses
    the implementation details knowledge that the array of "digits" of a
    natural number can contain leading zeros). *)
let hash_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"hash x = hash y if x = y"
    ~count:50
    ~print:to_string
    bignat_gen
    (fun x ->
       (* Get a natural number equal to [x], but with a high probability of
          containing leading zeros. *)
       let c = of_string "999999999999999999999999999" in
       let y = div (mul x c) c in

       hash x = hash y)

(* }}} *)

(* {{{ Arithmetic operations
   ----------------------------------------------------------------------------
*)

(** Test cases for [add]. *)
let add_tests =
  let add_test = binop_bignat_test add "add"
  in [
    add_test "0" "0" "0";
    add_test "0" "1" "1";
    add_test "1" "0" "1";
    add_test "1" "1" "2";
    add_test "11111111111" "99999999999" "111111111110";
    add_test "99999999999999999999999999" "1" "100000000000000000000000000";
    add_test "100000000000000000000000000" "1" "100000000000000000000000001";
    add_test
      "1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "1234567891111111111000000000888888888890";
    add_test
      "987654321987654321987654321000" "1234567890123456789012345678901234567890"
      "1234567891111111111000000000888888888890";
    add_test "999999999999999999999999999999" "123456789012345678901234567890" "1123456789012345678901234567889";
  ]

(** Test cases for [sub]. *)
let sub_tests =
  let sub_test = binop_bignat_test sub "sub"
  in [
    sub_test "0" "0" "0";
    sub_test "1" "0" "1";
    sub_test "111111111110" "11111111111" "99999999999";
    sub_test "100000000000" "9" "99999999991";
    sub_test "1000000000000000000000000000000000000000" "1" "999999999999999999999999999999999999999";
    sub_test
      "1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "1234567889135802467024691356913580246890";
    sub_test "999999999999999999999999999999" "123456789012345678901234567890" "876543210987654321098765432109";
  ]

(** Check that subtracting a larger natural number from a smaller one is an
    error .*)
let sub_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"sub x y is error when x < y"
    ~count:50
    ~print:Print.(tup2 to_string to_string)
    Gen.(tup2 bignat_gen bignat_gen)
    (fun (x, y) ->
       assume (compare x y < 0);
       try
         ignore (sub x y);
         false
       with
         Failure _ -> true)

(** Check that [sub (add x y) y = x]. *)
let add_and_sub_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"sub (add x y) y = x"
    ~count:250
    ~print:Print.(tup2 to_string to_string)
    Gen.(tup2 bignat_gen bignat_gen)
    (fun (x, y) ->
       equal (sub (add x y) y) x)

(** Check that [add (sub x y) y = x] if [x >= y]. *)
let sub_and_add_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"add (sub x y) y = x"
    ~count:250
    ~print:Print.(tup2 to_string to_string)
    Gen.(tup2 bignat_gen bignat_gen)
    (fun (x, y) ->
       assume (compare x y >= 0);
       equal (add (sub x y) y) x)

(** Test cases for [mul]. *)
let mul_tests =
  let mul_test = binop_bignat_test mul "mul"
  in [
    mul_test "0" "0" "0";
    mul_test "0" "1" "0";
    mul_test "1" "0" "0";
    mul_test "1" "1" "1";
    mul_test "1" "2" "2";
    mul_test "2" "2" "4";
    mul_test "42" "10" "420";
    mul_test "42" "27" "1134";
    mul_test "9999999" "9999999" "99999980000001";
    mul_test "11111111111" "99999999999" "1111111111088888888889";
    mul_test
      "1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "1219326312467611633592440164485596706448437738013609205901126352690000";
    mul_test
      "987654321987654321987654321000" "1234567890123456789012345678901234567890"
      "1219326312467611633592440164485596706448437738013609205901126352690000";
    mul_test
      "999999999999999999999999999999" "123456789012345678901234567890"
      "123456789012345678901234567889876543210987654321098765432110";
    mul_test
      "12345678901234567890123456789012345678901234567890" "98765432109876543210987654321098765432109876543210"
      "1219326311370217952261850327338667885945115073915611949397448712086533622923332237463801111263526900";
  ]

(** Test data for [div], [rem], and [divmod]. *)
let divmod_test_data = [
  ("0", "1", "0", "0");
  ("1", "1", "1", "0");
  ("1", "2", "0", "1");
  ("2", "1", "2", "0");
  ("2", "2", "1", "0");
  ("2", "3", "0", "2");
  ("4", "2", "2", "0");
  ("4", "3", "1", "1");
  ("4", "4", "1", "0");
  ("4", "5", "0", "4");
  ("420", "10", "42", "0");
  ("421", "10", "42", "1");
  ("429", "10", "42", "9");
  ("1134", "27", "42", "0");
  ("1160", "27", "42", "26");
  ("99999980000001", "9999999", "9999999", "0");
  ("99999989999999", "9999999", "9999999", "9999998");
  ("1111111111088888888889", "99999999999", "11111111111", "0");
  ("1111111111999999999999", "99999999999", "11111111120", "11111111119");
  ("1111111111088888888889", "11111111111", "99999999999", "0");
  ("1111111111999999999999", "11111111111", "100000000081", "8");
  (
    "1219326312467611633592440164485596706448437738013609205901126352690000",
    "1234567890123456789012345678901234567890",
    "987654321987654321987654321000", "0"
  );
  (
    "1219326312467611633592440164486213990393499466408115378740576969973945",
    "1234567890123456789012345678901234567890",
    "987654321987654321987654321000", "617283945061728394506172839450617283945"
  );
  (
    "1219326312467611633592440164486831274338561194802621551580027587257889",
    "1234567890123456789012345678901234567890",
    "987654321987654321987654321000", "1234567890123456789012345678901234567889"
  );
  (
    "1219326312467611633592440164485596706448437738013609205901126352690000", "987654321987654321987654321000",
    "1234567890123456789012345678901234567890", "0"
  );
  (
    "1219326312467611633592440164485596706448931565174603033062120179850500", "987654321987654321987654321000",
    "1234567890123456789012345678901234567890", "493827160993827160993827160500"
  );
  (
    "1219326312467611633592440164485596706449425392335596860223114007010999", "987654321987654321987654321000",
    "1234567890123456789012345678901234567890", "987654321987654321987654320999"
  );
  (
    "123456789012345678901234567889876543210987654321098765432110", "999999999999999999999999999999",
    "123456789012345678901234567890", "0"
  );
  (
    "123456789012345678901234567890376543210987654321098765432109", "999999999999999999999999999999",
    "123456789012345678901234567890", "499999999999999999999999999999"
  );
  (
    "123456789012345678901234567890876543210987654321098765432108", "999999999999999999999999999999",
    "123456789012345678901234567890", "999999999999999999999999999998"
  );
  (
    "123456789012345678901234567890876543210987654321098765432109", "999999999999999999999999999999",
    "123456789012345678901234567891", "0"
  );
  (
    "1219326311370217952261850327338667885945115073915611949397448712086533622923332237463801111263526900",
    "12345678901234567890123456789012345678901234567890",
    "98765432109876543210987654321098765432109876543210", "0"
  );
  (
    "1219326311370217952261850327338667885945115073915618122236899329370478684651726743636640561880810845",
    "12345678901234567890123456789012345678901234567890",
    "98765432109876543210987654321098765432109876543210", "6172839450617283945061728394506172839450617283945"
  );
  (
    "1219326311370217952261850327338667885945115073915624295076349946654423746380121249809480012498094789",
    "12345678901234567890123456789012345678901234567890",
    "98765432109876543210987654321098765432109876543210", "12345678901234567890123456789012345678901234567889"
  );
  (
    "1219326311370217952261850327338667885945115073915624295076349946654423746380121249809480012498094790",
    "12345678901234567890123456789012345678901234567890",
    "98765432109876543210987654321098765432109876543211", "0"
  );
]

(** Test cases for [div]. *)
let div_tests =
  let div_test = binop_bignat_test div "div"
  in List.map (fun (x, y, q, _) -> div_test x y q) divmod_test_data

(** Check that dividing any number by zero is an error. *)
let div_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"div x 0 is error"
    ~count:20
    ~print:to_string
    bignat_gen
    (fun x ->
       try
         ignore (div x zero);
         false
       with
         Division_by_zero -> true)

(** Check that [div (mul x y) y = x] if [y <> 0]. *)
let mul_and_div_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"div (mul x y) y = x"
    ~count:250
    ~print:Print.(tup2 to_string to_string)
    Gen.(tup2 bignat_gen bignat_gen)
    (fun (x, y) ->
       assume (not (equal y zero));
       equal (div (mul x y) y) x)

(** Test cases for [rem]. *)
let rem_tests =
  let rem_test = binop_bignat_test rem "rem"
  in List.map (fun (x, y, _, r) -> rem_test x y r) divmod_test_data

(** Check that dividing any number by zero is an error. *)
let rem_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"rem x 0 is error"
    ~count:20
    ~print:to_string
    bignat_gen
    (fun x ->
       try
         ignore (rem x zero);
         false
       with
         Division_by_zero -> true)

(** Test cases for [divmod]. *)
let divmod_tests =
  let divmod_test x y (q, r) = binop_any_test
      divmod "divmod"
      (Alcotest.pair bignat_testable bignat_testable)
      x y
      (of_string q, of_string r)
  in List.map (fun (x, y, q, r) -> divmod_test x y (q, r)) divmod_test_data

(** Check that dividing any number by zero is an error. *)
let divmod_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"divmod x 0 is error"
    ~count:20
    ~print:to_string
    bignat_gen
    (fun x ->
       try
         ignore (divmod x zero);
         false
       with
         Division_by_zero -> true)

(** Check that [add (mul q y) r = x] where [(q, r) = divmod x y] if
    [y <> 0]. *)
let divmod_and_mul_add_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"add (mul q y) r = x where (q, r) = divmod x y"
    ~count:250
    ~print:Print.(tup2 to_string to_string)
    Gen.(tup2 bignat_gen bignat_gen)
    (fun (x, y) ->
       assume (not (equal y zero));
       let d, m = divmod x y in
       equal (add (mul d y) m) x)

(** Test cases for [succ]. *)
let succ_tests =
  let succ_test = unop_bignat_test succ "succ"
  in [
    succ_test "0" "1";
    succ_test "1" "2";
    succ_test "9" "10";
    succ_test "10" "11";
    succ_test "42" "43";
    succ_test "9999" "10000";
    succ_test "10000" "10001";
    succ_test "999999999" "1000000000";
    succ_test "1000000000" "1000000001";
    succ_test "999999999999999999999999999" "1000000000000000000000000000";
  ]

(** Test cases for [pred]. *)
let pred_tests =
  let pred_test = unop_bignat_test pred "pred"
  in [
    pred_test "1" "0";
    pred_test "2" "1";
    pred_test "10" "9";
    pred_test "11" "10";
    pred_test "43" "42";
    pred_test "10000" "9999";
    pred_test "10001" "10000";
    pred_test "1000000000" "999999999";
    pred_test "1000000001" "1000000000";
    pred_test "1000000000000000000000000000" "999999999999999999999999999";
  ]

(* }}} *)

(* {{{ Conversions to and from other numerical types
   ----------------------------------------------------------------------------
*)

(** Check that [of_int i] is error if [i < 0]. *)
let of_int_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int i is error if i < 0 "
    ~count:20
    ~print:Print.int
    Gen.int
    (fun i ->
       assume (i < 0);
       try
         ignore (of_int i);
         false
       with Failure _ ->
         true)

(** Check that [to_int x] is error if [x] can not be represented as a built-in
    integer. *)
let to_int_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int x is error if x can not be represented as a built-in int"
    ~count:20
    ~print:to_string
    bignat_gen
    (fun x ->
       assume (not (is_int x));
       try
         ignore (to_int x);
         false
       with Failure _ ->
         true)

(** Check that [to_int_opt x = None] if [x] can not be represented as a
    built-in integer. *)
let to_int_opt_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int_opt x = None if x can not be represented as a built-in int"
    ~count:20
    ~print:to_string
    bignat_gen
    (fun x ->
       assume (not (is_int x));
       to_int_opt x = None)

(** Check that [of_int (to_int x) = x] if [x] is representable as a built-in
    integer. *)
let to_int_and_of_int_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_int (to_int x) = x"
    ~count:250
    ~print:to_string
    bignat_gen
    (fun x ->
       assume (is_int x);
       equal (of_int (to_int x)) x)

(** Check that [to_int (of_int i) = i] if [i >= 0]. *)
let of_int_and_to_int_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int (of_int i) = i"
    ~count:250
    ~print:Print.int
    Gen.int
    (fun i ->
       assume (i >= 0);
       to_int (of_int i) = i)

(** Check that [of_int (Option.get @@ to_int_opt x) = x] if [x] is
    representable as a built-in integer. *)
let to_int_opt_and_of_int_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_int (Option.get @@ to_int_opt x) = x"
    ~count:250
    ~print:to_string
    bignat_gen
    (fun x ->
       assume (is_int x);
       equal (of_int (Option.get @@ to_int_opt x)) x)

(** Check that [to_int_opt (of_int i) = Some i] if [i >= 0]. *)
let of_int_and_to_int_opt_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int_opt (of_int i) = Some i"
    ~count:250
    ~print:Print.int
    Gen.int
    (fun i ->
       assume (i >= 0);
       to_int_opt (of_int i) = Some i)

(* }}} *)

(* {{{ Conversions to and from strings
   ----------------------------------------------------------------------------
*)

(** Check that [of_string s] is error if [s] is not a valid representation of a
    natural number. *)
let of_string_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_string s is error if s is not a valid number representation"
    ~count:20
    ~print:Print.string
    Gen.string
    (fun s ->
       assume (not @@ is_num s);
       try
         ignore (of_string s);
         false
       with Failure _ ->
         true)

(** Check that [of_string_opt s = None] if [s] is not a valid representation of
    a natural number. *)
let of_string_opt_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_string_opt s = None if s is not a valid number representation"
    ~count:20
    ~print:Print.string
    Gen.string
    (fun s ->
       assume (not @@ is_num s);
       of_string_opt s = None)

(** Check that [of_string (to_string x) = x]. *)
let to_string_and_of_string_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_string (to_string x) = x"
    ~count:250
    ~print:to_string
    bignat_gen
    (fun x ->
       equal (of_string (to_string x)) x)

(** Check that [to_string (of_string s) = s] (up to leading zeros) if [s] is a
    valid representation of a natural number. *)
let of_string_and_to_string_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_string (of_string s) = s"
    ~count:250
    ~print:Print.string
    Gen.(string_of numeral)
    (fun s ->
       assume (s <> "");
       to_string (of_string s) = remove_leading_zeros s)

(** Check that [of_string_opt (to_string x) = Some x]. *)
let to_string_and_of_string_opt_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_string_opt (to_string x) = Some x"
    ~count:250
    ~print:to_string
    bignat_gen
    (fun x ->
       equal (Option.get @@ of_string_opt (to_string x)) x)

(** Check that [to_string (Option.get @@ of_string_opt s) = s] (up to leading
    zeros) if [s] is a valid representation of a natural number. *)
let of_string_opt_and_to_string_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_string (Option.get @@ of_string_opt s) = s"
    ~count:250
    ~print:Print.string
    Gen.(string_of numeral)
    (fun s ->
       assume (s <> "");
       to_string (Option.get @@ of_string_opt s) = remove_leading_zeros s)

(* }}} *)

let () =
  Alcotest.run ~compact:true "BigNat" [
    "compare", compare_tests;
    "equal", equal_tests;
    "min", min_tests;
    "max", max_tests;
    "hash", [hash_test];
    "add", add_tests;
    "sub", sub_tests;
    "sub failure", [sub_fail_test];
    "add, sub", [add_and_sub_are_inverse_ops_test];
    "sub, add", [sub_and_add_are_inverse_ops_test];
    "mul", mul_tests;
    "div", div_tests;
    "div failure", [div_fail_test];
    "mul, div", [mul_and_div_are_inverse_ops_test];
    "rem", rem_tests;
    "rem failure", [rem_fail_test];
    "divmod", divmod_tests;
    "divmod failure", [divmod_fail_test];
    "divmod, mul, add", [divmod_and_mul_add_are_inverse_ops_test];
    "succ", succ_tests;
    "pred", pred_tests;
    "of_int failure", [of_int_fail_test];
    "to_int failure", [to_int_fail_test];
    "to_int_opt failure", [to_int_opt_fail_test];
    "to_int, of_int", [to_int_and_of_int_are_inverse_ops_test];
    "of_int, to_int", [of_int_and_to_int_are_inverse_ops_test];
    "to_int_opt, of_int", [to_int_opt_and_of_int_are_inverse_ops_test];
    "of_int, to_int_opt", [of_int_and_to_int_opt_are_inverse_ops_test];
    "of_string failure", [of_string_fail_test];
    "of_string_opt failure", [of_string_opt_fail_test];
    "to_string, of_string", [to_string_and_of_string_are_inverse_ops_test];
    "of_string, to_string", [of_string_and_to_string_are_inverse_ops_test];
    "to_string, of_string_opt", [to_string_and_of_string_opt_are_inverse_ops_test];
    "of_string_opt, to_string", [of_string_opt_and_to_string_are_inverse_ops_test];
  ]
