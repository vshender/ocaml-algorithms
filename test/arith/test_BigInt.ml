open Algorithms_arith
open Algorithms_arith.BigInt
open Util

(* {{{ Util
   ----------------------------------------------------------------------------
*)

(** The implementation of [Alcotest.testable] for [BigInt.t]. *)
let bigint_testable = Alcotest.testable
    (fun fmt x -> Fmt.string fmt (to_string x))
    equal

(** [unop_any_test f fname expected_testable x expected] generates a test case
    that checks that [f x = expected].
    [x] is a string representation of a big integer, [expected_testable] is an
    [Alcotest.testable] for [expected].

    Example: [unop_any_test sign "sign" Alcotest.int "-42" -1]. *)
let unop_any_test f fname expected_testable x expected =
  let open Alcotest in
  test_case
    (Fmt.str "%s %s" fname x)
    `Quick
    (fun () ->
       check
         expected_testable
         (Fmt.str "%s %s %a" fname x (pp expected_testable) expected)
         expected
         (f (of_string x)))

(** [unop_bigint_test f fname x expected] generates a test case that checks
    that [f x = expected].
    [x] and [expected] are string representations of big integers.

    Example: [unop_bigint_test succ "succ" "42" "43"]. *)
let unop_bigint_test f fname x expected =
  unop_any_test f fname bigint_testable x (of_string expected)

(** [binop_any_test f fname expected_testable x y expected] generates a test
    case that checks that [f x y = expected].
    [x] and [y] are string representations of big integers, [expected_testable]
    is an [Alcotest.testable] for [expected].

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

(** [binop_bigint_test f fname x y expected] generates a test case that checks
    that [f x y = expected].
    [x], [y], and [expected] are string representations of big integers.

    Example: [binop_bigint_test add "add" "42" "27" "69"]. *)
let binop_bigint_test f fname x y expected =
  binop_any_test f fname bigint_testable x y (of_string expected)

(** [is_num s] checks that [s] is a valid representation of an integer. *)
let is_num s =
  let open String in
  let s =
    if s <> "" && s.[0] = '-' then sub s 1 (length s - 1)
    else s
  in s <> "" && for_all (fun c -> '0' <= c && c <= '9') s

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
    compare_test "0" "-1" 1;
    compare_test "1" "0" 1;
    compare_test "1" "1" 0;
    compare_test "1" "-1" 1;
    compare_test "-1" "0" (-1);
    compare_test "-1" "1" (-1);
    compare_test "-1" "-1" 0;
    compare_test "42" "999999999999999999999999999" (-1);
    compare_test "42" "-999999999999999999999999999" 1;
    compare_test "999999999999999999999999999" "42" 1;
    compare_test "-999999999999999999999999999" "42" (-1);
    compare_test "999999999999999999999999998" "999999999999999999999999999" (-1);
    compare_test "999999999999999999999999999" "999999999999999999999999998" 1;
    compare_test "999999999999999999999999999" "999999999999999999999999999" 0;
    compare_test "-999999999999999999999999998" "-999999999999999999999999999" 1;
    compare_test "-999999999999999999999999999" "-999999999999999999999999998" (-1);
    compare_test "-999999999999999999999999999" "-999999999999999999999999999" 0;
  ]

(** Test cases for [equal]. *)
let equal_tests =
  let equal_test = binop_any_test equal "equal" Alcotest.bool
  in [
    equal_test "0" "0" true;
    equal_test "0" "1" false;
    equal_test "0" "-1" false;
    equal_test "1" "0" false;
    equal_test "1" "1" true;
    equal_test "1" "-1" false;
    equal_test "-1" "0" false;
    equal_test "-1" "1" false;
    equal_test "-1" "-1" true;
    equal_test "42" "999999999999999999999999999" false;
    equal_test "42" "-999999999999999999999999999" false;
    equal_test "999999999999999999999999999" "42" false;
    equal_test "-999999999999999999999999999" "42" false;
    equal_test "999999999999999999999999998" "999999999999999999999999999" false;
    equal_test "999999999999999999999999999" "999999999999999999999999998" false;
    equal_test "999999999999999999999999999" "999999999999999999999999999" true;
    equal_test "-999999999999999999999999998" "-999999999999999999999999999" false;
    equal_test "-999999999999999999999999999" "-999999999999999999999999998" false;
    equal_test "-999999999999999999999999999" "-999999999999999999999999999" true;
  ]

(** Test cases for [min]. *)
let min_tests =
  let min_test = binop_bigint_test min "min"
  in [
    min_test "0" "0" "0";
    min_test "0" "1" "0";
    min_test "0" "-1" "-1";
    min_test "1" "0" "0";
    min_test "1" "1" "1";
    min_test "1" "-1" "-1";
    min_test "-1" "0" "-1";
    min_test "-1" "1" "-1";
    min_test "-1" "-1" "-1";
    min_test "42" "999999999999999999999999999" "42";
    min_test "-42" "999999999999999999999999999" "-42";
    min_test "42" "-999999999999999999999999999" "-999999999999999999999999999";
    min_test "-42" "-999999999999999999999999999" "-999999999999999999999999999";
    min_test "999999999999999999999999999" "42" "42";
    min_test "-999999999999999999999999999" "42" "-999999999999999999999999999";
    min_test "999999999999999999999999998" "999999999999999999999999999" "999999999999999999999999998";
    min_test "999999999999999999999999999" "999999999999999999999999998" "999999999999999999999999998";
    min_test "999999999999999999999999999" "999999999999999999999999999" "999999999999999999999999999";
    min_test "-999999999999999999999999998" "-999999999999999999999999999" "-999999999999999999999999999";
    min_test "-999999999999999999999999999" "-999999999999999999999999998" "-999999999999999999999999999";
    min_test "-999999999999999999999999999" "-999999999999999999999999999" "-999999999999999999999999999";
  ]

(** Test cases for [max]. *)
let max_tests =
  let max_test = binop_bigint_test max "max"
  in [
    max_test "0" "0" "0";
    max_test "0" "1" "1";
    max_test "0" "-1" "0";
    max_test "1" "0" "1";
    max_test "1" "1" "1";
    max_test "1" "-1" "1";
    max_test "-1" "0" "0";
    max_test "-1" "1" "1";
    max_test "-1" "-1" "-1";
    max_test "42" "999999999999999999999999999" "999999999999999999999999999";
    max_test "-42" "999999999999999999999999999" "999999999999999999999999999";
    max_test "42" "-999999999999999999999999999" "42";
    max_test "-42" "-999999999999999999999999999" "-42";
    max_test "999999999999999999999999999" "42" "999999999999999999999999999";
    max_test "-999999999999999999999999999" "42" "42";
    max_test "999999999999999999999999998" "999999999999999999999999999" "999999999999999999999999999";
    max_test "999999999999999999999999999" "999999999999999999999999998" "999999999999999999999999999";
    max_test "999999999999999999999999999" "999999999999999999999999999" "999999999999999999999999999";
    max_test "-999999999999999999999999998" "-999999999999999999999999999" "-999999999999999999999999998";
    max_test "-999999999999999999999999999" "-999999999999999999999999998" "-999999999999999999999999998";
    max_test "-999999999999999999999999999" "-999999999999999999999999999" "-999999999999999999999999999";
  ]

(* }}} *)

(* {{{ Arithmetic operations
   ----------------------------------------------------------------------------
*)

(** Test cases for [sign]. *)
let sign_tests =
  let sign_test = unop_any_test sign "sign" Alcotest.int
  in [
    sign_test "-42" (-1);
    sign_test "-1" (-1);
    sign_test "0" 0;
    sign_test "1" 1;
    sign_test "42" 1;
  ]

(** Test cases for [neg]. *)
let neg_tests =
  let neg_test = unop_bigint_test neg "neg"
  in [
    neg_test "-42" "42";
    neg_test "-1" "1";
    neg_test "0" "0";
    neg_test "1" "-1";
    neg_test "42" "-42";
  ]

(** Test cases for [abs]. *)
let abs_tests =
  let abs_test = unop_bigint_test abs "abs"
  in [
    abs_test "-42" "42";
    abs_test "-1" "1";
    abs_test "0" "0";
    abs_test "1" "1";
    abs_test "42" "42";
  ]

(** Test cases for [add]. *)
let add_tests =
  let add_test = binop_bigint_test add "add"
  in [
    add_test "0" "0" "0";
    add_test "0" "1" "1";
    add_test "0" "-1" "-1";
    add_test "1" "0" "1";
    add_test "1" "1" "2";
    add_test "1" "-1" "0";
    add_test "-1" "0" "-1";
    add_test "-1" "1" "0";
    add_test "-1" "-1" "-2";
    add_test "4" "5" "9";
    add_test "5" "4" "9";
    add_test "-4" "5" "1";
    add_test "-5" "4" "-1";
    add_test "4" "-5" "-1";
    add_test "5" "-4" "1";
    add_test "-4" "-5" "-9";
    add_test "-5" "-4" "-9";
    add_test "11111111111" "99999999999" "111111111110";
    add_test "-11111111111" "99999999999" "88888888888";
    add_test "11111111111" "-99999999999" "-88888888888";
    add_test "-11111111111" "-99999999999" "-111111111110";
    add_test "99999999999999999999999999" "1" "100000000000000000000000000";
    add_test "99999999999999999999999999" "-1" "99999999999999999999999998";
    add_test "-100000000000000000000000000" "1" "-99999999999999999999999999";
    add_test
      "1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "1234567891111111111000000000888888888890";
    add_test
      "1234567890123456789012345678901234567890" "-987654321987654321987654321000"
      "1234567889135802467024691356913580246890";
    add_test
      "-1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "-1234567889135802467024691356913580246890";
    add_test "-1234567890123456789012345678901234567890" "-987654321987654321987654321000"
      "-1234567891111111111000000000888888888890";
    add_test
      "987654321987654321987654321000" "1234567890123456789012345678901234567890"
      "1234567891111111111000000000888888888890";
    add_test
      "987654321987654321987654321000" "-1234567890123456789012345678901234567890"
      "-1234567889135802467024691356913580246890";
    add_test "999999999999999999999999999999" "123456789012345678901234567890" "1123456789012345678901234567889";
    add_test "999999999999999999999999999999" "-123456789012345678901234567890" "876543210987654321098765432109";
  ]

(** Test cases for [sub]. *)
let sub_tests =
  let sub_test = binop_bigint_test sub "sub"
  in [
    sub_test "0" "0" "0";
    sub_test "0" "1" "-1";
    sub_test "0" "-1" "1";
    sub_test "1" "0" "1";
    sub_test "1" "1" "0";
    sub_test "1" "-1" "2";
    sub_test "-1" "0" "-1";
    sub_test "-1" "1" "-2";
    sub_test "-1" "-1" "0";
    sub_test "-5" "4" "-9";
    sub_test "-4" "5" "-9";
    sub_test "5" "-4" "9";
    sub_test "4" "-5" "9";
    sub_test "-5" "-4" "-1";
    sub_test "-4" "-5" "1";
    sub_test "5" "4" "1";
    sub_test "4" "5" "-1";
    sub_test "111111111110" "11111111111" "99999999999";
    sub_test "111111111110" "-11111111111" "122222222221";
    sub_test "-111111111110" "11111111111" "-122222222221";
    sub_test "-111111111110" "-11111111111" "-99999999999";
    sub_test "100000000000" "9" "99999999991";
    sub_test "100000000000" "-9" "100000000009";
    sub_test "-100000000000" "9" "-100000000009";
    sub_test "-100000000000" "-9" "-99999999991";
    sub_test "1000000000000000000000000000000000000000" "1" "999999999999999999999999999999999999999";
    sub_test "1000000000000000000000000000000000000000" "-1" "1000000000000000000000000000000000000001";
    sub_test
      "1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "1234567889135802467024691356913580246890";
    sub_test
      "1234567890123456789012345678901234567890" "-987654321987654321987654321000"
      "1234567891111111111000000000888888888890";
    sub_test
      "-1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "-1234567891111111111000000000888888888890";
    sub_test
      "-1234567890123456789012345678901234567890" "-987654321987654321987654321000"
      "-1234567889135802467024691356913580246890";
    sub_test "999999999999999999999999999999" "123456789012345678901234567890" "876543210987654321098765432109";
    sub_test "999999999999999999999999999999" "-123456789012345678901234567890" "1123456789012345678901234567889";
  ]

(** Check that [sub (add x y) y = x]. *)
let add_and_sub_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"sub (add x y) y = x"
    ~count:1000
    ~print:Print.(tup2 to_string to_string)
    Gen.(tup2 bigint_gen bigint_gen)
    (fun (x, y) ->
       equal (sub (add x y) y) x)

(** Check that [add (sub x y) y = x]. *)
let sub_and_add_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"add (sub x y) y = x"
    ~count:1000
    ~print:Print.(tup2 to_string to_string)
    Gen.(tup2 bigint_gen bigint_gen)
    (fun (x, y) ->
       equal (add (sub x y) y) x)

(** Test cases for [mul]. *)
let mul_tests =
  let mul_test = binop_bigint_test mul "mul"
  in [
    mul_test "0" "0" "0";
    mul_test "0" "1" "0";
    mul_test "0" "-1" "0";
    mul_test "1" "0" "0";
    mul_test "1" "1" "1";
    mul_test "1" "-1" "-1";
    mul_test "-1" "0" "0";
    mul_test "-1" "1" "-1";
    mul_test "-1" "-1" "1";
    mul_test "1" "2" "2";
    mul_test "2" "2" "4";
    mul_test "42" "10" "420";
    mul_test "42" "-10" "-420";
    mul_test "42" "27" "1134";
    mul_test "27" "42" "1134";
    mul_test "9999999" "9999999" "99999980000001";
    mul_test "11111111111" "99999999999" "1111111111088888888889";
    mul_test "11111111111" "-99999999999" "-1111111111088888888889";
    mul_test
      "1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "1219326312467611633592440164485596706448437738013609205901126352690000";
    mul_test
      "1234567890123456789012345678901234567890" "-987654321987654321987654321000"
      "-1219326312467611633592440164485596706448437738013609205901126352690000";
    mul_test
      "-1234567890123456789012345678901234567890" "987654321987654321987654321000"
      "-1219326312467611633592440164485596706448437738013609205901126352690000";
    mul_test
      "-1234567890123456789012345678901234567890" "-987654321987654321987654321000"
      "1219326312467611633592440164485596706448437738013609205901126352690000";
    mul_test
      "987654321987654321987654321000" "1234567890123456789012345678901234567890"
      "1219326312467611633592440164485596706448437738013609205901126352690000";
    mul_test
      "987654321987654321987654321000" "-1234567890123456789012345678901234567890"
      "-1219326312467611633592440164485596706448437738013609205901126352690000";
    mul_test
      "999999999999999999999999999999" "123456789012345678901234567890"
      "123456789012345678901234567889876543210987654321098765432110";
    mul_test
      "999999999999999999999999999999" "-123456789012345678901234567890"
      "-123456789012345678901234567889876543210987654321098765432110";
    mul_test
      "12345678901234567890123456789012345678901234567890" "98765432109876543210987654321098765432109876543210"
      "1219326311370217952261850327338667885945115073915611949397448712086533622923332237463801111263526900";
    mul_test
      "12345678901234567890123456789012345678901234567890" "-98765432109876543210987654321098765432109876543210"
      "-1219326311370217952261850327338667885945115073915611949397448712086533622923332237463801111263526900";
  ]

(** Test data for [div], [rem], and [divmod]. *)
let divmod_test_data = [
  ("0", "1", "0", "0");
  ("0", "-1", "0", "0");
  ("1", "1", "1", "0");
  ("1", "-1", "-1", "0");
  ("-1", "1", "-1", "0");
  ("-1", "-1", "1", "0");
  ("1", "2", "0", "1");
  ("-1", "2", "0", "-1");
  ("1", "-2", "0", "1");
  ("-1", "-2", "0", "-1");
  ("2", "1", "2", "0");
  ("2", "-1", "-2", "0");
  ("-2", "1", "-2", "0");
  ("-2", "-1", "2", "0");
  ("2", "2", "1", "0");
  ("2", "3", "0", "2");
  ("-2", "3", "0", "-2");
  ("4", "2", "2", "0");
  ("4", "3", "1", "1");
  ("4", "4", "1", "0");
  ("4", "5", "0", "4");
  ("11", "3", "3", "2");
  ("-11", "3", "-3", "-2");
  ("11", "-3", "-3", "2");
  ("-11", "-3", "3", "-2");
  ("420", "10", "42", "0");
  ("420", "-10", "-42", "0");
  ("421", "10", "42", "1");
  ("429", "10", "42", "9");
  ("429", "-10", "-42", "9");
  ("1134", "27", "42", "0");
  ("1160", "27", "42", "26");
  ("99999980000001", "9999999", "9999999", "0");
  ("99999980000001", "-9999999", "-9999999", "0");
  ("-99999980000001", "9999999", "-9999999", "0");
  ("-99999980000001", "-9999999", "9999999", "0");
  ("99999989999999", "9999999", "9999999", "9999998");
  ("99999989999999", "-9999999", "-9999999", "9999998");
  ("-99999989999999", "9999999", "-9999999", "-9999998");
  ("-99999989999999", "-9999999", "9999999", "-9999998");
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
    "1219326311370217952261850327338667885945115073915611949397448712086533622923332237463801111263526900",
    "-12345678901234567890123456789012345678901234567890",
    "-98765432109876543210987654321098765432109876543210", "0"
  );
  (
    "1219326311370217952261850327338667885945115073915618122236899329370478684651726743636640561880810845",
    "12345678901234567890123456789012345678901234567890",
    "98765432109876543210987654321098765432109876543210", "6172839450617283945061728394506172839450617283945"
  );
  (
    "-1219326311370217952261850327338667885945115073915618122236899329370478684651726743636640561880810845",
    "12345678901234567890123456789012345678901234567890",
    "-98765432109876543210987654321098765432109876543210", "-6172839450617283945061728394506172839450617283945"
  );
  (
    "1219326311370217952261850327338667885945115073915624295076349946654423746380121249809480012498094789",
    "12345678901234567890123456789012345678901234567890",
    "98765432109876543210987654321098765432109876543210", "12345678901234567890123456789012345678901234567889"
  );
  (
    "-1219326311370217952261850327338667885945115073915624295076349946654423746380121249809480012498094789",
    "-12345678901234567890123456789012345678901234567890",
    "98765432109876543210987654321098765432109876543210", "-12345678901234567890123456789012345678901234567889"
  );
  (
    "1219326311370217952261850327338667885945115073915624295076349946654423746380121249809480012498094790",
    "12345678901234567890123456789012345678901234567890",
    "98765432109876543210987654321098765432109876543211", "0"
  );
  (
    "-1219326311370217952261850327338667885945115073915624295076349946654423746380121249809480012498094790",
    "12345678901234567890123456789012345678901234567890",
    "-98765432109876543210987654321098765432109876543211", "0"
  );
]

(** Test cases for [div]. *)
let div_tests =
  let div_test = binop_bigint_test div "div"
  in List.map (fun (x, y, q, _) -> div_test x y q) divmod_test_data

(** Check that dividing any number by zero is an error. *)
let div_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"div x 0 is error"
    ~count:20
    ~print:to_string
    bigint_gen
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
    Gen.(tup2 bigint_gen bigint_gen)
    (fun (x, y) ->
       assume (not (equal y zero));
       equal (div (mul x y) y) x)

(** Test cases for [rem]. *)
let rem_tests =
  let rem_test = binop_bigint_test rem "rem"
  in List.map (fun (x, y, _, r) -> rem_test x y r) divmod_test_data

(** Check that dividing any number by zero is an error. *)
let rem_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"rem x 0 is error"
    ~count:20
    ~print:to_string
    bigint_gen
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
      (Alcotest.pair bigint_testable bigint_testable)
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
    bigint_gen
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
    Gen.(tup2 bigint_gen bigint_gen)
    (fun (x, y) ->
       assume (not (equal y zero));
       let d, m = divmod x y in
       equal (add (mul d y) m) x)

(** Test cases for [succ]. *)
let succ_tests =
  let succ_test = unop_bigint_test succ "succ"
  in [
    succ_test "-1000000000" "-999999999";
    succ_test "-999999999" "-999999998";
    succ_test "-100" "-99";
    succ_test "-1" "0";
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
  let pred_test = unop_bigint_test pred "pred"
  in [
    pred_test "-999999999" "-1000000000";
    pred_test "-999999998" "-999999999";
    pred_test "-99" "-100";
    pred_test "0" "-1";
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

(** Check that [to_int x] is error if [x] can not be represented as a built-in
    integer. *)
let to_int_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int x is error if x can not be represented as a built-in int"
    ~count:20
    ~print:to_string
    bigint_gen
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
    bigint_gen
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
    bigint_gen
    (fun x ->
       assume (is_int x);
       equal (of_int (to_int x)) x)

(** Check that [to_int (of_int i) = i]. *)
let of_int_and_to_int_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int (of_int i) = i"
    ~count:250
    ~print:Print.int
    Gen.int
    (fun i ->
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
    bigint_gen
    (fun x ->
       assume (is_int x);
       equal (of_int (Option.get @@ to_int_opt x)) x)

(** Check that [to_int_opt (of_int i) = Some i]. *)
let of_int_and_to_int_opt_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int_opt (of_int i) = Some i"
    ~count:250
    ~print:Print.int
    Gen.int
    (fun i ->
       to_int_opt (of_int i) = Some i)

(** Check that [to_nat x] is error if [x < 0]. *)
let to_nat_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_nat x is error if x < 0"
    ~count:20
    ~print:to_string
    bigint_gen
    (fun x ->
       assume (compare x zero < 0);
       try
         ignore (to_nat x);
         false
       with Failure _ ->
         true)

(** Check that [to_nat_opt x = None] if [x < 0]. *)
let to_nat_opt_fail_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_int_opt x = None if x < 0"
    ~count:20
    ~print:to_string
    bigint_gen
    (fun x ->
       assume (compare x zero < 0);
       to_nat_opt x = None)

(** Check that [of_nat (to_nat x) = x] if [x >= 0]. *)
let to_nat_and_of_nat_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_nat (to_nat x) = x"
    ~count:250
    ~print:to_string
    bigint_gen
    (fun x ->
       assume (compare x zero >= 0);
       equal (of_nat (to_nat x)) x)

(** Check that [to_nat (of_nat n) = n]. *)
let of_nat_and_to_nat_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_nat (of_nat n) = n"
    ~count:250
    ~print:BigNat.to_string
    Util.bignat_gen
    (fun n ->
       BigNat.equal (to_nat (of_nat n)) n)

(** Check that [of_nat (Option.get @@ to_nat_opt x) = x] if [x >= 0]. *)
let to_nat_opt_and_of_nat_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_nat (Option.get @@ to_nat_opt x) = x"
    ~count:250
    ~print:to_string
    bigint_gen
    (fun x ->
       assume (compare x zero >= 0);
       equal (of_nat (Option.get @@ to_nat_opt x)) x)

(** Check that [to_nat_opt (of_nat n) = Some n]. *)
let of_nat_and_to_nat_opt_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_nat_opt (of_nat n) = Some n"
    ~count:250
    ~print:BigNat.to_string
    Util.bignat_gen
    (fun n ->
       BigNat.equal (Option.get @@ to_nat_opt (of_nat n)) n)

(* }}} *)

(* {{{ Conversions to and from strings
   ----------------------------------------------------------------------------
*)

(** Check that [of_string s] is error if [s] is not a valid representation of
    an integer. *)
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
    an integer. *)
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
    bigint_gen
    (fun x ->
       equal (of_string (to_string x)) x)

(** Check that [to_string (of_string s) = s] (up to leading zeros) if [s] is a
    valid representation of an integer. *)
let of_string_and_to_string_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_string (of_string s) = s"
    ~count:250
    ~print:Print.string
    strbigint_gen
    (fun s ->
       to_string (of_string s) = normalize_strnum s)

(** Check that [of_string_opt (to_string x) = Some x]. *)
let to_string_and_of_string_opt_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"of_string_opt (to_string x) = Some x"
    ~count:250
    ~print:to_string
    bigint_gen
    (fun x ->
       equal (Option.get @@ of_string_opt (to_string x)) x)

(** Check that [to_string (Option.get @@ of_string_opt s) = s] (up to leading
    zeros) if [s] is a valid representation of an integer. *)
let of_string_opt_and_to_string_are_inverse_ops_test =
  let open QCheck2 in
  QCheck_alcotest.to_alcotest @@
  Test.make
    ~name:"to_string (Option.get @@ of_string_opt s) = s"
    ~count:250
    ~print:Print.string
    strbigint_gen
    (fun s ->
       to_string (Option.get @@ of_string_opt s) = normalize_strnum s)

(* }}} *)

let () =
  Alcotest.run ~compact:true "BigInt" [
    "compare", compare_tests;
    "equal", equal_tests;
    "min", min_tests;
    "max", max_tests;
    "sign", sign_tests;
    "neg", neg_tests;
    "abs", abs_tests;
    "add", add_tests;
    "sub", sub_tests;
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
    "to_int failure", [to_int_fail_test];
    "to_int_opt failure", [to_int_opt_fail_test];
    "to_int, of_int", [to_int_and_of_int_are_inverse_ops_test];
    "of_int, to_int", [of_int_and_to_int_are_inverse_ops_test];
    "to_int_opt, of_int", [to_int_opt_and_of_int_are_inverse_ops_test];
    "of_int, to_int_opt", [of_int_and_to_int_opt_are_inverse_ops_test];
    "to_nat failure", [to_nat_fail_test];
    "to_nat_opt failure", [to_nat_opt_fail_test];
    "to_nat, of_nat", [to_nat_and_of_nat_are_inverse_ops_test];
    "of_nat, to_nat", [of_nat_and_to_nat_are_inverse_ops_test];
    "to_nat_opt, of_nat", [to_nat_opt_and_of_nat_are_inverse_ops_test];
    "of_nat, to_nat_opt", [of_nat_and_to_nat_opt_are_inverse_ops_test];
    "of_string failure", [of_string_fail_test];
    "of_string_opt failure", [of_string_opt_fail_test];
    "to_string, of_string", [to_string_and_of_string_are_inverse_ops_test];
    "of_string, to_string", [of_string_and_to_string_are_inverse_ops_test];
    "to_string, of_string_opt", [to_string_and_of_string_opt_are_inverse_ops_test];
    "of_string_opt, to_string", [of_string_opt_and_to_string_are_inverse_ops_test];
  ]
