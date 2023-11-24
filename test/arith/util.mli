open Algorithms_arith

(** The minimal number system signature required by the utility functions
    [num_testable], [unop_num_num_test], and [unop_num_raises_test]. *)
module type NumberSystem = sig
  type t
  val to_string : t -> string
  val equal : t -> t -> bool
end

(** The minimal number system signature required by test case generators for
    big numbers represented as strings.

    It would be nice to have one common signature here, but unfortunately the
    built-in [Int] module does not have the [of_string] function. *)
module type BigNumberSystem = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
end

val num_testable : (module NumberSystem with type t = 'a) -> 'a Alcotest.testable
(** [num_testable (module NS)] is the implementation of [Alcotest.testable] for
    [NS.t]. *)

val unop_num_any_test :
  (module NumberSystem with type t = 'a) ->
  ('a -> 'b) -> string ->
  'b Alcotest.testable -> 'a -> 'b -> unit Alcotest.test_case
(** [unop_num_any_test (module NS) f fname expected_testable x expected]
    generates a test case that checks that [f x = expected].
    [x] is a number of the type [NS.t], [expected_testable] is an
    [Alcotest.testable] for [expected].

    Example: [unop_num_any_test (module Int) sign "sign" Alcotest.int (-42) -1].
*)

val unop_num_num_test :
  (module NumberSystem with type t = 'a) ->
  ('a -> 'a) -> string -> 'a -> 'a -> unit Alcotest.test_case
(** [unop_num_num_test (module NS) f fname x expected] generates a test case
    that checks that [f x = expected].
    [x] and [expected] are numbers of the type [NS.t].

    Example: [unop_num_num_test (module Int64) isqrt "isqrt" 9L 3L]. *)

val unop_num_raises_test :
  (module NumberSystem with type t = 'a) ->
  ('a -> 'b) -> string ->
  'a -> exn -> unit Alcotest.test_case
(** [unop_num_raises_test (module NS) f fname x exn] generates a test case that
    checks that [f x] raises [exn].
    [x] is a number of the type [NS.t].

    Example: [unop_num_raises_test (module Int) isqrt "isqrt" (-1) (Failure "isqrt")].
*)

val binop_num_any_test :
  (module NumberSystem with type t = 'a) ->
  ('a -> 'a -> 'b) -> string ->
  'b Alcotest.testable -> 'a -> 'a -> 'b -> unit Alcotest.test_case
(** [binop_num_any_test (module NS) f fname expected_testable x y expected]
    generates a test case that checks that [f x y = expected].
    [x] and [y] are numbers of the type [NS.t], [expected_testable] is an
    [Alcotest.testable] for [expected].

    Example: [binop_num_any_test (module Int) compare "compaore" Alcotest.int 42 42 0].
*)

val unop_bignum_any_test :
  (module BigNumberSystem with type t = 'a) ->
  ('a -> 'b) -> string ->
  'b Alcotest.testable -> string -> 'b -> unit Alcotest.test_case
(** [unop_bignum_any_test (module NS) f fname expected_testable x expected]
    generates a test case that checks that [f x = expected].
    [x] is a string representation of a big number of the type [NS.t],
    [expected_testable] is an [Alcotest.testable] for [expected].

    Example: [unop_bignum_any_test (module BigInt) sign "sign" Alcotest.int "-42" -1].
*)

val unop_bignum_bignum_test :
  (module BigNumberSystem with type t = 'a) ->
  ('a -> 'a) -> string ->
  string -> string -> unit Alcotest.test_case
(** [unop_bignum_bignum_test (module NS) f fname x expected] generates a test
    case that checks that [f x = expected].
    [x] and [expected] are string representations of big numbers of the type
    [NS.t].

    Example: [unop_bignum_bignum_test (module BigNat) succ "succ" "42" "43"].
*)

val unop_bignum_raises_test :
  (module BigNumberSystem with type t = 'a) ->
  ('a -> 'b) -> string ->
  string -> exn -> unit Alcotest.test_case
(** [unop_bignum_raises_test (module NS) f fname x exn] generates a test case
    that checks that [f x] raises [exn].
    [x] is a string representation of a big number of the type [NS.t].

    Example: [unop_bignum_raises_test (module BigInt) isqrt "isqrt" "-1" (Failure "isqrt")].
*)

val binop_bignum_any_test :
  (module BigNumberSystem with type t = 'a) ->
  ('a -> 'a -> 'b) -> string -> 'b Alcotest.testable ->
  string -> string -> 'b -> unit Alcotest.test_case
(** [binop_bignum_any_test (module NS) f fname expected_testable x y expected]
    generates a test case that checks that [f x y = expected].
    [x] and [y] are string representations of big numbers of the type [NS.t],
    [expected_testable] is an [Alcotest.testable] for [expected].

    Example: [binop_bignum_any_test (module BigNat) compare "compare" Alcotest.int "42" "42" 0].
*)

val binop_bignum_bignum_test :
  (module BigNumberSystem with type t = 'a) ->
  ('a -> 'a -> 'a) -> string ->
  string -> string -> string -> unit Alcotest.test_case
(** [binop_bignum_bignum_test (module NS) f fname x y expected] generates a
    test case that checks that [f x y = expected].
    [x], [y], and [expected] are string representations of big numbers of the
    type [NS.t].

    Example: [test_binop_bignum_bignum (module BigNat) add "add" "42" "27" "69"].
*)

val strbignat_gen : string QCheck2.Gen.t
(** The generator ([QCheck2.Gen.t]) of string representations of big natural
    numbers. *)

val strbigint_gen : string QCheck2.Gen.t
(** The generator ([QCheck2.Gen.t]) of string representations of big
    integers. *)

val bignat_gen : BigNat.t QCheck2.Gen.t
(** The generator ([QCheck2.Gen.t]) for [BigNat.t]. *)

val bigint_gen : BigInt.t QCheck2.Gen.t
(** The generator ([QCheck2.Gen.t]) for [BigInt.t]. *)

val normalize_strnum : string -> string
(** [normalize_strnum s] normalizes the string representation [s] of a number.
    This function removes all leading zeros and redundant minus sign.

    Examples:

    - [normalize_strnum "42" = "42"];
    - [normalize_strnum "00042" = "42"];
    - [normalize_strnum "-00042" = "-42"];
    - [normalize_strnum "-0000" = "0"]. *)

val is_nat_num : string -> bool
(** [is_nat_num s] checks that [s] is a valid representation of a natural
    number. *)

val is_int_num : string -> bool
(** [is_int_num s] checks that [s] is a valid representation of an integer. *)
