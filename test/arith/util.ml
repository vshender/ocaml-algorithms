open Algorithms_arith

module type NumberSystem = sig
  type t
  val to_string : t -> string
  val equal : t -> t -> bool
end

module type BigNumberSystem = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
end

let num_testable (type a) (module NS : NumberSystem with type t = a) =
  let open NS in
  Alcotest.testable
    (fun fmt x -> x |> to_string |> Fmt.string fmt)
    equal

let unop_num_any_test
    (type a)
    (module NS : NumberSystem with type t = a) f fname expected_testable x expected =
  let open Alcotest in
  let open NS in
  test_case
    (Fmt.str "%s %s" fname (to_string x))
    `Quick
    (fun () ->
       check expected_testable
         (Fmt.str "%s %s = %a" fname (to_string x) (pp expected_testable) expected)
         expected
         (f x))

let unop_num_num_test
    (type a)
    (module NS : NumberSystem with type t = a) f fname x expected =
  let num_testable = num_testable (module NS) in
  unop_num_any_test (module NS) f fname num_testable x expected

let unop_num_raises_test
    (type a)
    (module NS : NumberSystem with type t = a) f fname x exn =
  let open Alcotest in
  let open NS in
  test_case
    (Fmt.str "%s %s" fname (to_string x))
    `Quick
    (fun () ->
       check_raises
         (Fmt.str "%s %s raises %s" fname (to_string x) (Printexc.to_string exn))
         exn
         (fun () -> ignore (f x)))

let binop_num_any_test
    (type a)
    (module NS : NumberSystem with type t = a) f fname expected_testable x y expected =
  let open Alcotest in
  let open NS in
  test_case
    (Fmt.str "%s %s %s" fname (to_string x) (to_string y))
    `Quick
    (fun () ->
       check expected_testable
         (Fmt.str "%s %s %s = %a" fname (to_string x) (to_string y) (pp expected_testable) expected)
         expected
         (f x y))

let unop_bignum_any_test
    (type a)
    (module NS : BigNumberSystem with type t = a) f fname expected_testable x expected =
  unop_num_any_test (module NS) f fname expected_testable (NS.of_string x) expected

let unop_bignum_bignum_test
    (type a)
    (module NS : BigNumberSystem with type t = a) f fname x expected =
  let bignum_testable = num_testable (module NS) in
  unop_bignum_any_test (module NS) f fname bignum_testable x (NS.of_string expected)

let unop_bignum_raises_test
    (type a)
    (module NS : BigNumberSystem with type t = a) f fname x exn =
  unop_num_raises_test (module NS) f fname (NS.of_string x) exn

let binop_bignum_any_test
    (type a)
    (module NS : BigNumberSystem with type t = a)
    f fname expected_testable x y expected =
  let open NS in
  binop_num_any_test (module NS) f fname expected_testable (of_string x) (of_string y) expected

let binop_bignum_bignum_test
    (type a)
    (module NS : BigNumberSystem with type t = a) f fname x y expected =
  let bignum_testable = num_testable (module NS) in
  binop_bignum_any_test (module NS) f fname bignum_testable x y (NS.of_string expected)

let strbignat_gen =
  let open QCheck2.Gen in
  string_of numeral >|= (fun s -> if s = "" then "0" else s)

let strbigint_gen =
  let open QCheck2.Gen in
  map2
    (fun strnum sign -> (if sign then "" else "-") ^ strnum)
    strbignat_gen
    bool

let bignat_gen =
  QCheck2.Gen.(strbignat_gen >|= BigNat.of_string)

let bigint_gen =
  QCheck2.Gen.(strbigint_gen >|= BigInt.of_string)

let normalize_strnum s =
  let open Str in
  let re = regexp "^\\(-?\\)0*\\(.*\\)$" in
  ignore (string_match re s 0);
  let ns = matched_group 1 s ^ matched_group 2 s in
  if ns = "-" || ns = "" then "0" else ns

let is_nat_num s =
  String.(length s > 0 && for_all (fun c -> '0' <= c && c <= '9') s)

let is_int_num s =
  let open String in
  let s =
    if s <> "" && s.[0] = '-' then sub s 1 (length s - 1)
    else s
  in s <> "" && for_all (fun c -> '0' <= c && c <= '9') s
