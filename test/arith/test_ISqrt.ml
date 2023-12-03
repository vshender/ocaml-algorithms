open Algorithms_arith
open Util

(** Test cases for numbers of the type [int]. *)
let isqrt_int_tests f fname =
  let isqrt_test = unop_num_num_test (module Int) f fname
  and isqrt_fail_test = unop_num_raises_test (module Int) f fname
  in [
    isqrt_fail_test (-42) (Failure "isqrt");
    isqrt_fail_test (-1) (Failure "isqrt");
    isqrt_test 0 0;
    isqrt_test 1 1;
    isqrt_test 2 1;
    isqrt_test 3 1;
    isqrt_test 4 2;
    isqrt_test 5 2;
    isqrt_test 6 2;
    isqrt_test 7 2;
    isqrt_test 8 2;
    isqrt_test 9 3;
    isqrt_test 42 6;
    isqrt_test 144 12;
    isqrt_test 999999 999;
    isqrt_test 1000000 1000;
    isqrt_test 10000000 3162;
    isqrt_test 1073741823 32767;
  ]

(** Test cases for numbers of the type [nativeint]. *)
let isqrt_nativeint_tests f fname =
  let isqrt_test = unop_num_num_test (module Nativeint) f fname
  and isqrt_fail_test = unop_num_raises_test (module Nativeint) f fname
  in [
    isqrt_fail_test (-42n) (Failure "isqrt");
    isqrt_fail_test (-1n) (Failure "isqrt");
    isqrt_test 0n 0n;
    isqrt_test 1n 1n;
    isqrt_test 2n 1n;
    isqrt_test 3n 1n;
    isqrt_test 4n 2n;
    isqrt_test 5n 2n;
    isqrt_test 6n 2n;
    isqrt_test 7n 2n;
    isqrt_test 8n 2n;
    isqrt_test 9n 3n;
    isqrt_test 42n 6n;
    isqrt_test 144n 12n;
    isqrt_test 999999n 999n;
    isqrt_test 1000000n 1000n;
    isqrt_test 10000000n 3162n;
    isqrt_test 1234567890n 35136n;
    isqrt_test 1073741823n 32767n;
    isqrt_test 1999999999n 44721n;
    isqrt_test 2147483647n 46340n;
  ]

(** Test cases for numbers of the type [int32]. *)
let isqrt_int32_tests f fname =
  let isqrt_test = unop_num_num_test (module Int32) f fname
  and isqrt_fail_test = unop_num_raises_test (module Int32) f fname
  in [
    isqrt_fail_test (-42l) (Failure "isqrt");
    isqrt_fail_test (-1l) (Failure "isqrt");
    isqrt_test 0l 0l;
    isqrt_test 1l 1l;
    isqrt_test 2l 1l;
    isqrt_test 3l 1l;
    isqrt_test 4l 2l;
    isqrt_test 5l 2l;
    isqrt_test 6l 2l;
    isqrt_test 7l 2l;
    isqrt_test 8l 2l;
    isqrt_test 9l 3l;
    isqrt_test 42l 6l;
    isqrt_test 144l 12l;
    isqrt_test 999999l 999l;
    isqrt_test 1000000l 1000l;
    isqrt_test 10000000l 3162l;
    isqrt_test 1073741823l 32767l;
    isqrt_test 1234567890l 35136l;
    isqrt_test 1999999999l 44721l;
    isqrt_test 2147483647l 46340l;
  ]

(** Test cases for numbers of the type [int64]. *)
let isqrt_int64_tests f fname =
  let isqrt_test = unop_num_num_test (module Int64) f fname
  and isqrt_fail_test = unop_num_raises_test (module Int64) f fname
  in [
    isqrt_fail_test (-42L) (Failure "isqrt");
    isqrt_fail_test (-1L) (Failure "isqrt");
    isqrt_test 0L 0L;
    isqrt_test 1L 1L;
    isqrt_test 2L 1L;
    isqrt_test 3L 1L;
    isqrt_test 4L 2L;
    isqrt_test 5L 2L;
    isqrt_test 6L 2L;
    isqrt_test 7L 2L;
    isqrt_test 8L 2L;
    isqrt_test 9L 3L;
    isqrt_test 42L 6L;
    isqrt_test 144L 12L;
    isqrt_test 999999L 999L;
    isqrt_test 1000000L 1000L;
    isqrt_test 10000000L 3162L;
    isqrt_test 1073741823L 32767L;
    isqrt_test 1234567890L 35136L;
    isqrt_test 1999999999L 44721L;
    isqrt_test 2147483647L 46340L;
    isqrt_test 9999999999L 99999L;
    isqrt_test 1234567890123456789L 1111111106L;
    isqrt_test 1999999999999999999L 1414213562L;
    isqrt_test 2999999999999999999L 1732050807L;
    isqrt_test 3999999999999999999L 1999999999L;
    isqrt_test 4611686018427387903L 2147483647L;
    isqrt_test 4999999999999999999L 2236067977L;
    isqrt_test 5999999999999999999L 2449489742L;
    isqrt_test 6999999999999999999L 2645751311L;
    isqrt_test 7999999999999999999L 2828427124L;
    isqrt_test 8999999999999999999L 2999999999L;
    isqrt_test 9223372036854775807L 3037000499L;
  ]

(** Test cases for numbers of the type [BigNat.t]. *)
let isqrt_bignat_tests f fname =
  let isqrt_test = unop_bignum_bignum_test (module BigNat) f fname
  in [
    isqrt_test "0" "0";
    isqrt_test "1" "1";
    isqrt_test "2" "1";
    isqrt_test "3" "1";
    isqrt_test "4" "2";
    isqrt_test "5" "2";
    isqrt_test "6" "2";
    isqrt_test "7" "2";
    isqrt_test "8" "2";
    isqrt_test "9" "3";
    isqrt_test "42" "6";
    isqrt_test "144" "12";
    isqrt_test "999999" "999";
    isqrt_test "1000000" "1000";
    isqrt_test "10000000" "3162";
    isqrt_test "1073741823" "32767";
    isqrt_test "1234567890" "35136";
    isqrt_test "1999999999" "44721";
    isqrt_test "2147483647" "46340";
    isqrt_test "9999999999" "99999";
    isqrt_test "1234567890123456789" "1111111106";
    isqrt_test "1999999999999999999" "1414213562";
    isqrt_test "2999999999999999999" "1732050807";
    isqrt_test "3999999999999999999" "1999999999";
    isqrt_test "4611686018427387903" "2147483647";
    isqrt_test "4999999999999999999" "2236067977";
    isqrt_test "5999999999999999999" "2449489742";
    isqrt_test "6999999999999999999" "2645751311";
    isqrt_test "7999999999999999999" "2828427124";
    isqrt_test "8999999999999999999" "2999999999";
    isqrt_test "9223372036854775807" "3037000499";
    isqrt_test "12345678901234567890" "3513641828";
    isqrt_test "50000000000000000000" "7071067811";
    isqrt_test "123456789012345678901234567890" "351364182882014";
    isqrt_test "987654321098765432109876543210" "993807990055808";
    isqrt_test
      "2343254317865987132508718326041632408731657021374876541784973"
      "1530769191571997614492600881936";
    isqrt_test
      "97897432041794017583412364871325814682374172879071840362105321807432748321743217583218758721432"
      "312885653301320317318318253541835955407042739429";
    isqrt_test
      "4321516519846514371273432184632175434132432143217432184732147321432143244315461635134232148329643124214"
      "2078825755046948222043561329910289642374583606533409";
    isqrt_test
      "8197368731647362147631065171078321463216483217086502164878326486321508732104768732165810253210571328642144321"
      "2863104736408949840713026783044501150415572695811988499";
    isqrt_test
      "7986314718648715416109437581974147832174821748703217443214324318973186521987432176473218647409875874326587321483214831757148756718326473218493214683216473214871698321648175081629843214"
      "89366183305816050813012684267308315977501524935816314686326560227288294832959072069526354123";
    isqrt_test
      "89764372184315973252153240321479832107432165073217473214732143210432147321579671326408731416578321047321432143210432165032174321498732165732107444409832164032165832174083216432170651263874132463210561326479321508432164032164732157032165032150123640321440134608321458732164321487321478467134063574363214"
      "9474406165259961729595411546335452794891541063597209809901176463618417221912189605305491048483761687407938576663112565979796484463852668097295894070464";
  ]

(** Test cases for numbers of the type [BigInt.t]. *)
let isqrt_bigint_tests f fname =
  let isqrt_test = unop_bignum_bignum_test (module BigInt) f fname
  and isqrt_fail_test = unop_bignum_raises_test (module BigInt) f fname
  in [
    isqrt_fail_test "-42" (Failure "isqrt");
    isqrt_fail_test "-1" (Failure "isqrt");
    isqrt_test "0" "0";
    isqrt_test "1" "1";
    isqrt_test "2" "1";
    isqrt_test "3" "1";
    isqrt_test "4" "2";
    isqrt_test "5" "2";
    isqrt_test "6" "2";
    isqrt_test "7" "2";
    isqrt_test "8" "2";
    isqrt_test "9" "3";
    isqrt_test "42" "6";
    isqrt_test "144" "12";
    isqrt_test "999999" "999";
    isqrt_test "1000000" "1000";
    isqrt_test "10000000" "3162";
    isqrt_test "1073741823" "32767";
    isqrt_test "1234567890" "35136";
    isqrt_test "1999999999" "44721";
    isqrt_test "2147483647" "46340";
    isqrt_test "9999999999" "99999";
    isqrt_test "1234567890123456789" "1111111106";
    isqrt_test "1999999999999999999" "1414213562";
    isqrt_test "2999999999999999999" "1732050807";
    isqrt_test "3999999999999999999" "1999999999";
    isqrt_test "4611686018427387903" "2147483647";
    isqrt_test "4999999999999999999" "2236067977";
    isqrt_test "5999999999999999999" "2449489742";
    isqrt_test "6999999999999999999" "2645751311";
    isqrt_test "7999999999999999999" "2828427124";
    isqrt_test "8999999999999999999" "2999999999";
    isqrt_test "9223372036854775807" "3037000499";
    isqrt_test "12345678901234567890" "3513641828";
    isqrt_test "50000000000000000000" "7071067811";
    isqrt_test "123456789012345678901234567890" "351364182882014";
    isqrt_test "987654321098765432109876543210" "993807990055808";
    isqrt_test
      "2343254317865987132508718326041632408731657021374876541784973"
      "1530769191571997614492600881936";
    isqrt_test
      "97897432041794017583412364871325814682374172879071840362105321807432748321743217583218758721432"
      "312885653301320317318318253541835955407042739429";
    isqrt_test
      "4321516519846514371273432184632175434132432143217432184732147321432143244315461635134232148329643124214"
      "2078825755046948222043561329910289642374583606533409";
    isqrt_test
      "8197368731647362147631065171078321463216483217086502164878326486321508732104768732165810253210571328642144321"
      "2863104736408949840713026783044501150415572695811988499";
    isqrt_test
      "7986314718648715416109437581974147832174821748703217443214324318973186521987432176473218647409875874326587321483214831757148756718326473218493214683216473214871698321648175081629843214"
      "89366183305816050813012684267308315977501524935816314686326560227288294832959072069526354123";
    isqrt_test
      "89764372184315973252153240321479832107432165073217473214732143210432147321579671326408731416578321047321432143210432165032174321498732165732107444409832164032165832174083216432170651263874132463210561326479321508432164032164732157032165032150123640321440134608321458732164321487321478467134063574363214"
      "9474406165259961729595411546335452794891541063597209809901176463618417221912189605305491048483761687407938576663112565979796484463852668097295894070464";
  ]

let () =
  Alcotest.run ~compact:true "ISqrt" [
    "newton (int)", isqrt_int_tests (ISqrt.newton (module Int)) "newton";
    "newton (nativeint)", isqrt_nativeint_tests (ISqrt.newton (module Nativeint)) "newton";
    "newton (int32)", isqrt_int32_tests (ISqrt.newton (module Int32)) "newton";
    "newton (int64)", isqrt_int64_tests (ISqrt.newton (module Int64)) "newton";
    "newton (BigNat)", isqrt_bignat_tests (ISqrt.newton (module BigNat)) "newton";
    "newton (BigInt)", isqrt_bigint_tests (ISqrt.newton (module BigInt)) "newton";
    "digit_by_digit (int)", isqrt_int_tests (ISqrt.(digit_by_digit (module ExtendedInt))) "digit_by_digit";
    "digit_by_digit (nativeint)", isqrt_nativeint_tests (ISqrt.(digit_by_digit (module ExtendedNativeint))) "digit_by_digit";
    "digit_by_digit (int32)", isqrt_int32_tests (ISqrt.(digit_by_digit (module ExtendedInt32))) "digit_by_digit";
    "digit_by_digit (int64)", isqrt_int64_tests (ISqrt.(digit_by_digit (module ExtendedInt64))) "digit_by_digit";
    "digit_by_digit (BigNat)", isqrt_bignat_tests (ISqrt.(digit_by_digit (module ExtendedBigNat))) "digit_by_digit";
    "digit_by_digit (BigInt)", isqrt_bigint_tests (ISqrt.(digit_by_digit (module ExtendedBigInt))) "digit_by_digit";
  ]
