open Algorithms_arith

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
