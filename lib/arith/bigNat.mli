(** Arbitrary-precision arithmetic on natural numbers. *)

(** {1 Natural numbers } *)

type t
(** The type of big natural numbers. *)

val zero : t
(** [zero] is the natural number [0]. *)

val one : t
(** [one] is the natural number [1]. *)


(** {1 Arithmetic operations } *)

val add : t -> t -> t
(** [add x y] is the addition [x + y]. *)

val sub : t -> t -> t
(** [sub x y] is the substraction [x - y].
    Raises [Failure "sub"] if [x < y]. *)

val mul : t -> t -> t
(** [mul x y] is the multiplication [x * y]. *)

val div : t -> t -> t
(** [div x y] is the integer division [x / y].  If [y] is not zero, [div x y]
    is the greatest natural number less than or equal to the real quotient of
    [x] by [y].
    Raises [Division_by_zero] if [y = 0]. *)

val rem: t -> t -> t
(** [rem x y] is the integer reminder of [x] divided by [y].  If [y] is not
    zero, the result of [rem x y] satisfies the following properties:
    [x = add (mul (div x y) y) (rem x y)] and [rem x y <= y - 1].  If [y = 0],
    [rem x y] raises [Division_by_zero]. *)

val divmod : t -> t -> t * t
(** [divmod x y] is the pair of division and remainder of [x] by [y].
    It is equivalent to [(div x y, rem x y)].
    Raises [Division_by_zero] if [y = 0]. *)

val succ : t -> t
(** [succ x] is the successor of [x], i.e., [add x one]. *)

val pred : t -> t
(** [pred x] is the predecessor of [x], i.e., [sub x one].
    Raises [Failure "pred"] if [x = 0]. *)


(** {1 Comparisons} *)

val compare : t -> t -> int
(** [compare x y] returns [0] if [x] is equal to [y], [1] if [x] is greater
    than [y], and [-1] if [x] is smaller than [y]. *)

val equal : t -> t -> bool
(** [equal x y] is [true] if and only if [x = y]. *)

val min : t -> t -> t
(** [min x y] is the smaller number of [x] and [y]. *)

val max : t -> t -> t
(** [max x y] is the greater number of [x] and [y]. *)


(** {1 Hash funtion} *)

val hash : t -> int
(** [hash x] computes a hash value for [x]. *)


(** {1 Conversion to and from other numerical types} *)

val of_int : int -> t
(** [of_int i] converts the built-in integer [i] to a big natural number.
    Raises [Failure "of_int"] if [i < 0]. *)

val is_int : t -> bool
(** [is_int x] checks if [x] can be represented as a built-in integer.  On a
    32-bit platform, [is_int x] returns [true] if and only if [x] is between 0
    and 2^30 - 1.  On a 64-bit platform, [is_int x] returns [true] if and only
    if [x] is between 0 and 2^62 - 1. *)

val to_int : t -> int
(** [to_int x] converts [x] to a built-in integer.
    Raises [Failure "to_int"] if [x] cannot be represented as a built-in
    integer. *)

val to_int_opt : t -> int option
(** [to_int x] converts [x] to a built-in integer.
    Returns [None] if [x] cannot be represented as a built-in integer, or
    [Some i] where [i] is the integer value. *)


(** {1 Conversion to and from strings} *)

val to_string : t -> string
(** [to_string x] is the string representation of [x], in decimal (base 10). *)

val of_string : string -> t
(** [of_string s] converts the string [s], which should consist of decimal
    digits only, to a big natural number.
    Raises [Failure "of_string"] if [s] is not a valid representation of a
    natural number. *)

val of_string_opt : string -> t option
(** [of_string_opt s] converts the string [s], which should consist of decimal
    digits only, to a big natural number.
    Returns [None] if [s] is not a valid representation of a natural number, or
    [Some x] where [x] is the big natural number. *)


(**/**)

(** {1 Internals} *)

(** The [Internals] module provides access to low-level details and operations
    on big natural numbers.  This module is intended for advanced uses where
    direct manipulation or access to internal representation is required, such
    as when implementing custom algorithms that are not provided by the
    [BigNat] module.

    The main idea is that a big natural number is stored as an array of its
    digits in some base.  In this implementation, the base is a power of
    [10]. *)
module Internals: sig
  val base : int
  (** [base] is the base of the number system used in the representation of big
      natural numbers. *)

  val digits : t -> int array
  (** [digits x] returns digits of the big natural number [x].  Digits are
      returned in a little-endian format, with the least significant digit at
      index [0].  Each digit is less than [base]. *)

  val add_digit : t -> int -> t
  (** [add_digit x y] adds the integer number [y] to [x].
      Requires [y < base]. *)

  val mul_digit : t -> int -> t
  (** [mul_digit x y] multiplies [x] by the integer number [y].
      Requires [y < base]. *)
end
