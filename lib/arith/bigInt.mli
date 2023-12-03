(** Arbitrary-precision arithmetic on integers. *)

(** {1 Integers } *)

type t
(** The type of big integers. *)

val zero : t
(** [zero] is the big integer [0]. *)

val one : t
(** [one] is the big integer [1]. *)

val minus_one : t
(** [minus_one] is the big integer [-1]. *)


(** {1 Arithmetic operations } *)

val sign : t -> int
(** [sign x] is the sign of [x].  This function returns [0] if [x] is zero, [1]
    if [x] is positive, and [-1] if [x] is negative. *)

val neg : t -> t
(** [neg x] is the negation of [x]. *)

val abs : t -> t
(** [abs x] is the absolute value of [x]. *)

val add : t -> t -> t
(** [add x y] is the addition [x + y]. *)

val sub : t -> t -> t
(** [sub x y] is the substraction [x - y]. *)

val mul : t -> t -> t
(** [mul x y] is the multiplication [x * y]. *)

val div : t -> t -> t
(** [div x y] is the integer division [x / y].  Integer division rounds the
    real quotient of its arguments towards zero.  More precisely, if
    [x >= 0] and [y > 0], [div x y] is the greatest integer less than or equal
    to the real quotient of [x] by [y].  Moreover,
    [div (neg x) y = div x (neg y) = neg (div x y)].
    Raises [Division_by_zero] if [y = 0]. *)

val rem: t -> t -> t
(** [rem x y] is the integer reminder of [x] divided by [y].  If [y] is not
    zero, the result of [rem x y] satisfies the following properties:
    [x = add (mul (div x y) y) (rem x y)] and [abs (rem x y) <= abs y - 1].  If
    [y = 0], [rem x y] raises [Division_by_zero].  Note that [rem x y] is
    negative only if [x < 0]. *)

val divmod : t -> t -> t * t
(** [divmod x y] is the pair of division and remainder of [x] by [y].
    It is equivalent to [(div x y, rem x y)].  Raises [Division_by_zero] if
    [y = 0]. *)

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
(** [equal x y] is [true] if and only if [x = y]. .*)

val min : t -> t -> t
(** [min x y] is the smaller number of [x] and [y]. *)

val max : t -> t -> t
(** [max x y] is the greater number of [x] and [y]. *)


(** {1 Hash funtion} *)

val hash : t -> int
(** [hash x] computes a hash value for [x]. *)


(** {1 Conversion to and from other numerical types} *)

val of_int : int -> t
(** [of_int i] converts the native integer [i] to a big integer. *)

val is_int : t -> bool
(** [is_int x] checks if [x] can be represented as a native integer.  On a
    32-bit platform, [is_int x] returns [true] if and only if [x] is between
    -2^30 + 1 and 2^30 - 1.  On a 64-bit platform, [is_int x] returns [true] if
    and only if [x] is between -2^62 + 1 and 2^62 - 1. *)

val to_int : t -> int
(** [to_int x] converts [x] to a native integer.
    Raises [Failure "to_int"] if [x] cannot be represented as a native
    integer. *)

val to_int_opt : t -> int option
(** [to_int x] converts [x] to a native integer.
    Returns [None] if [x] cannot be represented as a native integer, or
    [Some i] where [i] is the integer value. *)

val of_nat : BigNat.t -> t
(** [of_nat n] converts the big natural number [n] to a big integer. *)

val to_nat : t -> BigNat.t
(** [to_nat x] converts [x] to a big natural number.
    Raises [Failure "to_nat"] if [x < 0]. *)

val to_nat_opt : t -> BigNat.t option
(** [to_nat_opt x] converts [x] to a big natural number.
    Returns [None] if [x < 0], or [Some n] where [n] is the big natural
    number. *)


(** {1 Conversion to and from strings} *)

val to_string : t -> string
(** [to_string x] is the string representation of [x], in decimal (base 10). *)

val of_string : string -> t
(** [of_string s] converts the string [s] to a big integer.
    Raises [Failure "of_string"] if [s] is not a valid representation of an
    integer. *)

val of_string_opt : string -> t option
(** [of_string_opt s] converts the string [s] to a big integer.
    Returns [None] if [s] is not a valid representation of an integer, or
    [Some x] where [x] is the big integer. *)


(**/**)

(** {1 Internals} *)

(** The [Internals] module provides access to low-level details and operations
    on big integers.  This module is intended for advanced uses where direct
    manipulation or access to internal representation is required, such as when
    implementing custom algorithms that are not provided by the [BigInt]
    module.

    The main idea is that the magnitude of a big integer is stored as an array
    of its "digits" in some base.  In this implementation, the base is a power
    of [10]. *)
module Internals: sig
  val base : int
  (** [base] is the base of the number system used in the representation of
      magnitude of big integers. *)

  val digits : t -> int array
  (** [digits x] returns digits of the magnitude of the big integer [x].
      Digits are returned in a little-endian format, with the least significant
      digit at index [0].  Each digit is less than [base]. *)

  val add_digit : t -> int -> t
  (** [add_digit x y] adds the integer number [y] to the magnitude of [x].
      Requires [y < base]. *)

  val mul_digit : t -> int -> t
  (** [mul_digit x y] multiplies the magnitude of [x] by the integer number
      [y].  Requires [y < base]. *)
end
