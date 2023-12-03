(** Integer square root calculation. *)

(** {1 Newton's method} *)

(** The minimal number system signature required by [newton_method]. *)
module type NumberSystem = sig
  type t
  (** The type of numbers. *)

  val zero : t
  (** [zero] is the number [0]. *)

  val one : t
  (** [one] is the number [1]. *)

  val add : t -> t -> t
  (** [add x y] is the addition [x + y]. *)

  val div : t -> t -> t
  (** [div x y] is the integer division [x / y]. *)

  val compare : t -> t -> int
  (** [compare x y] returns [0] if [x] is equal to [y], [1] if [x] is greater
      than [y], and [-1] if [x] is smaller than [y]. *)

  val min : t -> t -> t
  (** [min x y] is the smaller of [x] and [y]. *)
end

val newton : (module NumberSystem with type t = 'a) -> 'a -> 'a
(** [newton (module NS) x] calculates the integer square root of the
    non-negative number [x] using Newton's method.

    The [NS] module should implement the required arithmetic operations and
    comparisons for the provided number type ['a].

    Raises [Failure "isqrt"] if [x < 0]. *)


(** {1 Digit-by-digit algorithm} *)

(** The minimal number system signature required by [digit_by_digit].
    Digit-by-digit algorithm requires access to the digits of a number. *)
module type ExtendedNumberSystem = sig
  type t
  (** The type of numbers. *)

  val zero : t
  (** [zero] is the number [0]. *)

  val sub : t -> t -> t
  (** [sub x y] is the substraction [x - y]. *)

  val compare : t -> t -> int
  (** [compare x y] returns [0] if [x] is equal to [y], [1] if [x] is greater
      than [y], and [-1] if [x] is smaller than [y]. *)

  val base : int
  (** The base of the number system, used for digit operations. *)

  val digits : t -> int array
  (** [digits x] returns an array of digits of [x] in the base of the number
      system. *)

  val add_digit : t -> int -> t
  (** [add_digit x d] returns the number after adding the digit [d] to [x].
      Requires [d < base]. *)

  val mul_digit : t -> int -> t
  (** [mul_digit x d] multiplies [x] by the digit [d].
      Requires [d < base]. *)
end

module ExtendedInt : ExtendedNumberSystem with type t = int
(** The implementation of [ExtendedNumberSystem] for [int]. *)

module ExtendedNativeint : ExtendedNumberSystem with type t = nativeint
(** The implementation of [ExtendedNumberSystem] for [nativeint]. *)

module ExtendedInt32 : ExtendedNumberSystem with type t = int32
(** The implementation of [ExtendedNumberSystem] for [int32]. *)

module ExtendedInt64 : ExtendedNumberSystem with type t = int64
(** The implementation of [ExtendedNumberSystem] for [int64]. *)

module ExtendedBigNat : ExtendedNumberSystem with type t = BigNat.t
(** The implementation of [ExtendedNumberSystem] for [BigNat.t]. *)

module ExtendedBigInt : ExtendedNumberSystem with type t = BigInt.t
(** The implementation of [ExtendedNumberSystem] for [BigInt.t]. *)

val digit_by_digit : (module ExtendedNumberSystem with type t = 'a) -> 'a -> 'a
(** [digit_by_digit (module NS) x] calculates the integer square root of the
    non-negative number [x] using digit-by-digit algorithm.

    The [NS] module should implement the required operations for the provided
    number type ['a].

    Raises [Failure "isqrt"] if [x < 0]. *)
