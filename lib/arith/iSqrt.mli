(** Integer square root calculation. *)

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
