(** An interface for ordered types.

    This module type is used to define types [t] that have an ordering, which
    is expressed through the [compare] function. *)
module type S = sig
  type t
  (** The type of elements that can be compared. *)

  val compare : t -> t -> int
  (** [compare x y] returns an integer that indicates the order of [x] and [y].

      - It returns 0 if [x] and [y] are equal.
      - It returns a positive integer if [x] is greater than [y].
      - It returns a negative integer if [x] is less than [y].

      The comparison must satisfy the properties of total order:

      - Reflexivity: [compare x x = 0].
      - Antisymmetry: if [compare x y > 0] then [compare y x < 0].
      - Transitivity: if [compare x y > 0] and [compare y z > 0], then
        [compare x z > 0].

      These properties ensure that the type [t] can be used in data structures
      and algorithms that require a consistent and total ordering of elements.
  *)
end
