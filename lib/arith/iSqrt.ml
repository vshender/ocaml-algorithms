(** Integer square root calculation. *)

module type NumberSystem = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val div : t -> t -> t
  val compare : t -> t -> int
  val min : t -> t -> t
end

(** Calculate the integer square root of a number using Newton's method for
    finding roots, iterating until convergence.

    Links:

    - https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Heron's_method
    - https://en.wikipedia.org/wiki/Integer_square_root#Algorithm_using_Newton's_method
*)
let newton (type a) (module NS : NumberSystem with type t = a) x =
  let open NS in
  let ( + ) = add and ( / ) = div in
  let two = one + one in

  let[@inline] xnext xcur = (xcur + x / xcur) / two in

  let rec newton_iter x0 x1 =
    (* If x + 1 is a perfect square, the sequence xi does not converge but
       ends up in a period-two cycle, alternating isqrt(x) and isqrt(x) + 1. *)
    let x2 = xnext x1 in
    if compare x0 x1 = 0 || compare x0 x2 = 0 then
      min x0 x1
    else
      newton_iter x1 x2
  in

  if compare x zero < 0 then
    failwith "isqrt"
  else
    let x0 = x / two in
    if compare x0 zero = 0 then  (* avoiding division by zero; x = 0 or x = 1 *)
      x
    else
      newton_iter x0 (xnext x0)
