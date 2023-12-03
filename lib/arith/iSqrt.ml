(** Integer square root calculation. *)

(* {{{ Newton's method
   ----------------------------------------------------------------------------
*)

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

(* }}} *)

(* {{{ Digit-by-digit algorithm
   ----------------------------------------------------------------------------
*)

module type ExtendedNumberSystem = sig
  type t
  val zero : t
  val sub : t -> t -> t
  val compare : t -> t -> int
  val base : int
  val digits : t -> int array
  val add_digit : t -> int -> t
  val mul_digit : t -> int -> t
end

(** The minimal integer numbers signature required by
    [ExtendedNumberSystemOfIntModule].  All the built-in integer modules fit
    this signature except [Int]. *)
module type IntModule = sig
  type t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val compare : t -> t -> int
  val of_string : string -> t
  val of_int : int -> t
  val to_int : t -> int
end

(** The functor that creates [ExtendedNumberSystem] by a built-in integer
    numbers module. *)
module ExtendedNumberSystemOfIntModule
    (NS : IntModule) : ExtendedNumberSystem with type t = NS.t =
struct
  include NS

  let base = 10

  let ten = of_string "10"

  let digits i =
    let rec digits_iter accu i =
      if compare i zero = 0 then
        accu
      else
        digits_iter (to_int (rem i ten) :: accu ) (div i ten)
    in
    if compare i zero > 0 then
      digits_iter [] i |> List.rev |> Array.of_list
    else
      [| 0 |]

  let add_digit x d = add x (of_int d)

  let mul_digit x d = mul x (of_int d)
end

module ExtendedInt = ExtendedNumberSystemOfIntModule (struct
    include Int
    let of_string = int_of_string
    let of_int = Fun.id
    let to_int = Fun.id
  end)

module ExtendedNativeint = ExtendedNumberSystemOfIntModule (Nativeint)

module ExtendedInt32 = ExtendedNumberSystemOfIntModule (Int32)

module ExtendedInt64 = ExtendedNumberSystemOfIntModule (Int64)

module ExtendedBigNat = struct
  include BigNat
  let base = BigNat.Internals.base
  let digits = BigNat.Internals.digits
  let add_digit = BigNat.Internals.add_digit
  let mul_digit = BigNat.Internals.mul_digit
end

module ExtendedBigInt = struct
  include BigInt
  let base = BigInt.Internals.base
  let digits = BigInt.Internals.digits
  let add_digit = BigInt.Internals.add_digit
  let mul_digit = BigInt.Internals.mul_digit
end

(** [digit_by_digit x] operates on the digits of [x] and calculates each digit
    of the square root in sequence from the most significant digit to the least
    significant.

    Links:

    - S. Okulov. Programming algorithms
    - https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Digit-by-digit_calculation
*)
let digit_by_digit (type a) (module NS : ExtendedNumberSystem with type t = a) x =
  let open NS in

  let ( *: ) = mul_digit and ( +: ) x y = add_digit x y in
  let digits = digits x in
  let len = Array.length digits in

  (* [find_isqrt_digit cur res l r] finds the next digit of the square root.
     It performs a binary search to find the maximum digit such that when
     combined with the partial result [res] and squared, it remains less than
     or equal to the currently considered part of the original number [x]. *)
  let rec find_isqrt_digit cur res l r =
    let m = l + (r - l) / 2 in
    let v = (res *: base *: 2 +: m) *: m in
    let c = compare cur v in

    if l + 1 >= r then
      if c >= 0 then
        (m, v)
      else
        (m - 1, (res *: base *: 2 +: (m - 1)) *: (m - 1))
    else if c < 0 then
      find_isqrt_digit cur res l m
    else if c > 0 then
      find_isqrt_digit cur res (m + 1) r
    else
      (m, v)
  in

  (* [find_isqrt_digits cur res i] recursively computes each digit of the
     square root by considering two digits of [x] at a time.  It uses
     [find_isqrt_digit] to find each square root digit and accumulate the
     result in [res]. *)
  let rec find_isqrt_digits cur res i =
    if i < 0 then
      res
    else
      let d1 = if i * 2 + 1 < len then digits.(i * 2 + 1) else 0
      and d2 = digits.(i * 2) in
      let cur' = (cur *: base +: d1) *: base +: d2 in
      let d, v = find_isqrt_digit cur' res 0 base in
      let res' = res *: base +: d in
      find_isqrt_digits (sub cur' v) res' (i - 1)
  in

  if compare x zero < 0 then
    failwith "isqrt"
  else
    find_isqrt_digits zero zero ((len - 1) / 2)

(* }}} *)
