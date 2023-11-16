(** Arbitrary-precision arithmetic on integers.

    The implementation of this module relies on the implementation of big
    natural numbers: a big integer is represented by its magnitude (a natural
    number) and sign.
*)

(* {{{ Big integers
    ---------------------------------------------------------------------------
*)

type t = {
  mag: BigNat.t;
  (** The magnitude of the big integer represented by a big natural number. *)

  sign: int;
  (** The sign of the big integer: [1] if the integer is positive or equal to
      zero, [-1] otherwise. *)
}

let zero = { mag = BigNat.zero; sign = 1 }

let one = { mag = BigNat.one; sign = 1 }

let minus_one = { mag = BigNat.one; sign = -1 }

(* }}} *)

(* {{{ Comparisons
   ----------------------------------------------------------------------------
*)

let compare { mag = xmag; sign = xsign } { mag = ymag; sign = ysign } =
  let open BigNat in
  if xsign * ysign = 1 then
    xsign * compare xmag ymag
  else
    xsign

let equal x y =
  compare x y = 0

let min x y =
  if compare x y <= 0 then x else y

let max x y =
  if compare x y >= 0 then x else y

(* }}} *)

(* {{{ Hash function
   ----------------------------------------------------------------------------
*)

let hash { mag; sign } =
  let p = 0x3fffffdd  (* maximum prime number less than 2^30 *)
  and m = 0x3fffffff  (* 2^30 - 1 *)
  in
  (BigNat.hash mag * p + Int.hash sign) land m

(* }}} *)

(* {{{ Arithmetic operations
   ----------------------------------------------------------------------------
*)

let sign { mag; sign } =
  if BigNat.(equal mag zero) then 0 else sign

let neg { mag; sign } =
  { mag; sign = if BigNat.(equal mag zero) then 1 else -sign }

let abs { mag; _ } = { mag; sign = 1 }

let add { mag = xmag; sign = xsign } { mag = ymag; sign = ysign } =
  let open BigNat in
  if xsign * ysign > 0 then
    { mag = add xmag ymag; sign = xsign }
  else
    let c = compare xmag ymag in
    if c > 0 then      { mag = sub xmag ymag; sign = xsign }
    else if c < 0 then { mag = sub ymag xmag; sign = ysign }
    else               { mag = sub xmag ymag; sign = 1 }

let sub x y = add x (neg y)

let mul { mag = xmag; sign = xsign } { mag = ymag; sign = ysign } =
  let mag = BigNat.mul xmag ymag in
  { mag; sign = if BigNat.(equal mag zero) then 1 else xsign * ysign }

let divmod { mag = xmag; sign = xsign } { mag = ymag; sign = ysign } =
  let open BigNat in
  let q, r = divmod xmag ymag in
  (
    { mag = q; sign = if BigNat.(equal q zero) then 1 else xsign * ysign },
    { mag = r; sign = if BigNat.(equal r zero) then 1 else xsign }
  )

let div x y = divmod x y |> fst

let rem x y = divmod x y |> snd

let succ x = add x one

let pred x = sub x one

(* }}} *)

(* {{{ Conversion to and from other numerical types
   ----------------------------------------------------------------------------
*)

let of_int i =
  { mag = BigNat.of_int (Int.abs i); sign = if i >= 0 then 1 else -1 }

let is_int { mag; _ } = BigNat.is_int mag

let to_int_opt { mag; sign } =
  BigNat.to_int_opt mag |> Option.map (( * ) sign)

let to_int x =
  match to_int_opt x with
  | Some n -> n
  | None   -> failwith "to_int"

let of_nat n = { mag = n; sign = 1 }

let to_nat_opt { mag; sign } =
  if sign = 1 then Some mag else None

let to_nat x =
  match to_nat_opt x with
  | Some x -> x
  | None   -> failwith "to_nat"

(* }}} *)

(* {{{ Conversion to and from string
   ----------------------------------------------------------------------------
*)

let to_string { mag; sign } =
  let str = BigNat.to_string mag in
  if sign = 1 then str else "-" ^ str

let of_string_opt str =
  let open BigNat in
  let sign = if str <> "" && str.[0] = '-' then -1 else 1 in
  let str = if sign = 1 then str else String.(sub str 1 (length str - 1))
  in
  of_string_opt str
  |> Option.map (fun mag -> { mag; sign = if equal mag zero then 1 else sign })

let of_string str =
  match of_string_opt str with
  | Some x -> x
  | None   -> failwith "of_string"

(* }}} *)
