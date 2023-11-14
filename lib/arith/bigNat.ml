(** Arbitrary-precision arithmetic on natural numbers.

    The main idea is that the number is stored as an array of its "digits" in
    some base.  In this implementation, the base is a power of [10], as this is
    convenient for some algorithms.

    Operations on numbers in this form are performed using "school" algorithms
    of column addition, subtraction, multiplication, and division.

    Links:

    - S. Okulov. Programming algorithms
    - https://cp-algorithms.com/algebra/big-integer.html
*)

(* {{{ Natural numbers
   ----------------------------------------------------------------------------
*)

type t = {
  num: int array;
  (** The digits of the natural number stored in order from least to most
      significant. *)

  len: int;
  (** The number of digits in the big natural number (the underlying array may
      contain leading zeros and therefore be longer than the actual length of
      the number). *)
}

let zero = { num = [| 0 |]; len = 1 }

let one = { num = [| 1 |]; len = 1 }

(* }}} *)

(* {{{ Helper functions and values
   ----------------------------------------------------------------------------
*)

(** [digits_number n] is the number of digits in [n].
    Requires [n >= 0]. *)
let digits_number n = (float n |> log10) +. 1. |> int_of_float |> Int.max 1

(** [pow b n] returns [b] raised to the power of [n].
    Requires [n >= 0]. *)
let rec pow b n =
  if n = 0 then 1 else b * pow b (n - 1)

(** A base used to represent natural numbers as arrays.
    This number is the maximum power of [10] such that its square is
    representable as a built-in integer. *)
let base =
  pow 10 ((Int.max_int |> float |> sqrt |> int_of_float |> digits_number) - 1)

(** Length of the [base] in digits. *)
let dlen = digits_number (base - 1)

(** [normalize x] ensures that the [len] field accurately reflects the number
    of significant "digits" in the [x]. *)
let normalize x =
  let rec normalize_iter i =
    if i = 1 || x.num.(i - 1) > 0 then
      i
    else
      normalize_iter (i - 1)
  in { x with len = normalize_iter x.len }

(* }}} *)

(* {{{ Comparisons
   ----------------------------------------------------------------------------
*)

(** [compare_shift x y shift] compares two big natural numbers [x] and [y],
    treating [x] as if it were shifted right by [shift] positions (discarding
    low-order digits).  This function returns [0] if [x] is equal to [y] after
    the shift, [1] if it is greater, and [-1] if it is smaller.
    Requires [shift >= 0]. *)
let compare_shift { num = num1; len = len1 } { num = num2; len = len2 } shift =
  let rec compare_shift_iter i =
    if i < 0 then 0
    else if num1.(i + shift) < num2.(i) then -1
    else if num1.(i + shift) > num2.(i) then 1
    else compare_shift_iter (i - 1)
  in
  if len1 - shift < len2 then -1
  else if len1 - shift > len2 then 1
  else compare_shift_iter (len2 - 1)

let compare x y = compare_shift x y 0

let equal x y = compare x y = 0

let min x y = if compare x y <= 0 then x else y

let max x y = if compare x y >= 0 then x else y

(* }}} *)

(* {{{ Hash function
   ----------------------------------------------------------------------------
*)

let hash x =
  let p = 0x3fffffdd  (* maximum prime number less than 2^30 *)
  and m = 0x3fffffff  (* 2^30 - 1 *)
  in
  let rec hash_iter acc i =
    if i < 0 then
      acc
    else
      hash_iter ((acc * p + Int.hash x.num.(i)) land m) (i - 1)
  in hash_iter 0 (x.len - 1)

(* }}} *)

(* {{{ Arithmetic operations
   ----------------------------------------------------------------------------
*)

let add x y =
  let maxlen = Int.max x.len y.len in
  let num = Array.make (maxlen + 1) 0 in

  let carry = ref 0 in
  for i = 0 to maxlen do
    let xi = if i < x.len then x.num.(i) else 0
    and yi = if i < y.len then y.num.(i) else 0 in
    let sum = xi + yi + !carry in
    num.(i) <- sum mod base;
    carry := sum / base
  done;

  normalize { num; len = maxlen + 1 }

let sub x y =
  let num = Array.copy x.num in

  let rec sub_iter i borrow =
    if i < y.len || borrow > 0 then begin
      let yi = if i < y.len then y.num.(i) else 0 in
      num.(i) <- num.(i) - yi - borrow;
      if num.(i) < 0 then begin
        num.(i) <- num.(i) + base;
        sub_iter (i + 1) 1
      end else
        sub_iter (i + 1) 0
    end
  in
  if compare x y >= 0 then begin
    sub_iter 0 0;
    normalize { num; len = x.len }
  end else
    failwith "sub"

let mul x y  =
  let num = Array.make (x.len + y.len) 0 in

  for j = 0 to y.len - 1 do
    let carry = ref 0 in
    for i = 0 to x.len - 1 do
      let res = num.(i + j) + x.num.(i) * y.num.(j) + !carry in
      num.(i + j) <- res mod base;
      carry := res / base;
    done;
    num.(x.len + j) <- !carry;
  done;

  normalize { num; len = x.len + y.len }

(** [sub_shift_inplace x y shift] substracts [y] from [x], treating [y] as if
    it were shifted left by [shift] positions.  The operation is done in-place:
    [x]'s underlying array is directly modified to store the result in order to
    reduce the number of allocations.
    This function returns the normalized result of the substraction.
    Requires [shift >= 0] and [x >= y'] where [y'] is the shifted [y]. *)
let sub_shift_inplace x y shift =
  let rec sub_shift_inplace_iter i borrow =
    if i < y.len || borrow > 0 then begin
      let yi = if i < y.len then y.num.(i) else 0 in
      let i' = i + shift in
      x.num.(i') <- x.num.(i') - yi - borrow;
      if x.num.(i') < 0 then begin
        x.num.(i') <- x.num.(i') + base;
        sub_shift_inplace_iter (i + 1) 1
      end else
        sub_shift_inplace_iter (i + 1) 0
    end
  in
  sub_shift_inplace_iter 0 0;
  normalize x

(** [mul_by_int x y] is the multiplication [x * y].
    Requires: [n < base]. *)
let mul_by_int x y =
  let num = Array.make (x.len + 1) 0 in
  Array.blit x.num 0 num 0 x.len;

  let carry = ref 0 in
  for i = 0 to x.len - 1 do
    let res = num.(i) * y + !carry in
    num.(i) <- res mod base;
    carry := res / base
  done;
  num.(x.len) <- !carry;

  normalize { num; len = x.len + 1 }

let divmod x y =
  let quot_len = Int.max 1 (x.len - y.len + 1) in
  let quot = { num = Array.make quot_len 0; len = quot_len }
  and rem = ref ({ x with num = Array.copy x.num }) in

  (* Find the maximum factor [f] such that [x > y' * f] where [y'] is the
     shifted [y].  The function returns the pair of numbers: [f] and
     [y * f]. *)
  let rec find_factor x y shift l r =
    let m = l + (r - l) / 2 in
    let p = mul_by_int y m in
    let c = compare_shift x p shift in

    if l + 1 >= r then
      if c >= 0 then (m, p) else (m - 1, mul_by_int y (m - 1))
    else if c < 0 then
      find_factor x y shift l m
    else if c > 0 then
      find_factor x y shift (m + 1) r
    else
      (m, p)
  in

  let rec divmod_iter shift =
    if shift < 0 then
      normalize quot, !rem
    else
      let factor, product = find_factor !rem y shift 0 base in
      quot.num.(shift) <- factor;
      rem := sub_shift_inplace !rem product shift;
      divmod_iter (shift - 1)
  in

  if not (equal y zero) then
    divmod_iter (x.len - y.len)
  else
    raise Division_by_zero

let div x y = divmod x y |> fst

let rem x y = divmod x y |> snd

let succ x = add x one

let pred x = sub x one

(* }}} *)

(* {{{ Conversion to and from other numerical types
   ----------------------------------------------------------------------------
*)

let of_int n =
  let cap = (digits_number n + dlen - 1) / dlen in
  let num = { num = Array.make cap 0; len = cap } in

  let rec of_int_iter n i =
    if i < cap then begin
      num.num.(i) <- n mod base;
      of_int_iter (n / base) (i + 1)
    end
  in

  if n >= 0 then begin
    of_int_iter n 0;
    num
  end
  else
    failwith "of_int"

let is_int x =
  compare x (of_int Int.max_int) <= 0

let to_int_opt x =
  let rec to_int_iter n i =
    if i < 0 then
      n
    else
      to_int_iter (n * base + x.num.(i)) (i - 1)
  in

  if is_int x then
    Some (to_int_iter 0 (x.len - 1))
  else
    None

let to_int x =
  match to_int_opt x with
  | Some n -> n
  | None   -> failwith "to_int"

(* }}} *)

(* {{{ Conversion to and from strings
   ----------------------------------------------------------------------------
*)

let to_string { num; len } =
  let buf = Buffer.create ((len - 1) * dlen + digits_number num.(len - 1)) in

  num.(len - 1) |> string_of_int |> Buffer.add_string buf;
  for i = len - 2 downto 0 do
    let s = string_of_int num.(i) in
    for _ = 0 to dlen - (String.length s) - 1 do
      Buffer.add_char buf '0'
    done;
    Buffer.add_string buf s
  done;

  Buffer.contents buf

let of_string_opt str =
  let len = String.length str in
  let num = Array.make ((len - 1) / dlen + 1) 0 in

  let rec of_string_iter i j =
    if j <= dlen then
      num.(i) <- String.sub str 0 j |> int_of_string
    else begin
      num.(i) <- String.sub str (j - dlen) dlen |> int_of_string;
      of_string_iter (i + 1) (j - dlen)
    end
  in

  try
    of_string_iter 0 len;
    Some (normalize { num; len = Array.length num })
  with
    Failure _ -> None

let of_string str =
  match of_string_opt str with
  | Some x -> x
  | None   -> failwith "of_string"

(* }}} *)
