open Algorithms_arith

let strbignat_gen =
  let open QCheck2.Gen in
  string_of numeral >|= (fun s -> if s = "" then "0" else s)

let strbigint_gen =
  let open QCheck2.Gen in
  map2
    (fun strnum sign -> (if sign then "" else "-") ^ strnum)
    strbignat_gen
    bool

let bignat_gen =
  QCheck2.Gen.(strbignat_gen >|= BigNat.of_string)

let bigint_gen =
  QCheck2.Gen.(strbigint_gen >|= BigInt.of_string)

let normalize_strnum s =
  let open Str in
  let re = regexp "^\\(-?\\)0*\\(.*\\)$" in
  ignore (string_match re s 0);
  let ns = matched_group 1 s ^ matched_group 2 s in
  if ns = "-" || ns = "" then "0" else ns
