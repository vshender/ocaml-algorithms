let find_in_array (type a) (module OT : OrderedType.S with type t = a) arr x =
  let len = Array.length arr in
  let rec search_iter i =
    if i = len then
      None
    else if OT.compare arr.(i) x = 0 then
      Some i
    else
      search_iter (i + 1)
  in search_iter 0

let find_in_list (type a) (module OT : OrderedType.S with type t = a) lst x =
  let rec search_iter i = function
    | []        -> None
    | y :: rest ->
      if OT.compare x y = 0 then
        Some i
      else
        search_iter (i + 1) rest
  in search_iter 0 lst
