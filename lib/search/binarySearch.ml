let find (type a) (module OT : OrderedType.S with type t = a) arr x =
  let rec binary_search l r =
    if l >= r then
      None
    else
      let m = l + (r - l) / 2 in
      let c = OT.compare x arr.(m) in
      if c < 0 then
        binary_search l m
      else if c > 0 then
        binary_search (m + 1) r
      else
        Some m
  in binary_search 0 (Array.length arr)
