val find : (module OrderedType.S with type t = 'a) -> 'a array -> 'a -> int option
(** [find (module OT) arr x] performs a binary search in the sorted array [arr]
    for an element equal to [x], according to [OT.compare].  The function
    requires [arr] to be sorted in non-decreasing order.  The function returns
    [Some i], where [i] is the index of an occurrence of [x] in [arr] if it
    exists, or [None] if [x] is not found.

    Note: The function may not necessarily return the first occurrence of [x]
    if [x] appears multiple times in the array. *)
