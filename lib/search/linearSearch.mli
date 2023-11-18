val find_in_array :
  (module OrderedType.S with type t = 'a) -> 'a array -> 'a -> int option
(** [find_in_array (module OT) arr x] performs a linear search in the array
    [arr] for an element equal to [x], according to [OT.compare].  The function
    returns [Some i], where [i] is the index of the first occurrence of [x] in
    [arr] if it exists, or [None] if [x] is not found. *)

val find_in_list :
  (module OrderedType.S with type t = 'a) -> 'a list -> 'a -> int option
(** [find_in_list (module OT) lst x] performs a linear search in the list [lst]
    for an element equal to [x], according to [OT.compare].  The function
    returns [Some i], where [i] is the index of the first occurrence of [x] in
    [lst] if it exists, or [None] if [x] is not found. *)
