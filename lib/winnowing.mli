type fingerprint = {
	hash: int;
	location: int;
}

(* [winnow w h] applies the winnowing algorithm for a list of hashes [h]  with
 * window size [w] returns: a list of (v,p) tuples where v is a hash value and
 * p is the position in the original input the (v,p) list is not guaranteed to
 * be in any particular order requires: [w] is a positive integer
 *)
(* val winnow: int -> int list -> fingerprint list *)
val winnow: int -> Preprocessing.indexed_hash list -> Preprocessing.indexed_hash list
