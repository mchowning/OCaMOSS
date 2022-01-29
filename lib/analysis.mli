type hashLocation = {
  path: string;
  line: int;
}

type locations = {
  needle_line: int;
  haystack_line: int;
}

type hashMatch = {
  needle: hashLocation;
  haystack: hashLocation;
}

val find_matches: (string, Winnowing.fingerprint list) Hashtbl.t
               -> (string, Winnowing.fingerprint list) Hashtbl.t 
               -> hashMatch list

val hash_matches_to_table: hashMatch list
                        -> (string, (string, locations list) Base.Hashtbl.t) Base.Hashtbl.t

val analyze: (string, Winnowing.fingerprint list) Hashtbl.t 
          -> (string, Winnowing.fingerprint list) Hashtbl.t 
          -> unit