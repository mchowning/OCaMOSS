type hashLocation = {
  path: string;
  index: int;
}

type locations = {
  needle_index: int;
  haystack_index: int;
}

type hashMatch = {
  needle: hashLocation;
  haystack: hashLocation;
}

type analysis_info = {
  needles_path: string;
  haystack_path: string;
  guarantee_threshold: int;
  min_threshhold: int;
}

val find_matches: (string, Winnowing.fingerprint list) Base.Hashtbl.t
               -> (string, Winnowing.fingerprint list) Base.Hashtbl.t 
               -> hashMatch list

val hash_matches_to_table: hashMatch list
                        -> (string, (string, locations list) Base.Hashtbl.t) Base.Hashtbl.t

val analyze: (string, Winnowing.fingerprint list) Base.Hashtbl.t 
          -> (string, Winnowing.fingerprint list) Base.Hashtbl.t 
          -> string option
          -> analysis_info
          -> unit