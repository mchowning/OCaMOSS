type hashLocation = {
  path: string;
  start_index: int;
  end_index: int
}

type location = {
  start_index: int;
  end_index: int
}

type locations = {
  needle: location;
  haystack: location;
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

val find_matches: (string, Preprocessing.indexed_hash list) Base.Hashtbl.t
               -> (string, Preprocessing.indexed_hash list) Base.Hashtbl.t 
               -> hashMatch list

val hash_matches_to_table: hashMatch list
                        -> (string, (string, hashMatch list) Base.Hashtbl.t) Base.Hashtbl.t

val analyze: (string, Preprocessing.indexed_hash list) Base.Hashtbl.t 
          -> (string, Preprocessing.indexed_hash list) Base.Hashtbl.t 
          -> string option
          -> analysis_info
          -> unit