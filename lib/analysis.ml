
type hashLocation = {
  path: string;
  line: int;
}

type hashMatch = {
  needle_path: string;
  needle_line: int;
  haystack_path: string;
  haystack_line: int;
}

let convertToHashDict fileHashDict = begin
  let result = Hashtbl.create 10000 in
  Hashtbl.iter (fun path fingerprints ->
    List.iter (fun (fingerprint: Winnowing.fingerprint) -> 
      let newLocation = { path; line = fingerprint.location } in
      let newValue = match (Hashtbl.find_opt result fingerprint.hash) with
            | None -> [newLocation]
            | Some ls -> newLocation :: ls in
      Hashtbl.add result fingerprint.hash newValue;
    ) fingerprints
  ) fileHashDict;
  result
end

let find_matches needles haystack = begin
  let haystackHashDict = convertToHashDict haystack in
  Hashtbl.fold (fun needle_path needle_fingerprints acc ->
    let matches = List.concat_map (fun (fingerprint: Winnowing.fingerprint) ->
      match (Hashtbl.find_opt haystackHashDict fingerprint.hash) with

        (* No matching hashes in the haystack *)
        | None -> []

        (* Record matching hashes in the haystack *)
        | Some ls -> 
            List.map (fun haystack_location -> 
              { needle_path; 
                needle_line = fingerprint.location; 
                haystack_path = haystack_location.path; 
                haystack_line = haystack_location.line }
            ) ls

    ) needle_fingerprints in
    acc @ matches
  ) needles [];
end

let by_file hash_locations = begin
  let result = Hashtbl.create 10 in
  List.iter (fun (hl: hashLocation) -> 
      let value = match Hashtbl.find_opt result hl.path with
        | None -> [hl.line]
        | Some ls -> hl.line :: ls 
      in
      Hashtbl.add result hl.path value
    ) hash_locations;
  result
end