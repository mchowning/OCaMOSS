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
              { needle = { path = needle_path; line = fingerprint.location }; 
                haystack = { path = haystack_location.path; line = haystack_location.line };
              }
            ) ls

    ) needle_fingerprints in
    acc @ matches
  ) needles [];
end

let hash_matches_to_table (hms: hashMatch list) = begin

  let tbl = Hashtbl.create (List.length hms) in
  Base.List.iter hms ~f:(fun hm -> begin
      let tbl' = match Hashtbl.find_opt tbl hm.needle.path with
        | None -> Hashtbl.create 5
        | Some t -> begin
              Hashtbl.remove tbl hm.needle.path;
              t
            end
      in

      let locations = {
        needle_line = hm.needle.line; 
        haystack_line = hm.haystack.line 
      } in

      (match Hashtbl.find_opt tbl' hm.haystack.path with
        | None -> begin
              Hashtbl.add tbl' hm.haystack.path [ locations ];
            end;
        | Some ls -> begin
            Hashtbl.remove tbl' hm.haystack.path;
            Hashtbl.add tbl' hm.haystack.path (locations :: ls);
          end);
      
      Hashtbl.add tbl hm.needle.path tbl'; 
      end
  );
  tbl
end

let by_file hash_locations = begin
  let result = Hashtbl.create 10 in
  List.iter (fun (hl: hashLocation) -> 
      let value = match Hashtbl.find_opt result hl.path with
        | None -> [hl.line]
        | Some ls -> ls @ [hl.line] (* append to end to maintain order *)
      in
      Hashtbl.remove result hl.path;
      Hashtbl.add result hl.path value
    ) hash_locations;
  result
end