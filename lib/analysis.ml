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
  let tups = Base.List.map hms ~f:(fun hm ->
    (hm.needle.path, 
      (hm.haystack.path, { needle_line = hm.needle.line; 
                           haystack_line = hm.haystack.line }))
  ) in
  let needle_key_name = Base.Hashtbl.of_alist_multi (module Base.String) tups in
  Base.Hashtbl.map needle_key_name ~f:(Base.Hashtbl.of_alist_multi (module Base.String))
end

let hash_match_table_print needles_tb hm_tbl =
  print_endline "\nRESULTS";
  let needles = Base.Hashtbl.to_alist hm_tbl in
  Base.List.iter needles ~f:(fun (needle, haystack_tbl) -> 
    let haystacks = Base.Hashtbl.to_alist haystack_tbl in
    Base.List.iter haystacks ~f:(fun (haystack, matches) -> 
      let total = Hashtbl.find needles_tb needle |> List.length in
      let num_matches = List.length matches in
      Printf.printf "\n%4d / %-4d  %s\n             %s\n%!" num_matches total needle haystack;
    )
  )

let analyze needles haystack =
  find_matches needles haystack 
    |> hash_matches_to_table 
    |> hash_match_table_print needles