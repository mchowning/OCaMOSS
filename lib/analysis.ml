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

let hash_match_table_print hm_tbl =
  (* let sort_haystack_lengths = 
    let one_level_ls = Base.Hashtbl.to_alist hm_tbl in
    Base.List.map one_level_ls ~f:(fun (a, b) -> 
      let two_level_ls = Base.Hashtbl.to_alist b in
      let sorted_two_level_ls = Base.List.stable_sort two_level_ls ~compare:(fun (_,v1) (_,v2) ->
        compare (Base.List.length v1) (Base.List.length v2)
      ) in
      (a, sorted_two_level_ls)
      ) in *)

  (* let sorted_by_num_haystack_matches = 
    let one_level_ls = Base.Hashtbl.to_alist hm_tbl in
    let two_level_ls = Base.List.map one_level_ls ~f:(fun (a, b) -> (a, Base.Hashtbl.to_alist b)) in
    Base.List.stable_sort two_level_ls 
      ~compare:(fun (_,h1) (_,h2) -> 
        let haystacks_by_length hs = Base.List.stable_sort hs 
              ~compare:(fun (_,v1) (_,v2) -> 
                 compare (List.length v1) (List.length v2)
               ) in
        
        let h1_by_length = haystacks_by_length h1 in
        let h2_by_length = haystacks_by_length h2 in
        compare  _ 
      ) *)

  let needles = Base.Hashtbl.to_alist hm_tbl in
  Base.List.iter needles ~f:(fun (needle, haystack_tbl) -> 
    Printf.printf "%s\n%!" needle;
    let haystacks = Base.Hashtbl.to_alist haystack_tbl in
    Base.List.iter haystacks ~f:(fun (haystack, matches) -> 
      Printf.printf "    %s\n%!" haystack;
      Base.List.iter matches ~f:(fun locs -> 
        Printf.printf "        %d %d\n%!" locs.needle_line locs.haystack_line
      )
    )
  )

let analyze needles haystack =
  find_matches needles haystack 
    |> hash_matches_to_table 
    |> hash_match_table_print


(* FIXME: Remove??? *)
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