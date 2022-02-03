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

    (* |> handle_io ~json_filename:json_filename 
                 ~needles_fingerprint_tbl:shortened_needles 
                 ~needles_path:needles_path_with_trailing_slash
                 ~haystack_fingerprint_tbl:shortened_haystack 
                 ~haystack_path:haystack_path_with_trailing_slash *)

let convertToHashDict fileHashDict = begin
  (* let result = Hashtbl.create 10000 in
  Hashtbl.iter (fun path fingerprints ->
    List.iter (fun (fingerprint: Winnowing.fingerprint) -> 
      let newLocation = { path; index = fingerprint.location } in
      let newValue = match (Hashtbl.find_opt result fingerprint.hash) with
            | None -> [newLocation]
            | Some ls -> newLocation :: ls in
      Hashtbl.add result fingerprint.hash newValue;
    ) fingerprints
  ) fileHashDict;
  result *)

  let result = Base.Hashtbl.create (module Base.Int) in
  Base.Hashtbl.iteri fileHashDict ~f:(fun ~key:path ~data:fingerprints ->
    List.iter (fun (fingerprint: Winnowing.fingerprint) -> 
      let newLocation = { path; index = fingerprint.location } in
      let newValue = match (Base.Hashtbl.find result fingerprint.hash) with
            | None -> [newLocation]
            | Some ls -> newLocation :: ls in
      Base.Hashtbl.set result ~key:fingerprint.hash ~data:newValue
    ) fingerprints
  );
  result
end

let find_matches needles haystack = begin
  let haystackHashDict = convertToHashDict haystack in
  Base.Hashtbl.fold needles ~init:[] ~f:(fun ~key:needle_path ~data:needle_fingerprints acc ->
    let matches = List.concat_map (fun (fingerprint: Winnowing.fingerprint) ->
      match (Base.Hashtbl.find haystackHashDict fingerprint.hash) with

        (* No matching hashes in the haystack *)
        | None -> []

        (* Record matching hashes in the haystack *)
        | Some ls -> 
            List.map (fun haystack_location -> 
              { needle = { path = needle_path; index = fingerprint.location }; 
                haystack = { path = haystack_location.path; index = haystack_location.index };
              }
            ) ls

    ) needle_fingerprints in
    acc @ matches
  );
end

let hash_matches_to_table (hms: hashMatch list) = begin
  let tups = Base.List.map hms ~f:(fun hm ->
    (hm.needle.path, 
      (hm.haystack.path, { needle_index = hm.needle.index; 
                           haystack_index = hm.haystack.index }))
  ) in
  let needle_key_name = Base.Hashtbl.of_alist_multi (module Base.String) tups in
  Base.Hashtbl.map needle_key_name ~f:(Base.Hashtbl.of_alist_multi (module Base.String))
end

let sort_haystack_tup_by_match_ratio (name1, matches1, total1) (name2, matches2, total2)=
  let n1 = Int.to_float (List.length matches1) /. Int.to_float total1 in
  let n2 = Int.to_float (List.length matches2) /. Int.to_float total2 in
  if (n1 != n2) then
    compare n2 n1
  else if (total1 != total2) then
    compare total2 total1
  else
    compare name1 name2

let sort_needle_by_num_matches (needle1, haystack_tups1) (needle2, haystack_tups2) =
  let matches ls = 
    Base.List.sum (module Base.Int) ls ~f:(fun (_, matches, _) -> List.length matches)
    |> Int.to_float
  in
  let total ls = 
    Base.List.sum (module Base.Int) ls ~f:(fun (_, _, total) -> total)
    |> Int.to_float
  in
  let match_ratio ls = 
    matches ls /. total ls in
  let n_matches_1 = match_ratio haystack_tups1 in
  let n_matches_2 = match_ratio haystack_tups2 in
  if (n_matches_1 != n_matches_2) then
    compare n_matches_2 n_matches_1
  else
    let total1 = total haystack_tups1 in
    let total2 = total haystack_tups2 in
    if (total1 != total2) then
      compare total2 total1
    else
      compare needle1 needle2


(* let hash_match_table_json_file ~json_filename:json_filename 
                               ~needles_fingerprint_tbl:needles_fingerprint_tbl 
                               ~needles_path:needles_path
                               ~haystack_fingerprint_tbl:haystack_fingeprint_tbl 
                               ~haystack_path:haystack_path
                               ~hm_tbl:hm_tbl =  *)
let hash_match_table_json_file ~json_filename:json_filename
                               ~analysis_info:(info: analysis_info)
                               ~needles_fingerprint_tbl:needles_fingerprint_tbl 
                               ~haystack_fingerprint_tbl:haystack_fingeprint_tbl 
                               ~hm_tbl:hm_tbl = 
  let needles = Base.Hashtbl.to_alist hm_tbl in
  let needles_with_tup = Base.List.map needles ~f:(fun (needle, haystack_tbl) ->
    let haystacks = Base.Hashtbl.to_alist haystack_tbl in
    (needle, haystacks)
  ) in
  let flattened = Base.List.concat_map needles_with_tup ~f:(fun (needle, ls) -> 
    Base.List.concat_map ls ~f:(fun (haystack, locations_list) ->
      Base.List.map locations_list ~f:(fun locations ->
        { needle = 
            { path = needle; 
              index = locations.needle_index };
          haystack = 
            { path = haystack; 
              index = locations.haystack_index }
        }
      )
    )
  ) in

  let open Yojson in
    let matches = `List 
      (Base.List.map flattened ~f:(fun (hm: hashMatch) -> 
        `Assoc [
          ("needle_path", `String hm.needle.path);
          ("needle_index", `Int hm.needle.index);
          ("haystack_path", `String hm.haystack.path);
          ("haystack_index", `Int hm.haystack.index)
        ]
      )) in

    let file_object tb file_type = 
      let alist = Base.Hashtbl.to_alist tb in
      Base.List.map alist ~f:(fun (file, (fingerprints: Winnowing.fingerprint list)) -> 
        let total = List.length fingerprints in
        (file, `Assoc [
          ("type", `String file_type);
          ("total", `Int total)]
      )) in

    let needle_files = file_object needles_fingerprint_tbl "needle" in
    let haystack_files = file_object haystack_fingeprint_tbl "haystack" in

    let output = `Assoc [
      ("config", `Assoc [
        ("paths", `Assoc [
          ("neeedles", `String info.needles_path);
          ("haystack", `String info.haystack_path)
        ]);
        ("thresholds", `Assoc [
          ("guarantee", `Int info.guarantee_threshold);
          ("min", `Int info.min_threshhold);
        ]);
      ]);
      ("files", `Assoc [
        ("needles", `Assoc needle_files);
        ("haystack", `Assoc haystack_files)
      ]);
      ("matches", matches)
    ] in
    Yojson.to_file (json_filename ^ ".json") output

let hash_match_table_print needles_tb hm_tbl =

  print_endline "\nRESULTS";
  let needles = Base.Hashtbl.to_alist hm_tbl in
  let needles_with_tup = Base.List.map needles ~f:(fun (needle, haystack_tbl) ->
    let haystacks = Base.Hashtbl.to_alist haystack_tbl in
    let haystacks_with_total = Base.List.map haystacks ~f:(fun (haystack, matches) ->
      let total = Base.Hashtbl.find_exn needles_tb needle |> List.length in
      (haystack, matches, total)
    ) in
    (needle, haystacks_with_total)
  ) in
  let sorted_needles = List.sort sort_needle_by_num_matches needles_with_tup in
  Base.List.iter sorted_needles ~f:(fun (needle, haystack_tup) -> 
    Printf.printf "\n%s\n" needle;
    let sorted_haystacks = List.sort sort_haystack_tup_by_match_ratio haystack_tup in
    Base.List.iter sorted_haystacks ~f:(fun (haystack, matches, total) -> 
      let num_matches = List.length matches in
      Printf.printf "%5d / %-5d  %s\n%!" num_matches total haystack;
    )
  )

let handle_io ~json_filename:json_filename 
              ~analsysi_info:info
              ~needles_fingerprint_tbl:needles_fingerprint_tbl 
              ~haystack_fingerprint_tbl:haystack_fingerprint_tbl 
              hm_tbl =
  (match json_filename with
   | None -> ()
   | Some json_filename -> hash_match_table_json_file ~json_filename:json_filename 
                                                      ~analysis_info:info
                                                      ~needles_fingerprint_tbl:needles_fingerprint_tbl 
                                                      ~haystack_fingerprint_tbl:haystack_fingerprint_tbl 
                                                      ~hm_tbl:hm_tbl);
  hash_match_table_print needles_fingerprint_tbl hm_tbl

let top_level_dir_name path =
 let up_to_top_level = (Filename.dirname path) in
  let top_level_dir = Base.String.chop_prefix_exn path ~prefix:up_to_top_level in
  let without_prefix_slash = Base.String.chop_prefix_exn top_level_dir ~prefix:"/" in
  Base.String.chop_suffix_if_exists without_prefix_slash ~suffix:"/"
  (* let has_trailing_slash = Base.String.is_suffix without_prefix_slash ~suffix:"/" in
  if has_trailing_slash then
    without_prefix_slash
  else
    without_prefix_slash ^ "/" *)

let update_keys tbl ~f:f =
  let alist = Base.Hashtbl.to_alist tbl in
  let updated = Base.List.map alist ~f:(fun (key, value) ->
    (f key, value)
  ) in
  Base.Hashtbl.of_alist_exn (module Base.String) updated


let analyze 
  (needles: (string, Winnowing.fingerprint list) Base.Hashtbl.t) 
  (haystack: (string, Winnowing.fingerprint list) Base.Hashtbl.t) 
  (json_filename: string option)
  (info: analysis_info) =

  (* let prefix_remove_and_add ~remove:remove ~add:add s = add ^ (Base.String.chop_prefix_exn s ~prefix:remove) in *)

  (* let needle_path_prefix = top_level_dir_name needles_path in
  let haystack_path_prefix = top_level_dir_name haystack_path in *)

  let path_with_trailing_slash p =
    if (Base.String.is_suffix p ~suffix:"/")
    then p
    else p ^ "/" in

  let needles_path_with_trailing_slash = path_with_trailing_slash info.needles_path in
  let haystack_path_with_trailing_slash = path_with_trailing_slash info.haystack_path in


  let shortened_needles = update_keys needles ~f:(Base.String.chop_prefix_exn ~prefix:needles_path_with_trailing_slash) in
  let shortened_haystack = update_keys haystack ~f:(Base.String.chop_prefix_exn ~prefix:haystack_path_with_trailing_slash) in

  find_matches shortened_needles shortened_haystack 
    |> hash_matches_to_table 
    |> handle_io ~json_filename:json_filename 
                 ~analsysi_info:info
                 ~needles_fingerprint_tbl:shortened_needles 
                 ~haystack_fingerprint_tbl:shortened_haystack 