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

    (* |> handle_io ~json_filename:json_filename 
                 ~needles_fingerprint_tbl:shortened_needles 
                 ~needles_path:needles_path_with_trailing_slash
                 ~haystack_fingerprint_tbl:shortened_haystack 
                 ~haystack_path:haystack_path_with_trailing_slash *)

let convertToHashDict fileHashDict = begin
  let result = Base.Hashtbl.create (module Base.Int) in
  Base.Hashtbl.iteri fileHashDict ~f:(fun ~key:path ~data:ihash_ls ->
    List.iter (fun (ihash: Preprocessing.indexed_hash) -> 
      let newLocation = { path; start_index = ihash.start_index; end_index = ihash.end_index } in
      let newValue = match (Base.Hashtbl.find result ihash.hash) with
            | None -> [newLocation]
            | Some ls -> newLocation :: ls in
      Base.Hashtbl.set result ~key:ihash.hash ~data:newValue
    ) ihash_ls
  );
  result
end

let same_extension path1 path2 =
  let exn1 = Filename.extension path1 in
  let exn2 = Filename.extension path2 in
  if (exn1 = exn2) then
    true
  else
    (* FIXME find a more robust way to check this *)
    let is_javascript a = a = ".js" || a = ".jsx" || a = ".ts" || a = ".tsx" in
    is_javascript exn1 && is_javascript exn2


let find_matches 
    (needles: (string, Preprocessing.indexed_hash list) Base.Hashtbl.t) 
    (haystack: (string, Preprocessing.indexed_hash list) Base.Hashtbl.t) 
  = begin
  let haystackHashDict = convertToHashDict haystack in
  Base.Hashtbl.fold needles ~init:[] ~f:(fun ~key:needle_path ~data:needle_fingerprints acc ->
    let matches = List.concat_map (fun (ihash: Preprocessing.indexed_hash) ->
      match (Base.Hashtbl.find haystackHashDict ihash.hash) with

        (* No matching hashes in the haystack *)
        | None -> []

        (* Record matching hashes in the haystack *)
        | Some ls -> 
            Base.List.concat_map ls ~f:(fun haystack_location -> 
              (* Only report match if the file extensions match *)
              if same_extension needle_path haystack_location.path then
                [ { needle = { path = needle_path; 
                            start_index = ihash.start_index; 
                            end_index = ihash.end_index }; 
                  haystack = { path = haystack_location.path; 
                              start_index = haystack_location.start_index; 
                              end_index = haystack_location.end_index };
                } ]
              else
                []
            )


    ) needle_fingerprints in
    acc @ matches
  );
end

let hash_matches_to_table (hms: hashMatch list) = begin
  let tups = Base.List.map hms ~f:(fun hm ->
    (hm.needle.path, 
      (hm.haystack.path, { needle = hm.needle;
                           haystack = hm.haystack }))
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
                               ~needles_tbl:needles_tbl 
                               ~haystack_tbl:haystack_tbl 
                               ~hm_tbl:hm_tbl = 

  let hm_tbl_combined = Base.Hashtbl.map hm_tbl ~f:(fun haystack_tbl -> 
    Base.Hashtbl.map haystack_tbl ~f:(fun hm_list -> 

      let adjoining hm1 hm2 =
        let adjoining_locations (hl1: hashLocation) (hl2: hashLocation) =

          let adjoins_start = 
            let hl1_start_edge = hl1.start_index - 1 in
            hl2.start_index <= hl1_start_edge && hl2.end_index >= hl1_start_edge
          in

          let adjoins_end =
            let hl1_end_edge = hl1.end_index + 1 in
            hl2.start_index <= hl1_end_edge && hl2.end_index >= hl1_end_edge
          in

          (* If either the start or end adjoins, then the locations adjoin *)
          adjoins_start || adjoins_end
        in

        (* Only consider adjoining if both the needle and haystack locations adjoin *)
        adjoining_locations hm1.needle hm2.needle 
          && adjoining_locations hm1.haystack hm2.haystack
      in

      let combine hm1 hm2 =
        if hm1.needle.path != hm2.needle.path then
          failwith "cannot combine hash matches from different needle files";
        if hm1.haystack.path != hm2.haystack.path then
          failwith "cannot combine hash matches from different haystack files";
        {
          needle = {
            path = hm1.needle.path;
            start_index = Base.Int.min hm1.needle.start_index hm2.needle.start_index;
            end_index = Base.Int.max hm1.needle.end_index hm2.needle.end_index;

          };
          haystack = {
            path = hm1.haystack.path;
            start_index = Base.Int.min hm1.haystack.start_index hm2.haystack.start_index;
            end_index = Base.Int.max hm1.haystack.end_index hm2.haystack.end_index;
          }
        }
      in

      let rec combine_overlapping hm_list = 
        match hm_list with
        | [] -> []
        | hm :: [] -> [hm]
        | (hm1 :: hm2 :: rest) -> 
              if adjoining hm1 hm2 then
                let combined = combine hm1 hm2
                in combine_overlapping (combined :: rest)
              else 
                hm1 :: combine_overlapping (hm2 :: rest)
      in

      let rec process orig_list = 
        let processed = combine_overlapping orig_list in

        (* If processing changed the list at all, check if there are any new overlaps *)
        if (List.length orig_list == List.length processed) then
          orig_list
        else
          process processed
        in

      process hm_list
    )
  ) in

  let needles = Base.Hashtbl.to_alist hm_tbl_combined in
  let needles_with_tup = Base.List.map needles ~f:(fun (needle, haystack_tbl) ->
    let haystacks = Base.Hashtbl.to_alist haystack_tbl in
    (needle, haystacks)
  ) in
  let flattened = Base.List.concat_map needles_with_tup ~f:(fun (needle, ls) -> 
    Base.List.concat_map ls ~f:(fun (haystack, locations_list) ->
      Base.List.map locations_list ~f:(fun locations ->
        { needle = 
            { path = needle; 
              start_index = locations.needle.start_index;
              end_index = locations.needle.end_index };
          haystack = 
            { path = haystack; 
              start_index = locations.haystack.start_index;
              end_index = locations.haystack.end_index }
        }
      )
    )
  ) in

  let open Yojson in
    let matches = `List 
      (Base.List.map flattened ~f:(fun (hm: hashMatch) -> 
        `Assoc [
          ("needle", `Assoc [
            ("path", `String hm.needle.path);
            ("start_index", `Int hm.needle.start_index);
            ("end_index", `Int hm.needle.end_index)
          ]);
          ("haystack", `Assoc [
            ("path", `String hm.haystack.path);
            ("start_index", `Int hm.haystack.start_index);
            ("end_index", `Int hm.haystack.end_index)
          ])
        ]
      )) in

    let file_object tb file_type = 
      let alist = Base.Hashtbl.to_alist tb in
      Base.List.map alist ~f:(fun (file, ihash_ls) -> 
        let total = List.length ihash_ls in
        (file, `Assoc [
          ("type", `String file_type);
          ("total", `Int total)]
      )) in

    let needle_files = file_object needles_tbl "needle" in
    let haystack_files = file_object haystack_tbl "haystack" in

    let output = `Assoc [
      ("config", `Assoc [
        ("paths", `Assoc [
          ("needles", `String info.needles_path);
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
              ~needles_tbl:needles_tbl 
              ~haystack_tbl:haystack_tbl 
              hm_tbl =
  (match json_filename with
   | None -> ()
   | Some json_filename -> hash_match_table_json_file ~json_filename:json_filename 
                                                      ~analysis_info:info
                                                      ~needles_tbl:needles_tbl 
                                                      ~haystack_tbl:haystack_tbl 
                                                      ~hm_tbl:hm_tbl);
  hash_match_table_print needles_tbl hm_tbl

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
  (needles: (string, Preprocessing.indexed_hash list) Base.Hashtbl.t) 
  (haystack: (string, Preprocessing.indexed_hash list) Base.Hashtbl.t) 
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
                 ~needles_tbl:shortened_needles 
                 ~haystack_tbl:shortened_haystack 