let parser guarantee_threshold min_threshold dir_or_file_name = 

  if (min_threshold > guarantee_threshold) 
    then failwith "noise_threshold must be greater than guarantee_threshold";
  let window_size = guarantee_threshold - min_threshold + 1 in

  let dir_contents d =
    (* FIXME skip hidden directories (starting with .) *)
    let rec loop result = function
      | f::fs when Sys.is_directory f ->
            Sys.readdir f
            |> Array.to_list
            |> List.map (Filename.concat f)
            |> List.append fs
            |> loop result
      | f::fs -> loop (f::result) fs
      | []    -> result
    in
      loop [] [d]
  in
  let files =  dir_contents dir_or_file_name in

  Printf.printf "Parsing %d files\n%!" (List.length files);

  (* let count = ref 0 in
  let start = ref true in
  let print_count s = begin
    count := !count + 1;
    let (_,y) = pos_cursor () in
    let newY = if !start then y else y - 1 in
    set_cursor 1 newY;
    erase Below;
    Printf.printf "%n\n%s%!" !count s;
    start := false;
  end in *)

  List.fold_left (fun acc file_path -> begin

    let hashed_file = Preprocessing.hash_file min_threshold file_path in

    match hashed_file with
    | None -> acc
    | Some hashes ->
      let winnowed_hashes = Winnowing.winnow window_size hashes in
      Hashtbl.add acc file_path winnowed_hashes;
      acc
  end) (Hashtbl.create (List.length files)) files

let libmain_func () =
  let usage_msg = "--needles <needle_dir> --haystack <haystack_dir> --guarnatee_threshold <number> --min_threshold <number>" in

  let needle_arg = ref "" in
  let haystack_arg = ref "" in
  let guarantee_threshold = ref 70 in
  let min_threshold = ref 30 in

  let speclist =
    [("--needles", Arg.Set_string needle_arg, "[filename or directory] Set base for comparison");
     ("--haystack", Arg.Set_string haystack_arg, "[filename or directory] Set destination to search for matches");
     ("--guarantee_threshold", Arg.Set_int guarantee_threshold, "[number] After removing noise, files with at least this many matching consecutive characters are guaranteed to be identified");
     ("--min_threshold", Arg.Set_int min_threshold, "[number] After removing noise, files without this many matching consecutive characters will NOT be identified")] in

  let anon_arg_fun arg =
    Printf.printf "Unexpected anonymous argument of: %s\n%!" arg;
  in

  Arg.parse speclist anon_arg_fun usage_msg;

  Printf.printf "needle_arg: %s\n%!" !needle_arg;
  Printf.printf "haystack_arg: %s\n%!" !haystack_arg;
  Printf.printf "guarantee_threshold arg: %n\n%!" !guarantee_threshold;
  Printf.printf "min_threshold arg: %n\n%!" !min_threshold;

  let parse_fun = parser !guarantee_threshold !min_threshold in 
  let needles = parse_fun !needle_arg in
  let haystack = parse_fun !haystack_arg in

  Analysis.analyze needles haystack;