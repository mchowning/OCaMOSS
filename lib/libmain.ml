open Preprocessing
open Comparison
open Dictionary
open ANSITerminal
(* let libmain_func () = print_endline "libmain_func" *)

type color = RED | GREEN | CYAN | TEXT

type state = {display: (color * string) list; directory: string; results:
                CompDict.t option; result_files: (color * string) list;
              threshold: float}

type cmd = RUN of string | DIR | HELP | SETDIR of string | RESULTS of string
         | PAIR | COMPARE of (string*string)| ERROR

let help =
  "Commands (case-sensitive): \n
run [threshold] --- runs oCaMoss on the working directory.
The threshold argument gives the program the percentage of the file to match
with another for it to be flagged as plagiarised. It is optional with default
value 0.5, and must be at least 0.4 and at most 1
dir --- lists the working directory and the files that it contains
setdir [dir] --- sets the directory to look for files and resets any results
results --- lists the file names for which there are results
results [filename] --- lists the detailed results of overlap for that file
resultpairs -- lists all the pairs of files for which there are results
compare [fileA] [fileB] --- prints out specific overlaps of files A and B
quit --- exits the REPL
help --- display instructions again"

let newstate = {display = [(GREEN,
                            "
      _______   _______             __   __   _______   _______   _______
     |       | |       |           |  |_|  | |       | |       | |       |
     |   _   | |     __|  _______  |       | |   _   | |  _____| |  _____|
     |  | |  | |    |    |   _   | |       | |  | |  | | |_____  | |_____
     |  |_|  | |    |__  |  |_|  | |       | |  |_|  | |_____  | |_____  |
     |       | |       | |   _   | | ||_|| | |       |  _____| |  _____| |
     |_______| |_______| |__| |__| |_|   |_| |_______| |_______| |_______|
");
                           (TEXT,"Welcome to OCaMOSS!!");(CYAN,help)];
                directory = "./" ; results = None; result_files = [];
                threshold = 0.5}
let parse str =
  let input_split = String.split_on_char ' ' str in
  match input_split with
  | ["help"] -> HELP
  | "run"::[t] -> RUN t
  | ["run"] -> RUN "0.5"
  | ["dir"] -> DIR
  | "setdir"::d::[] -> SETDIR d
  | "results"::f::[] -> RESULTS f
  | ["results"] -> RESULTS ""
  | ["resultpairs"] -> PAIR
  | "compare"::a::b::[] -> COMPARE (a,b)
  | _ -> ERROR

let rec repl st =
  let rec print_display d =
    match d with
    |[] -> ()
    |(RED, s)::t -> print_string [red] (s^"\n"); print_display t
    |(TEXT, s)::t -> print_string [Reset] (s^"\n"); print_display t
    |(GREEN, s)::t -> print_string [green] (s^"\n"); print_display t
    |(CYAN, s)::t -> print_string [cyan] (s^"\n"); print_display t
  in
  print_display st.display;
  print_string [Reset] "> ";
  match String.trim (read_line ()) with
  | exception End_of_file -> ()
  | "quit" -> print_string [green]
                ("Thank you for using OCaMOSS!!\n");
  | input -> handle_input st input

and handle_input st input =
  let rec print_dir_files dir str suf =
    try
      let f_name = Unix.readdir dir in
      if String.get f_name 0 = '.' || not (String.contains f_name '.')
      then print_dir_files dir str suf else
      if suf != "" && not (Filename.check_suffix f_name suf)
      then failwith "Different file extensions"
      else
        print_dir_files dir (str^f_name^"\n") (Filename.extension f_name)
    with
    | Failure f -> f
    | End_of_file -> str
  in
  match parse input with
  |HELP -> repl {st with display = [(CYAN,help)]}
  |RUN t -> begin
      try begin
        if st.directory = "./"
        then repl {st with display =
                             [(RED,"Error: Directory has not been set")]}
        else
          let t' = float_of_string t in
          if t' >= 0.4 && t' <= 1.0
          then handle_run st t'
          else
            repl {st with display =
                            [(RED, "Error: Threshold must between 0.4 and 1")]}
      end
      with
      | Failure f_msg when f_msg = "float_of_string" ->
        repl {st with display = [(RED,"Error: Invalid argument(s)")]}
      | Failure f_msg -> repl {st with display = [(RED,f_msg)]}
      | e ->
        print_endline (Printexc.to_string e);
        repl {st with display = [(RED,"Error: Something went wrong")]}
    end
  |DIR ->
    if st.directory = "./"
    then repl {st with display =
                         [(RED,"Error: Directory has not been set")]}
    else let dir_files = print_dir_files (Unix.opendir st.directory) "" "" in
      repl {st with display = [(TEXT, "Current working directory: " ^
                                      st.directory^"\n Files: \n"^ dir_files)]}
  |SETDIR d -> begin
      try
        if d = "" || not (Sys.is_directory d)
        then repl {st with display = [(RED,"Error: Invalid directory")]}
        else let dir_files = print_dir_files (Unix.opendir d) "" "" in
          if dir_files = ""
          then repl {st with display = [(RED,"Error: Directory has no files")]}
          else if dir_files = "Different file extensions"
          then repl {st with display =
                               [(RED,"Error: Not all files in this directory are of the same type")]}
          else repl {newstate with directory = d ; display = [(GREEN,
                                                               "Successfully set working directory to: " ^ d);
                                                              (TEXT,"Files: \n" ^ dir_files)]}

      with _ -> repl {st with display = [(RED,"Error: Invalid directory")]}
    end
  |RESULTS f -> begin
      match st.results with
      |Some _ -> begin
          if f = "" then if st.result_files = [] then repl {st with display =
                                                                      [(GREEN,"Success. There were no plagarised files found.\n")];}
            else repl {st with display =
                                 (TEXT, "Results for files:")::st.result_files}
          else handle_results st f
        end
      |None -> repl {st with display =
                               [(RED,"Error: no results to display. Run OCaMoss first")]}
    end
  | PAIR -> begin
      match st.results with
      | Some r -> if st.result_files = []
        then repl {st with display =
                             [(GREEN,"Success. There were no plagarised files found.\n")];}
        else handle_pair r st
      | None -> repl {st with display =
                                [(RED,"Error: no results to display. Run OCaMoss first")]}
    end
  |COMPARE (a,b) -> begin
      match st.results with
      |Some r -> handle_compare st a b r
      |None -> repl {st with display =
                               [(RED,"Error: no results to display. Run OCaMoss first")]}
    end
  |ERROR -> repl {st with display = [(RED,"Error: invalid command")]}

and handle_compare st a b results =
  let rec pad l n =
    if List.length l >= n then l else pad (l@[("","")]) n
  in
  match (CompDict.find a results, CompDict.find b results) with
  | (Some v1, Some v2) -> begin
      match (FileDict.find b v1, FileDict.find a v2) with
      | (Some r1, Some r2) -> begin
          let l1 = List.map (snd) r1 |> List.rev in
          let l2 = List.map (snd) r2 |> List.rev in
          let res1 = Preprocessing.get_file_positions
              (Unix.opendir st.directory) st.directory a l2 in
          let res2 = Preprocessing.get_file_positions
              (Unix.opendir st.directory) st.directory b l1 in
          print_endline "generating display...";
          let padded1 = pad res1 (List.length res2) in
          let padded2 = pad res2 (List.length res1) in
          let newdispl = List.fold_left2 (fun acc r1 r2 ->
              if String.length (snd r1) >= 40 then
                (TEXT, Printf.sprintf "%-40s%s" (a ^ " position " ^ fst r1)
                   (b ^ " position " ^ fst r2))::
                (RED, Printf.sprintf "%-40s%s"  (snd r1 ^ "\n") (
                    snd r2 ^ "\n"))::acc
              else
                (TEXT, Printf.sprintf "%-40s%s" (a ^ " position " ^ fst r1)
                   (b ^ " position " ^ fst r2))::
                (RED, Printf.sprintf "%-40s%s"  (snd r1) (snd r2 ^ "\n"))::acc
            ) [] padded1 padded2 in
          repl {st with display = newdispl}
        end
      |_,_ -> failwith "unexpected"
    end
  |_,_ -> repl {st with display = [(RED,
                                    "Error: no results to display for files " ^ a ^","^ b)]}


and handle_results st f =
  let cmp_tuple (k1,s1) (k2,s2) =
    if Stdlib.compare s1 s2 = 0 then -(Stdlib.compare k1 k2)
    else (Stdlib.compare s1 s2)
  in
  let concat_result_list lst is_pair =
    List.fold_left (fun a (f,ss) ->
        (TEXT, Printf.sprintf "%-40s%s" ("File: " ^ f)
           ((if is_pair then "Similarity score: " else "Overall score: ") ^
            (string_of_float ss)))::a) [] (List.sort (cmp_tuple) lst)
  in
  match st.results with
  |None -> failwith "unexpected"
  |Some r -> begin
      match CompDict.find f r with
      |Some v -> begin
          let r_list = Comparison.create_pair_sim_list f (FileDict.to_list v) in
          repl {st with display = (TEXT, "Results for file " ^ f ^
                                         ": \n")::(concat_result_list r_list true)}
        end
      |None -> repl {st with display = [(RED,
                                         "Error: no results to display for file " ^ f)]}
    end

and handle_run st t =
  let rec parse_dir dir dict dir_name =
    try
      let f_name = Unix.readdir dir in
      if String.get f_name 0 = '.' || not (String.contains f_name '.')
      then parse_dir dir dict dir_name else begin
        let full_path = dir_name ^ Filename.dir_sep ^ f_name in
        match (Preprocessing.hash_file full_path) with
        | None -> parse_dir dir dict dir_name
        | Some hashes -> 
          let winnowed_hashes = Winnowing.winnow 40 hashes in
          let new_dict = Comparison.FileDict.insert f_name winnowed_hashes dict in
          parse_dir dir new_dict dir_name
        (* let hashes = Preprocessing.hash_file full_path in
        let winnowed_hashes = Winnowing.winnow 40 hashes in
        let new_dict = Comparison.FileDict.insert f_name winnowed_hashes dict in
        parse_dir dir new_dict dir_name *)
      end
    with
    | End_of_file -> dict
  in
  let cmp_tuple (k1,s1) (k2,s2) =
    if Stdlib.compare s1 s2 = 0 then -(Stdlib.compare k1 k2)
    else (Stdlib.compare s1 s2)
  in
  let concat_result_list lst is_pair =
    List.fold_left (fun a (f,ss) ->
        (TEXT, Printf.sprintf "%-40s%s" ("File: " ^ f)
           ((if is_pair then "Similarity score: " else "Overall score: ") ^
            (string_of_float ss)))::a) []
      (lst |> List.sort (cmp_tuple)|> List.filter (fun (_,s) -> s >= t))
  in
  (*let tm = Sys.time () in*)
  print_endline "parsing files...";
  let parsefiles = parse_dir (Unix.opendir st.directory)
      Comparison.FileDict.empty st.directory in
  print_endline "generating results...";
  let comparison = Comparison.compare parsefiles in
  let files = concat_result_list
      (Comparison.create_sim_list comparison t) false in
  (*Printf.printf "Execution time: %fs\n" (Sys.time () -. tm);*)
  if files = [] then repl {st with display =
                                     [(GREEN,"Success. There were no plagarised files found.\n")];
                                   results = Some comparison; threshold = t}
  else repl {st with display =
                       (GREEN,"Success. The list of plagiarised files are:")::files;
                     results = Some comparison; result_files = files; threshold = t}

and handle_pair r st =
  let disp = List.fold_left (fun d (f,_) -> d ^ (match CompDict.find f r with
      | None -> ""
      | Some f_d -> List.fold_left (fun s (f2,ss) -> s ^
                                                     if ss < st.threshold && f != f2 then "" else Printf.sprintf "%-40s%s" (f)
                                                         (f2) ^ "\n")
                      "" (create_pair_sim_list f (FileDict.to_list f_d))))
      "" (CompDict.to_list r) in
  repl {st with display = [(TEXT,disp)]}

(* ----------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------- *)


type state_dirs = {
  display: (color * string) list; 
  (* directory: string;  *)
  input_dir: string;
  compare_to_dir: string;
  results: CompDict.t option; 
  result_files: (color * string) list;
  threshold: float
  }

let count = ref 0

let parse_dir dir_name = 
  (* let rec parse_dir' dir dict dir_name =
    try
      let f_name = Unix.readdir dir in
      if String.get f_name 0 = '.'
      then begin
        (* Printf.printf "skipping %s\n%!" f_name; *)
        parse_dir' dir dict dir_name 
      end
      else
        let full_path = dir_name ^ Filename.dir_sep ^ f_name in
        (* Printf.printf "Parsing %s\n%!" full_path; *)
        let kind = (Unix.stat full_path).st_kind in
        if kind == S_DIR
        then begin
          (* Printf.printf "entering directory %s\n%!" full_path; *)


          let new_dict = parse_dir' (Unix.opendir full_path) dict full_path in
          parse_dir' dir new_dict dir_name
        end
        else begin
          let full_path = dir_name ^ Filename.dir_sep ^ f_name in
          Printf.printf "%s\n%!" full_path;
          (* Printf.printf "entering file\n%!"; *)

          (* REMOVE ME AND UNCOMMENT BELOW *)
          (* parse_dir' dir dict dir_name *)

          count := !count + 1;

          (* match (Preprocessing.hash_file full_path) with
          | None -> parse_dir' dir dict dir_name
          | Some hashes -> 
              let winnowed_hashes = Winnowing.winnow 40 hashes in
              let new_dict = Comparison.FileDict.insert full_path winnowed_hashes dict in
              parse_dir' dir new_dict dir_name *)

          (* let hashed_file = Preprocessing.hash_file full_path in
          let new_dict = match hashed_file with
          | None -> dict
          | Some hashes -> 
              let winnowed_hashes = Winnowing.winnow 40 hashes in
              Comparison.FileDict.insert full_path winnowed_hashes dict
          in 
          parse_dir' dir new_dict dir_name *)
          parse_dir' dir dict dir_name

        end
    with
    | End_of_file -> 
        Unix.closedir dir;
        dict
    in
  parse_dir' (Unix.opendir dir_name) Comparison.FileDict.empty dir_name *)

  (* let rec parse_dir' dir dict dir_name =
    match Unix.readdir dir with
    | f_name ->
      if String.get f_name 0 = '.'
      then begin
        (* Printf.printf "skipping %s\n%!" f_name; *)
        parse_dir' dir dict dir_name 
      end
      else
        let full_path = dir_name ^ Filename.dir_sep ^ f_name in
        (* Printf.printf "Parsing %s\n%!" full_path; *)
        let kind = (Unix.stat full_path).st_kind in
        if kind == S_DIR
        then begin
          (* Printf.printf "entering directory %s\n%!" full_path; *)


          let new_dict = parse_dir' (Unix.opendir full_path) dict full_path in
          parse_dir' dir new_dict dir_name
        end
        else begin
          let full_path = dir_name ^ Filename.dir_sep ^ f_name in
          Printf.printf "%s\n%!" full_path;
          (* Printf.printf "entering file\n%!"; *)

          (* REMOVE ME AND UNCOMMENT BELOW *)
          (* parse_dir' dir dict dir_name *)

          count := !count + 1;

          (* match (Preprocessing.hash_file full_path) with
          | None -> parse_dir' dir dict dir_name
          | Some hashes -> 
              let winnowed_hashes = Winnowing.winnow 40 hashes in
              let new_dict = Comparison.FileDict.insert full_path winnowed_hashes dict in
              parse_dir' dir new_dict dir_name *)

          (* let hashed_file = Preprocessing.hash_file full_path in
          print_endline "hashed file";
          let new_dict = match hashed_file with
          | None -> dict
          | Some hashes -> 
              let winnowed_hashes = Winnowing.winnow 40 hashes in
              print_endline "winnowed";
              Comparison.FileDict.insert full_path winnowed_hashes dict
          in 
          print_endline "recursive call";
          parse_dir' dir new_dict dir_name *)
          parse_dir' dir dict dir_name
        end
    | exception End_of_file -> 
        Unix.closedir dir;
        dict
    in *)


  let dir_contents dir =
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
      loop [] [dir]
  in
  let files =  dir_contents dir_name in



  List.fold_left (fun acc file_path -> begin

    let file_path = "/Users/matt/code/a8c/WordPress/src/js/_enqueues/vendor/tinymce/tinymce.js" in
    print_endline file_path;

    let hashed_file = hash_file file_path in

    print_endline "hashed";

    match hashed_file with
    | None -> acc
    | Some hashes ->
      let winnowed_hashes = Winnowing.winnow 40 hashes in

      print_endline "winnowed";
      (* print_endline "winnowed";
      acc *)

      Comparison.FileDict.insert file_path winnowed_hashes acc;
  (* end) Comparison.FileDict.empty files *)
  end) Comparison.FileDict.empty [List.hd files]



    (* let file_path = "/Users/matt/code/a8c/WordPress/src/js/_enqueues/vendor/tinymce/tinymce.js" in
    let hashed_file = hash_file file_path in
    Comparison.FileDict.empty *)


  (* let rec collect_files dir dir_name acc =
    count := !count + 1;
    match Unix.readdir dir with
    | f_name ->
      if String.get f_name 0 = '.'
      then begin
        (* Printf.printf "skipping %s\n%!" f_name; *)
        collect_files dir dir_name acc
      end
      else
        let full_path = dir_name ^ Filename.dir_sep ^ f_name in
        (* Printf.printf "Parsing %s\n%!" full_path; *)
        let kind = (Unix.stat full_path).st_kind in
        if kind == S_DIR
        then begin
          (* Printf.printf "entering directory %s\n%!" full_path; *)

          let sub_dir_files = collect_files (Unix.opendir full_path) full_path acc in
          let new_acc = acc @ sub_dir_files in
          collect_files dir dir_name new_acc
        end
        else begin
          Printf.printf "%s\n%!" full_path;
          collect_files dir dir_name (full_path :: acc)
        end
    | exception End_of_file -> 
        Unix.closedir dir;
        acc

  in

  let all_files = collect_files (Unix.opendir dir_name) dir_name [] in
  all_files *)

  (* parse_dir' (Unix.opendir dir_name) Comparison.FileDict.empty dir_name *)

let libmain_func () =
  let usage_msg = "-needles <needle_dir> -haystack <haystack_dir>" in
  let needle_dir = ref "" in
  let haystack_dir = ref "" in
  let speclist =
    [("-needles", Arg.Set_string needle_dir, "Set base directory name");
     ("-haystack", Arg.Set_string haystack_dir, "Set compare to directory name")] in
  (* let anon_arg_fun arg = Printf.printf "%s\n" ("Anonymous argument: " ^ arg) in *)
  let anon_arg_fun arg = begin
    print_endline "Running anonymous argument";
    let hashes = hash_file arg in
    match hashes with
    | Some h -> Printf.printf "num hashes: %n\n%!" (List.length h);
    (); 
  end in
  Arg.parse speclist anon_arg_fun usage_msg;

  try
    let needle_files = parse_dir !needle_dir in

    Printf.printf "Count: %i\n" !count;
  with
  | e -> 
      Printf.printf "Count: %i\n" !count;
      Printf.printf "Error: %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      Printf.printf "Backtrace: %s\n" (Printexc.get_backtrace ());