open Unix
open Filename
open Yojson.Basic.Util

type comment_info = {
  single_comment: string;
  multi_comment_start: string;
  multi_comment_end: string;
  nest: bool;
  strings: bool;
}

type language_info = {
  keywords: string list;
  special_chars: char list;
  comment_info: comment_info;
}

type char_with_pos = (char * int)
type index_string = char_with_pos list


let index_string_to_string (istr: index_string) = Base.String.of_char_list (Base.List.map istr ~f:fst)

let get_language_info language_file = 
  (* Using this environment variable in tests because I need a different path there it seems *)
  let language_files_path = try
    Sys.getenv "LANGUAGE_FILES_PATH"
  with
   | Not_found -> "language_files/" in
  let path_to_language_file = language_files_path ^ language_file in
  let json = Yojson.Basic.from_file path_to_language_file in
  let keywords_list = json |> member "keywords"
                      |> to_list |> List.map to_string in
  let special_chars = json |> member "special characters"
                      |> to_list |> List.map to_string 
                      |> List.map (fun str -> String.get str 0) in   
  let one_line_comm_st = json |> member "comment" |> to_string in
  let mult_line_comm_st = json |> member "comment-start" |> to_string in
  let mult_line_comm_end = json |> member "comment-end" |> to_string in
  let comments_nest = json |> member "comments-nest" |> to_bool in
  let rm_string = json |> member "strings" |> to_bool in
  let comment_info = {
    single_comment = one_line_comm_st;
    multi_comment_start = mult_line_comm_st;
    multi_comment_end = mult_line_comm_end;
    nest = comments_nest;
    strings = rm_string 
  } in
  {
    keywords = keywords_list;
    special_chars = special_chars;
    comment_info = comment_info;
  }

let keywords l = l.keywords
let special_chars l = l.special_chars
let comment_info l = l.comment_info

(* [rem_white_space code_string] returns a string list representing the strings
 * in [code_string] separated by tabs, newlines, carriage returns, and spaces,
 * with empty strings filtered out of said list.
 *
 * example: [rem_white_space " Hello World " = ["Hello"; "World"]]
*)
let rem_white_space code_string =
  code_string |>
  String.split_on_char '\t' |> String.concat " " |>
  String.split_on_char '\n' |> String.concat " " |>
  String.split_on_char '\r' |> String.concat " " |>
  String.split_on_char ' ' |>
  List.filter (fun str -> str <> "")

let rem_white_space_tup (code_string: index_string) =
  Base.List.fold code_string ~init:[] ~f:(fun acc (c, i) ->
    let split = 
      c == ' ' ||
      c == '\t' ||
      c == '\n' ||
      c == '\r' in
    if split then
      acc @ [[(c,i)]]
    else 
      match acc with
      | [] -> [[(c,i)]]
      | (a :: rest) -> (a @ [(c,i)]) :: rest
  )

  (* code_string |>
  String.split_on_char '\t' |> String.concat " " |>
  String.split_on_char '\n' |> String.concat " " |>
  String.split_on_char '\r' |> String.concat " " |>
  String.split_on_char ' ' |>
  List.filter (fun str -> str <> "") *)

(* [split_and_keep_on_spec_chars spec_chars str] essentially does the same
 * thing as [String.split_on_char] for each character in [spec_char], except
 * each of the characters in [spec_char] are themselves an element in the
 * resulting list.
 *
 * example:
 * [split_and_keep_on_spec_chars [!;?] "Hello!World?" =
 * ["Hello"; "!"; "World"; "?"]]
*)

module C = Base.Char;;
let split_and_keep_on_spec_chars spec_chars str =
  String.fold_left (fun acc c -> begin
    let is_special_char c = List.exists (fun x -> x == c) spec_chars in
    let open C in
    if is_special_char c
    then (C.to_string c) :: acc
    else match acc with
         | [] -> [C.to_string c] 
         | (s :: rest) ->
              match Base.String.to_list s with
              | [] -> raise (Failure "list should not have empty strings")
              | [h] -> 
                  if is_special_char h
                  then C.to_string c :: acc
                  else (s ^ C.to_string c) :: rest
              | _ -> (s ^ C.to_string c) :: rest
  end ) [] str
  |> List.rev

let split_and_keep_on_spec_chars_tup spec_chars (tup: (char * int) list) =
  Base.List.fold tup ~init:[] ~f:(fun acc (c,i) -> begin
    let is_special_char c = List.exists (fun x -> x == c) spec_chars in
    let open C in
    if is_special_char c
    then [(c,i)] :: acc
    else match acc with
         (* | [] -> [C.to_string c] *)
         | [] -> [[(c,i)]] 
         | (s :: rest) ->
              match s with
              | [] -> raise (Failure "list should not have empty strings")
              | [(c',_)] -> 
                  if is_special_char c'
                  then [(c,i)] :: acc
                  else (s @ [(c,i)]) :: rest
              | _ -> (s @ [(c,i)]) :: rest
  end)
  |> List.rev

(* [split_on_str str_to_split_on acc_str_arr str_to_split] splits up
 * [str_to_split] on every instance of [str_to_split_on] when
 * [acc_str_arr] = []. [acc_str_arr] acts as an accumulator array that stores
 * the accumulating string array as splitting is being done.
 *
 * example:
 * [split_on_str "Hello" [] "HelloWorldHelloWorldHello" =
 * ["Hello"; "World"; "Hello"; "World"; "Hello"]
*)
let split_on_str str_to_split_on str_to_split =
  let re = Str.regexp (Str.quote str_to_split_on) in
  let split = Str.full_split re str_to_split in
  List.map (function 
    | Str.Text s -> s
    | Delim s -> s
  ) split


(* [remove_strings code_str], when treating [code_str] as a block of code,
 * removes all instances of strings in code_str.
 *
 * example: [remove_strings "let str = \"Hello\"" = "let str = "]
*)
let remove_strings code_str =
  let code_str = String.split_on_char '\'' code_str |> String.concat "\"" in
  let filter_from_arr (acc_arr, start) str =
    if str = "\"" && start = false then
      (" "::acc_arr, true)
    else if str = "\"" then
      (" "::acc_arr, false)
    else
    if start then (" s "::acc_arr, start)
    else (" "::str::acc_arr, false)
  in
  let split_on_strings_arr = split_on_str "\"" code_str in
  let acc_tup =
    List.fold_left filter_from_arr ([], false) split_on_strings_arr in
  match acc_tup with
  | (acc_arr, _) -> List.rev acc_arr

(* [remove_comments comment_start comment_end comments_nest code_str], where
 * - [comment_start] is a string that indicates the start of a comment.
 * - [comment_end] is a string that indicates the end of a comment.
 * - [comments_nest] is a bool that indicates whether or not comments nest in
 *   the language that [code_str] is written in.
 * - and [code_str] is the string representation of code whose comments are to
 *   be removed.
 * returns [code_str] but with all of its comments removed.
*)
let remove_comments
    comment_start comment_end comments_nest code_str =
  let do_filter_from_arr (acc_arr, nesting) str =
    if str = comment_start then
      if comments_nest then (" "::acc_arr, nesting + 1)
      else (" "::acc_arr, 1)
    else if str = comment_end then
      if comments_nest then (" "::acc_arr, nesting - 1)
      else (" "::acc_arr, 0)
    else if nesting > 0 then 
      (acc_arr, nesting)
    else (" "::str::acc_arr, 0)
  in
  let split_on_comments_arr =
    split_on_str comment_start code_str |>
    List.map (split_on_str comment_end) |> List.flatten
  in
  let acc_tup =
    List.fold_left do_filter_from_arr ([], 0) split_on_comments_arr in
  match acc_tup with
  | (acc_arr, _) -> List.rev acc_arr

(* [replace_generics keywords spec_chars str_arr] goes through the list, and
 * replaces any string that
 * - isn't a keyword
 * - isn't the string representation of a special character
 * - isn't the string representation of an int
 * with the letter "v". All relevant keywords and special characters are found
 * in [keywords] and [spec_chars], respectively.
 *
 * example: [
 *  replace_generics
 *    ["if"; "then"; "else"]
 *    ['['; ']']
 *    ["if"; "some_bool"; "then"; "["; "]"; "else"; "["; "one_element"; "]"] =
 *  ["if"; "v"; "then"; "["; "]"; "else"; "["; "v"; "]"]
 * ]
*)
let replace_generics keywords spec_chars str_arr =
  List.map
    (fun str ->
       if List.mem str keywords ||
          ((String.length str = 1) && (List.mem (String.get str 0) spec_chars))
       then str
       else
         try
           int_of_string str |> string_of_int
         with Failure _ -> "v" )
    str_arr

let replace_generics_tup (keywords: string list) spec_chars (istrs: index_string list) =
  let member (str: string) (istr: index_string) = 
    let (cs,_) = Base.List.unzip istr in
    let str' = Base.String.of_char_list cs in
    str == str' in

  let any xs = Base.List.exists xs ~f:(fun x -> x) in

  let any_member (strs: string list) (istr: index_string) =
    List.exists (fun (str: string) ->
      member str istr
    ) strs in

  let is_an_int (istr: index_string) = 
    let str = Base.String.of_char_list (Base.List.map istr ~f:fst) in
    Str.string_match (Str.regexp "[0-9]+") str 0
  in

  (* FIXME use fold *)
  Base.List.map istrs ~f:(fun istr ->
      let is_keyword = any_member keywords istr in
      let is_spec_char = match istr with
      | [(c,_)] -> any (Base.List.map spec_chars ~f:(fun sc -> sc = c))
      | _ -> false in
      
      if is_keyword || is_spec_char 
      then istr
      else if is_an_int istr
           then istr
           else 
            let index = match istr with
            | [] -> failwith "empty list item in replace_generics_tup--this should never happen"
            | ((_,i) :: _) -> i in
            [('v', index)])
      


       (* if member keywords istr ||
          ((String.length str = 1) && (List.mem (String.get str 0) spec_chars))
       then str
       else
         try
           int_of_string str |> string_of_int
         with Failure _ -> "v" )
    str_arr *)

(* let flatten_index_strings ls =
  Base.List.fold *)

(* Refer to preprocessing.mli for this function's specifications *)
let remove_noise comment_info code_string keywords spec_chars is_txt =
  if is_txt then code_string
  else
    let rm_one_line_comment s =
      if comment_info.single_comment = "" then s
      else
        remove_comments comment_info.single_comment "\n" false s
        |> String.concat ""
    in
    let rm_mult_line_comment s =
      if comment_info.multi_comment_start = "" then s
      else
        remove_comments comment_info.multi_comment_start comment_info.multi_comment_end comment_info.nest s
        |> String.concat ""
    in
    let rm_strings s =
      if comment_info.strings then remove_strings s |> String.concat ""
      else s
    in
    let print_and_pass message input = begin
      print_endline message;
      input
    end in
    code_string 
    (* |> print_and_pass "before rm_strings" *)
    |> rm_strings
    (* |> print_and_pass "before rm_mult_line_comment" *)
    |> rm_mult_line_comment 
    (* |> print_and_pass "before rm_one_line_comment" *)
    |> rm_one_line_comment
    (* |> print_and_pass "before split_and_keep_on_spec_chars" *)
    |> split_and_keep_on_spec_chars spec_chars 
    (* |> print_and_pass "before rem_white_space" *)
    |> List.map rem_white_space 
    (* |> print_and_pass "before flatten" *)
    |> List.flatten 
    (* |> print_and_pass "before replace_generics" *)
    |> replace_generics keywords spec_chars 
    (* |> print_and_pass "before concat" *)
    |> String.concat ""


let remove_noise_tup comment_info (code_string_tup: index_string) keywords spec_chars is_txt =
  if is_txt then code_string_tup
  else
    (* let rm_one_line_comment s =
      if comment_info.single_comment = "" then s
      else
        remove_comments comment_info.single_comment "\n" false s
        |> String.concat ""
    in *)
    (* let rm_mult_line_comment s =
      if comment_info.multi_comment_start = "" then s
      else
        remove_comments comment_info.multi_comment_start comment_info.multi_comment_end comment_info.nest s
        |> String.concat ""
    in *)
    (* let rm_strings s =
      if comment_info.strings then remove_strings s |> String.concat ""
      else s
    in *)
    let print_and_pass message input = begin
      print_endline message;
      input
    end in
    code_string_tup 
    (* |> rm_strings *)
    (* |> rm_mult_line_comment  *)
    (* |> rm_one_line_comment *)
    |> split_and_keep_on_spec_chars_tup spec_chars 
    |> List.map rem_white_space_tup
    |> List.flatten 
    |> replace_generics_tup keywords spec_chars 
    |> List.flatten
    (* |> String.concat "" *)

(* Refer to preprocessing.mli for this function's specifications *)
let rec k_grams s n =
  let s_length = String.length s in
  let rec helper s index acc =  
    if index < 0
    then acc
    else
      let gram = String.sub s index n in
      helper s (index-1) (gram::acc)
  in
  helper s (s_length - n) []

let rec k_grams_tup (is: index_string) n =
  let s_length = List.length is in

  let rec helper s index acc =  
    if index < 0
    then acc
    else
      let gram = Base.List.sub is ~pos:index ~len:n in
      (* let gram = String.sub s index n in *)
      helper s (index-1) (gram::acc)
  in

  helper is (s_length - n) []


(* [determine_language_file f] returns the string that represents the name of
 * of the json file that contains all relevant information about the language
 * that the code in the file named [f] is written in.
*)
let determine_language_file f =
  if check_suffix f "txt" then Some "txt_info.json"
  else if check_suffix f "ml" then Some "ocaml_info.json"
  else if check_suffix f "mli" then Some "ocaml_info.json"
  else if check_suffix f "c" then Some "c_info.json"
  else if check_suffix f "cpp" then Some "cpp_info.json"
  else if check_suffix f "java" then Some "java_info.json"
  else if check_suffix f "min.js" then None
  else if check_suffix f "js" then Some "javascript_info.json"
  else if check_suffix f "json" then Some "javascript_info.json"
  else if check_suffix f "ts" then Some "javascript_info.json" (* typescript keywords are included in the javascript file *)
  else if check_suffix f "php" then Some "php_info.json"
  else if check_suffix f "py" then Some "python_info.json"
  else None

let language_info_assoc_list = 
  [
  (* ".txt", get_language_info "txt_info.json"; *)
  ".ml", get_language_info "ocaml_info.json";
  ".mli", get_language_info "ocaml_info.json";
  ".c", get_language_info "c_info.json";
  ".cpp", get_language_info "cpp_info.json";
  ".java", get_language_info "java_info.json";
  ".js", get_language_info "javascript_info.json";
  ".ts", get_language_info "javascript_info.json"; (* typescript keywords are included in the javascript file *)
  ".php", get_language_info "php_info.json";
  ".py", get_language_info "python_info.json";
  ]

type language = { info: language_info; language_file: string; file: string }

let determine_language f = 
  Option.bind (List.assoc_opt (extension f) language_info_assoc_list)
    (fun info ->
      Option.bind (determine_language_file f)
        (fun language_file ->
          Some { info = info; language_file = language_file; file = f }
        )
      )

let get_ngrams f n = 

  match (determine_language f) with
  | None -> None
  | Some language ->
    let keywords = language.info.keywords in
    let spec_chars = language.info.special_chars in
    let f_channel = open_in f in
    let f_string = really_input_string f_channel (in_channel_length f_channel) in
    close_in f_channel;
    let is_txt = check_suffix f "txt" in
    let com_info = language.info.comment_info in

    let chars = Base.String.to_list f_string in
    let indices = Base.List.range 0 (List.length chars) in
    let char_ints = Base.List.zip_exn chars indices in
    let noise_removed_str =
      remove_noise_tup com_info char_ints keywords spec_chars is_txt in
    let result = k_grams_tup noise_removed_str n in
    Some result

type indexed_hash = {
  hash: int;
  start_index: int;
  end_index: int;
}

(* Refer to preprocessing.mli for this function's specifications *)
let hash_file min_threshold f = begin
  match (get_ngrams f min_threshold) with
  | None -> None
  | Some n_grams -> begin
      (* Some (Base.List.map ~f:(fun h -> begin
        let result = Hashtbl.hash h in
        result
      end) n_grams) *)
      Some (Base.List.map n_grams ~f:(fun istr -> begin
        let str = index_string_to_string istr in
        let hashed = Hashtbl.hash str in
        let (_,start) = Base.List.hd_exn istr in
        let (_,last) = Base.List.last_exn istr in
        { hash = hashed; 
          start_index = start; 
          end_index = last }
      end))
    end
  end
