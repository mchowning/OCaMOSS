open OUnit2
open OCaMossLib


let list_printer l_printer ls =
  let strings = List.map (fun x -> "    " ^ l_printer x) ls in
  let items = String.concat ",\n" strings in
  "[\n" ^ items ^ "\n]"

let hashtbl_val_int_to_str tbl = Hashtbl.fold (fun a b acc -> begin
  Hashtbl.add acc a (list_printer Int.to_string b);
  acc
end) tbl (Hashtbl.create 10)

let hashtbl_printer tbl = 
  Hashtbl.fold (fun key value acc ->
    acc ^ "\n  " ^ key ^ ": " ^ value
  ) tbl "\n{" |> (fun s -> s ^ "\n}\n")

let hashMatch_printer (hm: Analysis.hashMatch) =
  "{ " ^ "needle_path: " ^ hm.needle_path ^ ", " 
       ^ "needle_hash: " ^ Int.to_string hm.needle_line ^ ", " 
       ^ "haystack_path: " ^ hm.haystack_path ^ ", "
       ^ "haystack_hash: " ^ Int.to_string hm.haystack_line ^
  " }"

let matches_needles _ =

  let input_needles: (string, Winnowing.fingerprint list) Hashtbl.t = Hashtbl.create 5 in
  Hashtbl.add input_needles "a" [ 
    { hash = 1; location = 1 }; 
    { hash = 11; location = 2 } 
  ];
  Hashtbl.add input_needles "b" [ 
    { hash = 10; location = 1 }; 
    { hash = 11; location = 2 } 
  ];
  Hashtbl.add input_needles "c" [ 
    { hash = 1; location = 1 }; 
    { hash = 2; location = 2 }; 
    { hash = 3; location = 3 } 
  ];

  let input_haystack: (string, Winnowing.fingerprint list) Hashtbl.t = Hashtbl.create 5 in
  Hashtbl.add input_haystack "aa" [ 
    { hash = 1; location = 10 }; 
    { hash = 3; location = 30 }; 
    { hash = 5; location = 50 }; 
    { hash = 7; location = 70 }; 
    { hash = 9; location = 90 }
  ];

  Hashtbl.add input_haystack "bb" [ 
    { hash = 2; location = 20 };
    { hash = 4; location = 40 };
    { hash = 6; location = 60 };
    { hash = 8; location = 80 }
  ];

  let actual = Analysis.find_matches input_needles input_haystack in

  let expected: Analysis.hashMatch list = [
    { needle_path = "a"; 
      needle_line = 1; 
      haystack_path = "aa"; 
      haystack_line = 10 };

    { needle_path = "c"; 
      needle_line = 1; 
      haystack_path = "aa"; 
      haystack_line = 10 };
    { needle_path = "c"; 
      needle_line = 2; 
      haystack_path = "bb"; 
      haystack_line = 20 };
    { needle_path = "c"; 
      needle_line = 3; 
      haystack_path = "aa"; 
      haystack_line = 30 };
  ] in

  assert_equal ~printer:(list_printer hashMatch_printer) expected actual




let results_by_file _ =

  let input: Analysis.hashLocation list = [
    { path = "a"; 
      line = 1; };
    { path = "a"; 
      line = 2; };
    { path = "a"; 
      line = 3; };
    { path = "b"; 
      line = 11; };
    { path = "c"; 
      line = 20; };
    { path = "b"; 
      line = 12; };
    { path = "b"; 
      line = 13; }
  ] in
  let actual = Analysis.by_file input in

  let expected = 
    let tbl = Hashtbl.create 10 in begin
      Hashtbl.add tbl "a" [1;2;3];
      Hashtbl.add tbl "b" [11;12;13];
      Hashtbl.add tbl "c" [20];

      tbl
    end in

  assert_equal ~printer:(fun tbl -> hashtbl_printer (hashtbl_val_int_to_str tbl)) expected actual
 

let tests = [
  "matches needles" >:: matches_needles;
  "results by file" >:: results_by_file;
]