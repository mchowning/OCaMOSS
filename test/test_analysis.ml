open OUnit2
open OCaMossLib

let hashtbl_printer tbl = 
  Hashtbl.fold (fun key value acc ->
    acc ^ "\n  " ^ key (*^ ": " ^ value *)
  ) tbl "\n{" |> (fun s -> s ^ "\n}\n")

let hashMatch_printer (hm: Analysis.hashMatch) =
  "{ " ^ "needle_path: " ^ hm.needle_path ^ ", " 
       ^ "needle_hash: " ^ Int.to_string hm.needle_line ^ ", " 
       ^ "haystack_path: " ^ hm.haystack_path ^ ", "
       ^ "haystack_hash: " ^ Int.to_string hm.haystack_line ^
  " }"

let list_printer l_printer ls =
  let strings = List.map (fun x -> "    " ^ l_printer x) ls in
  let items = String.concat ",\n" strings in
  "[\n" ^ items ^ "\n]"


let tests = [
  "description" >:: (fun _ -> assert_equal 1 1);

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

  "matches needles" >:: (fun _ -> assert_equal ~printer:(list_printer hashMatch_printer) expected actual);
]