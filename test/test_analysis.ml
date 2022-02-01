open OUnit2
open OCaMossLib

let location_to_string (l: Analysis.locations) = 
  "{ " ^ Int.to_string l.needle_index ^ "; " ^ Int.to_string l.haystack_index ^ " }"
let list_printer l_printer ls =
  let strings = List.map l_printer ls in
  let items = String.concat ";\n" strings in
  "[\n" ^ items ^ "\n]"

let hashtbl_val_to_str to_string tbl = Hashtbl.fold (fun a b acc -> begin
  Hashtbl.add acc a (to_string b);
  acc
end) tbl (Hashtbl.create 10)

let hashtbl_printer tbl = 
  Hashtbl.fold (fun key value acc ->
    acc ^ "\n" ^ key ^ ": " ^ value
  ) tbl "\n{" |> (fun s -> s ^ "\n}\n")

let base_hashtbl_printer tbl = 
  Base.Hashtbl.to_alist tbl |> Base.List.map ~f:(fun (k, v) ->
    k ^ ": " ^ v
  ) |> String.concat "\n"



let hashMatch_printer (hm: Analysis.hashMatch) =
  "{ " ^ "needle_path: " ^ hm.needle.path ^ ", " 
       ^ "needle_hash: " ^ Int.to_string hm.needle.index ^ ", " 
       ^ "haystack_path: " ^ hm.haystack.path ^ ", "
       ^ "haystack_hash: " ^ Int.to_string hm.haystack.index ^
  " }"

let matches_needles _ =

  let input_needles: (string, Winnowing.fingerprint list) Base.Hashtbl.t = Base.Hashtbl.create (module Base.String) in
  Base.Hashtbl.add_exn input_needles ~key:"a" ~data:[ 
    { hash = 1; location = 1 }; 
    { hash = 11; location = 2 } 
  ];
  Base.Hashtbl.add_exn input_needles ~key:"b" ~data:[ 
    { hash = 10; location = 1 }; 
    { hash = 11; location = 2 } 
  ];
  Base.Hashtbl.add_exn input_needles ~key:"c" ~data:[ 
    { hash = 1; location = 1 }; 
    { hash = 2; location = 2 }; 
    { hash = 3; location = 3 } 
  ];

  let input_haystack: (string, Winnowing.fingerprint list) Base.Hashtbl.t = Base.Hashtbl.create (module Base.String) in
  Base.Hashtbl.add_exn input_haystack ~key:"aa" ~data:[ 
    { hash = 1; location = 10 }; 
    { hash = 3; location = 30 }; 
    { hash = 5; location = 50 }; 
    { hash = 7; location = 70 }; 
    { hash = 9; location = 90 }
  ];

  Base.Hashtbl.add_exn input_haystack ~key:"bb" ~data:[ 
    { hash = 2; location = 20 };
    { hash = 4; location = 40 };
    { hash = 6; location = 60 };
    { hash = 8; location = 80 }
  ];

  let actual = Analysis.find_matches input_needles input_haystack in

  let expected: Analysis.hashMatch list = [
    { needle = { path = "a"; index = 1; };
      haystack = { path = "aa"; index = 10 };};
    { needle = { path = "c"; index = 1; };
      haystack = { path = "aa"; index = 10 };};
    { needle = { path = "c"; index = 2; };
      haystack = { path = "bb"; index = 20 };};
    { needle = { path = "c"; index = 3; };
      haystack = { path = "aa"; index = 30 };};
  ] in

  assert_equal ~printer:(list_printer hashMatch_printer) expected actual

let hash_matches_to_table _ =

  let (input: Analysis.hashMatch list) = [
    { needle = { path = "a"; index = 1; };
      haystack = { path = "aa"; index = 10 };};
    { needle = { path = "c"; index = 1; };
      haystack = { path = "aa"; index = 10 };};
    { needle = { path = "c"; index = 2; };
      haystack = { path = "bb"; index = 20 };};
    { needle = { path = "c"; index = 1; };
      haystack = { path = "bb"; index = 30 };};
    { needle = { path = "c"; index = 3; };
      haystack = { path = "aa"; index = 40 };};
  ] in
  let actual = Analysis.hash_matches_to_table input in

  let expected =
    let expected_tups = [
      ("a", [
        ("aa", (1, 10));
      ]);
      ("c", [
        ("aa", (3, 40));
        ("aa", (1, 10));
        ("bb", (1, 30));
        ("bb", (2, 20));
      ])
    ] in
    let first_level = Base.Hashtbl.of_alist_exn (module Base.String) expected_tups in
    Base.Hashtbl.map first_level ~f:(fun ls ->
      let inner = Base.Hashtbl.of_alist_multi (module Base.String) ls in
      Base.Hashtbl.map inner ~f:(fun inners ->
        Base.List.map inners ~f:(fun (needle_index, haystack_index) -> ({ needle_index; haystack_index }: Analysis.locations))
    ))
  in

  let inner_tbl_printer tbl = base_hashtbl_printer (Base.Hashtbl.map tbl ~f:(list_printer location_to_string)) in
  let printer tbl = base_hashtbl_printer (Base.Hashtbl.map tbl ~f:inner_tbl_printer) in
  let equality tbl1 tbl2 = Base.Hashtbl.equal 
    (fun sub_tbl1 sub_tbl2 ->
      Base.Hashtbl.equal 
        (fun locs1 locs2 -> 
          Base.List.equal (fun (loc1: Analysis.locations) loc2 ->
            loc1.needle_index = loc2.needle_index && loc1.haystack_index = loc2.haystack_index
          ) locs1 locs2
        ) sub_tbl1 sub_tbl2
    ) tbl1 tbl2 
    in
  assert_equal ~cmp:(equality) ~printer:printer expected actual

let tests = [
  "matches needles" >:: matches_needles;
  "haystack matches to table" >:: hash_matches_to_table;
]