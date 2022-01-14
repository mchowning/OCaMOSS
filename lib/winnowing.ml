module type BoundedQueueWithCounter = sig
  type 'a t
  val empty : int -> 'a t
  val create : int -> 'a -> 'a t
  val is_empty : 'a t -> bool
  val is_full : 'a t -> bool
  val size : 'a t -> int
  val enqueue : 'a -> 'a t -> 'a t
  val dequeue : 'a t -> 'a option * 'a t
  val count : 'a t -> int
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val to_list: 'a t -> 'a list
end

module Window : BoundedQueueWithCounter = struct

  type 'a t = { data: ('a list) * ('a list) ; maxsize: int ; size: int ; count: int}

  let empty n =
    if n = 0 then failwith "Cannot create queue of size 0!"
    else
      { data = ([],[]); maxsize = n; size = n; count = 0}

  let create n i =
    let rec gen l acc i =
      if l = 0 then acc else gen (l - 1) (i::acc) i
    in
    if n = 0 then failwith "Cannot create queue of size 0!"
    else
      let initdata = gen n [] i in
      { data = (initdata,[]); maxsize = n; size = n; count = 0}

  let is_empty q = (q.size = 0)

  let is_full q = (q.size = q.maxsize)

  let size q = q.size

  let rec dequeue q =
    match q.data with
    |h::t, b -> (Some h, {q with data = (t,b) ; size = q.size - 1})
    |[],[] -> (None, q)
    |[], h::t -> dequeue {q with data = (List.rev (h::t),[])}

  let rec enqueue item q =
    if is_full q then dequeue q |> snd |> enqueue item
    else
      match q.data with
      |f,b -> {q with data = (f,item::b); size = q.size + 1 ;count = q.count + 1}

  let count q = q.count

  let to_list q = (fst q.data)@(snd q.data |> List.rev)

  let fold f init q =
    List.fold_left f init (to_list q)


end


(* module TupSet = Set.Make(Int);; *)
module TupSet = Set.Make(
  struct
    let compare (a,_) (b,_) = compare a b
    type t = int * int
  end
)


let winnow_alt size hs =
  (* let hs_length = List.length hs in
  let rec helper acc idx = 
    if idx < 0
    then acc
    else
      let window = Base.List.sub hs ~pos:idx ~len:size in
      let min_tup = Base.List.foldi window ~init:None ~f:(fun i acc h ->
        match acc with
          | None -> Some (h,i)
          | Some (mh,mi) -> 
              if h < mh 
              then Some (h,i) 
              else Some (mh,mi)
      ) in
      match min_tup with
      | None -> acc
      | Some t -> 
          let new_set = TupSet.add t acc in
          helper new_set (idx - 1)
  in
  TupSet.elements (helper TupSet.empty (hs_length - size)) *)

  let hs_length = List.length hs in

  (* let rec min_from_window (a_n,a_i) (l::ls) =
    let (b_n,b_i) = List.fold_left (fun (a,b) (x,y) ->
      if x < a then (a,b) else (x,y)
    ) l ls in
    (* No minimum if same as previously selected minimum *)
    if a_n == b_n then None else Some (b_n,b_i) in *)


  let hs_indexed = List.combine hs (Base.List.range 0 hs_length) in
  let mins =

    (* let first_window = Base.List.sub hs ~pos:0 ~len:size in

    let first_min = List.fold_right (fun (h,i) min_option -> 
      match min_option with
      | None -> Some (h,i) 
      | Some (mh,mi) -> 
          if h < mh
          then Some (h,i)
          else Some (mh,mi)
    ) (List.combine first_window (Base.List.range 0 size)) None in *)

    let min_from_window window = begin
      print_endline "window";
      List.iter (fun (h,i) -> Printf.printf "(%i,%n)\n%!" h i) window;

      List.fold_right (fun (h,i) min_option -> 
        match min_option with
        | None -> Some (h,i) 
        | Some (mh,mi) -> 
            if h < mh
            then Some (h,i)
            else Some (mh,mi)
      ) window None |> Option.get 
    end in

    let (winnowed,_) = List.fold_left (fun (mins, last) (h,idx) -> 
      if idx + size > hs_length
      then begin
        Printf.printf "base case\n";
        (mins, last)
      end
      else
        let window = Base.List.sub hs_indexed ~pos:idx ~len:size in
        let new_min = min_from_window window in
        Printf.printf "new window min of %i\n" (fst new_min);
        match mins with
        | [] -> begin
                  ([new_min], Some new_min)
                end
        |_ -> let (l_h, l_idx) = Option.get last in
              begin
              Printf.printf "idx: %n, l_idx: %n, size: %n\n" idx l_idx size;
              (* if idx - l_idx < size *)
              if idx <= l_idx
              

              (* Only have to do a single comparison *)
              (* then if h < l_h  *)
              then begin
                   Printf.printf "single comparison, h: %i, l_h: %i\n" h l_h;
                   if h < l_h 
                    then let new_min = (h,idx) in
                         (new_min :: mins, Some new_min)
                    else (mins, last)
                   end

              (* Previous min is no longer in window.
                 Need to compare all elements in window *)
              else 
                   
                   begin
                   Printf.printf "new_min: %i, l_h: %i\n%!" (fst new_min) l_h;

                    if (fst new_min) == l_h
                    (* No new minimum if same as last *)
                    then (mins, last)
                    (* Minimum is different, so keep*)
                    else (new_min :: mins, Some new_min)
                  end
              end
      ) ([], None) hs_indexed in
    winnowed
  in
  List.rev mins


  (* let rec helper acc idx = 
    if idx < 0
    then acc
    else
      let window = Base.List.sub hs ~pos:idx ~len:size in
      let min_tup = Base.List.foldi window ~init:None ~f:(fun i acc h ->
        match acc with
          | None -> Some (h,i)
          | Some (mh,_) -> 
              if h < mh 
              then Some (h,i) 
              else acc
      ) |> Option.get in
      helper (min_tup::acc) (idx - 1)
      (* match min_tup with
      | None -> acc
      | Some t -> 
          helper (t::acc) (idx - 1) *)
  in
  helper [] (hs_length - size) |> List.rev *)


  (* FIXME handle if the string is shorter than the window *)

  (* let hs_length = List.length hs in
  let rec helper acc idx = 
    if idx >= hs_length
    then acc
    else 
      let new_elt = List.nth hs idx in
      let new_acc = if size == 1
        (* A single element window is always the minimum of itself *)
        then (new_elt,idx):: acc 
        else let (previous_elt,_) = List.hd acc in
          if new_elt < previous_elt && (not (List.exists (fun (e,_) -> e == new_elt) acc))
          then (new_elt, idx)::acc
          else acc
      in
      helper new_acc (idx + 1)
  in
  let initial_window = Base.List.sub hs ~pos:0 ~len:size in
  let initial_min = Base.List.foldi initial_window ~init:None ~f:(fun i acc h ->
        match acc with
          | None -> Some (h,i)
          | Some (mh,mi) -> 
              if h < mh 
              then Some (h,i) 
              else Some (mh,mi)
      ) |> Option.get in
  helper [initial_min] 1 *)

(* h = hashes list, w = window size *)
let winnow w h =
  winnow_alt w h


  (* (* calculates the global position of the i-th hash in the window
     example: if [global_pos 5 W] = 100, then the 5th hash in W is the 100th
     hash that was processed by the winnowing algorithm. *)
  let global_pos i w =
    let c = Window.count w in
    let s = Window.size w in
    c - (s - 1 - i)
  in
  (* helper function *)
  let mincheck ((minval,minpos),count) x =
    if x <= minval then ((x, count), count + 1)
    else ((minval, minpos), count + 1)
  in

  (*  At the end of each iteration, min is a tuple of the (value,position)
      of the rightmost minimal hash in the
      current window. hash x is only added to the fingerprint [res] the
      first time an instance of x is selected as the
      rightmost minimal hash of a window.

      hashes - the complete list of hashes
      window - window object
      acc - list of selected hash x position pairs
      n - counter for position of hashes
      (v,p) - value and position of minimum hash
  *)
  let count = ref 0 in
  let rec winnowhelper hashes window acc n (v,p) =
    count := !count + 1;
    (* Printf.printf "winnowing depth %n\n%!" !count; *)
    if n = List.length hashes then acc
    else begin
      let nexthash = List.nth hashes n in
      let new_window = Window.enqueue nexthash window in
      if nexthash <= v then
        let new_acc = (nexthash, global_pos (Window.size new_window - 1)
                         new_window)::acc in
        winnowhelper hashes new_window new_acc (n+1) (nexthash,
                                                      Window.size new_window - 1)
      else begin
        let p = p - 1 in
        if p < 0 then
          let new_min = fst (Window.fold mincheck ((max_int,0),0) new_window) in
          let new_acc = (fst new_min, global_pos (snd new_min) new_window)::acc in
          winnowhelper hashes new_window new_acc (n+1) new_min
        else
          winnowhelper hashes new_window acc (n+1) (v,p)
      end
    end
  in
  let window = Window.create w max_int in
  let res = winnowhelper h window [] 0 (max_int, 0) in 
  Printf.printf "winnowing depth %n\n%!" !count;
  res *)