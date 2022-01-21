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

type fingerprint = {
	hash : int;
	location: int;
}

(* size = window size
   hs = hashes list *)
let winnow size hs =

  let hs_length = List.length hs in

  (* FIXME have this create fingerprints *)
  let hs_indexed' = List.combine hs (Base.List.range 0 hs_length) in
  let hs_indexed = Base.List.map hs_indexed' ~f:(fun (h,l) -> {hash = h; location = l}) in
  let mins =
    let min_from_window window = begin
      (* print_endline "window"; *)
      (* List.iter (fun (h,i) -> Printf.printf "(%i,%n)\n%!" h i) window; *)

      List.fold_right (fun win min_option -> 
        match min_option with
        | None -> Some win
        | Some prev -> 
            if win.hash < prev.hash
            then Some win
            else Some prev
      ) window None |> Option.get 
    end in

    let (winnowed,_) = List.fold_left (fun acc f -> 
      let (mins, last_option) = acc in
      if f.location + size > hs_length
      then acc
      else
        let window = Base.List.sub hs_indexed ~pos:f.location ~len:size in
        let new_min = min_from_window window in
        match mins with
        | [] -> ([new_min], Some new_min)
        |_ -> let last = Option.get last_option in
              if f.location <= last.location

              (* Only have to do a single comparison *)
              then if f.hash < last.hash
                   then let new_min = f in
                        (new_min :: mins, Some new_min)
                   else acc

              (* Previous min is no longer in window.
                 Need to compare all elements in window *)
              else if new_min.hash == last.hash

                   (* No new minimum if same as last *)
                   then acc

                   (* Minimum is different, so keep*)
                   else (new_min :: mins, Some new_min)

      ) ([], None) hs_indexed in
    winnowed
  in
  List.rev mins
