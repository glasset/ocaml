type radar = (float array * string)

let extract_tuple line =
	let rec loop lst i =
	if i >= (String.rindex_from line (String.length line - 1) ',') then begin
		[String.sub line i ((String.length line) - i)] @ lst end
	else
		begin
		let sep = ((String.index_from line i ',') - i) in
			loop (lst @ [String.sub line i sep]) (i + sep + 1)
		end
	in  let res = loop [] 0 in (Array.of_list (List.map float_of_string (List.tl res)), List.hd res)

let examples_of_file fileName =
	let set = ref [ ] in
    begin try
    let file = open_in fileName in
        while true do
            let line = input_line file in
				set := !set @ [extract_tuple line]
        done;
        close_in file
    with
        | Sys_error err -> print_endline "Failed to open file"
        | End_of_file -> ()
    end;
	!set

let eu_dist (a:float array) (b:float array) =
    let sum = ref 0. in
    let i = ref 0 in
    while !i < Array.length a do
        sum := !sum +. ((a.(!i) -. b.(!i)) *. (a.(!i) -. b.(!i)));
        incr i
    done;
    sqrt !sum

type res = (string * int)

(* Test *)
let isEgual (a, b) s1 = if s1 = b then true else false
let get (a, b) = b

let check book (a, b) =
	let real = (List.nth book (List.length book - 1) ) in
    if (isEgual real a) then
        print_string "|"
    else
        Printf.printf "{f:%s,%s}" (get real) a
(* end *)

let sort (a, b) (c, d) =
    if b = d then
        0
    else if b > d then
        1
    else
        (-1)

let rec append (a, b) res =
    try
        (List.remove_assoc a res) @ [(a, (List.assoc a res) + 1)]
    with
        | Not_found -> res @ [(a, 1)]

let take n l =
  let rec sub_list n accu l =
    match l with
    | [] -> accu
    | hd :: tl ->
      if n = 0 then accu
      else sub_list (n - 1) (accu @ [hd]) tl
  in sub_list n [] l

let predicte all neighbors =
    let result = take 1 (List.sort sort all) in
    let rec loop lst res = match lst with
        | hd::tl -> loop tl (append hd res)
        | [] -> res
    in (List.nth (List.rev (List.sort sort (loop result []))) 0)

let estimate_value neighbors (a, b) =
    let rec loop lst res = match lst with
        | (c, d)::tl -> loop tl (res @ [(d, eu_dist a c)])
        | [] -> if List.length res = 0 then [("?", 100.)] else res
    in predicte (loop neighbors [] ) (neighbors @ [(a, b)])

let main str =
    let dataSet = examples_of_file str in
	    let neighbors = ref [] in
        let rec loop dataSet =  match dataSet with
            | hd::tl -> begin check (!neighbors @ [hd]) (estimate_value !neighbors hd);neighbors := !neighbors @ [hd]; loop tl end
            | [] -> ()
        in loop dataSet

let () =
    let arr = Sys.argv in
    if Array.length arr <> 2 then
        print_endline "Need a file as argv"
    else
        begin
            main arr.(1);
       print_newline ()
        end

