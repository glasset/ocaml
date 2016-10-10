type data = (float array * string)

let extract_tuple line =
	let rec loop lst i =
	if (i + 1) >= String.length line then begin
		[String.sub line i 1] @ lst end
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
let check book estimate =
    if (isEgual (List.nth book (List.length book - 1) ) estimate) then
        print_string "T"
    else
        print_string "F"
(* end *)
(* TODO List.sort sur (a, B) + nth sur les N first + avg by char *)
let predicte res neighbors =
    check neighbors "g";
    neighbors

let estimate_value neighbors (a, b) =
    let rec loop lst res = match lst with
        | (c, d)::tl -> loop tl res @ [(d, eu_dist a c)]
        | [] -> []
    in predicte (loop neighbors [] ) (neighbors @ [(a, b)])


let main str =
    let dataSet = examples_of_file str in
        let rec loop dataSet acc =  match dataSet with
            | hd::tl -> loop tl (estimate_value acc hd)
            | [] -> ()
        in loop dataSet []


let () =
    let arr = Sys.argv in
    if Array.length arr <> 2 then
        print_endline "Need a file as argv"
    else
       main arr.(1)

