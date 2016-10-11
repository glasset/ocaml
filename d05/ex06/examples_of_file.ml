type data = (float array * string)

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


(* test *)

let arrToString arr =
	let arrSize = Array.length arr in
	let rec loop str i =
		if arrSize <= i then
			str
		else
			loop (str ^ "," ^ (string_of_float arr.(i))) (i + 1)
	in loop "" 0

let print_tuple (a,b) =
	Printf.printf "(a:%d, b:%s)" (Array.length a) b

let rec print_lst lst = match lst with
	| hd::tl -> print_tuple hd; print_lst tl
	| [] -> ()


let () =
    let arr = Sys.argv in
    if Array.length arr <> 2 then
        print_endline "Need a file as argv"
    else
        print_lst (examples_of_file arr.(1)); print_newline ();

