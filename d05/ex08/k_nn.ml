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

type res2 = (int * float)
type res3 = (string * res2)
(* Test *)
let isEgual (a, b) s1 = if s1 = b then true else false
let getB (a, b) = b
let getA (a, b) = a

let check book a =
	let real = (List.nth book (List.length book - 1) ) in
    if (isEgual real a) then
        begin 
        print_string "|";
        1
        end
    else
        begin
        Printf.printf "{f:%s,%s}" (getB real) a;
        0
        end
(* end *)

let sort (a, b) (c, d) =
    if b = d then
        0
    else if b > d then
        1
    else
        (-1)
let getBase a b = (a, (1 , b))
let getDouble (b, c) d = (b + 1, c +. d)

let rec append ((a:string), (b:float)) (acc:res3 list) =
    try
        (List.remove_assoc a acc) @ [ (a, (getDouble (List.assoc a acc) b))]
    with
        | Not_found -> acc @ [(getBase a b)]

let take n l =
  let rec sub_list n accu l =
    match l with
    | [] -> accu
    | hd :: tl ->
      if n = 0 then accu
      else sub_list (n - 1) (accu @ [hd]) tl
  in sub_list n [] l


let getStr (a, (b, c)) = a

let best (a, (b, c)) (d, (e, f)) =
    if b > e then a
    else if f < c then d
    else a

let return_string lst =
    if (List.length lst = 1) then
        getStr (List.nth lst 0)
    else
       best (List.nth lst 0) (List.nth lst 1)

let sort2 (a, (b ,c)) (d, (e, f)) =
    if b = e then
        0
    else if b > e then
        1
    else
        (-1)

let predicte all k =
    let result = take k (List.sort sort all) in
    let rec loop lst (res:res3 list) = match lst with
        | hd::tl -> loop tl (append hd res)
        | [] -> res
    in (return_string (List.rev (List.sort sort2 (loop result []))))

(* K_nn *)
let estimate_value neighbors k (a, b) =
    let rec loop lst res = match lst with
        | (c, d)::tl -> loop tl (res @ [(d, eu_dist a c)])
        | [] -> if List.length res = 0 then [("?", 100.)] else res
    in predicte (loop neighbors [] ) k

let main str =
    let dataSet = examples_of_file str in
	let good = ref 0 in
	let total = ref 0 in
	let neighbors = ref [] in
        let rec loop dataSet =  match dataSet with
            | hd::tl -> begin
                incr total;
                good := !good + (check (!neighbors @ [hd]) (estimate_value !neighbors 5 hd)); (* K = 5 *) 
                neighbors := !neighbors @ [hd];
                loop tl
                end
            | [] -> ()
        in loop dataSet;
        print_string "\nAccuracy: ";
        print_float (((float_of_int !good) /. (float_of_int !total)) *. 100.);
       print_endline "%"

let () =
    let arr = Sys.argv in
    if Array.length arr <> 2 then
        print_endline "Need a file as argv"
    else
        begin
            main arr.(1);
        end

