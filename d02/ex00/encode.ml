let encode lst =
    let rec count lst n res =  match lst with
        | first::second::tail ->
                if first = second then
                    count (second::tail) (n + 1) res
                else
                    count (second::tail) 0 (res @ [((n + 1), first)])
        | first::tail -> count [] 0 (res @ [((n + 1), first)])
        | [] -> res
    in count lst 0 []


(* TEST *)
let rec print_tuples lst = match lst with
         | [] -> print_char '\n'
         | (i, c)::tail -> print_int i; print_char c; print_tuples tail

let main () =
    print_tuples (encode ['a'; 'a'; 'a'; 'b'; 'b'; 'b']);
    print_tuples (encode ['b'; 'a'; 'a'; 'b'; 'b']);
    print_tuples (encode [])

let () = main ()
