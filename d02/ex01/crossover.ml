let crossover lst1 lst2 =
    let rec match_lst lst lst2 res = match lst with
        | first::tail ->
                let rec in_list x lst2 = match lst2 with
                    | [] -> false
                    | first::tail -> (x = first) || (in_list x tail)
                in
                if (in_list first lst2) then
                    match_lst tail lst2 (res @ [first])
                else
                    match_lst tail lst2 res
        | [] -> res
    in
    match_lst lst1 lst2 []


(* TEST *)
let rec print lst = match lst with
         | [] -> print_char '\n'
         | first::tail -> print_char first; print tail

let rec print_2 lst = match lst with
         | [] -> print_char '\n'
         | first::tail -> print_int first; print_2 tail

let main () =
    print (crossover ['a'; 'b'] ['b'; 'c']);
    print (crossover ['b'; 'a']  ['b'; 'a']);
    print (crossover ['b'; 'a']  ['b'; 'a']);
    print (crossover ['a']  []);
    print (crossover []  ['b']);
    print_2 (crossover [0; 1; 2; 3; 5; 8]  [8; 13; 21; 34])

let () = main ()
