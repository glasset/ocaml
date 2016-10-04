let converges f x n =
    let rec count f a b =
        if b = 0 then
            a
        else
            count f (f a) (b -1)
    in
    let res = count f x n
    in
    if res = f res then
        true
    else
        false

let main () =
    print_endline (string_of_bool (converges (( * ) 2) 2 5));
    print_endline (string_of_bool (converges (fun x -> x / 2) 2 3));
    print_endline (string_of_bool (converges (fun x -> x / 2) 2 2))

let () = main ()
