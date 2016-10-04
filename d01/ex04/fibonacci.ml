let rec fibonacci n =
    if n < 0 then
        (-1)
    else
    let rec f_acc n acc acc_s =
        if n = 0 then
            acc
        else if n = 1 then
            acc_s
        else
            f_acc (n - 1) acc_s (acc_s + acc)
    in f_acc n 0 1

let main () =
    print_endline (string_of_int (fibonacci (-42)));
    print_endline (string_of_int (fibonacci 1));
    print_endline (string_of_int (fibonacci 3));
    print_endline (string_of_int (fibonacci 4));
    print_endline (string_of_int (fibonacci 6))

let () = main ()
