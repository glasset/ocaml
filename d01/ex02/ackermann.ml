let ackermann x y =
    if x < 0 || y < 0 then
        (-1)

let main () =
    print_endline (string_of_int (ackermann (-1) 7));
    print_endline (string_of_int (ackermann 0 0));
    print_endline (string_of_int (ackermann 1 7));
    print_endline (string_of_int (ackermann 4 1))

let () = main ()
