let repeat_x n =
    if n < 0 then
        "Error"
    else
        let rec c_string str n =
            if n = 0 then
                str
            else
                c_string (str ^ "x") (n - 1)
        in
        c_string "" n

let main () =
    print_endline (repeat_x (-1));
    print_endline (repeat_x 0);
    print_endline (repeat_x 1);
    print_endline (repeat_x 3);
    print_endline (repeat_x 4)


let () = main ()


