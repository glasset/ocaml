let rec hfs_f n =
    if n = 0 then
        1
    else if n > 0 then
        (n - hfs_m (hfs_f (n - 1)))
    else
        (-1)

and hfs_m x =
    if x = 0 then
        0
    else if x > 0 then
        (x - hfs_f (hfs_m (x - 1)))
    else
        (-1)

let main () =
    print_endline (string_of_int (hfs_m 0));
    print_endline (string_of_int (hfs_f 0));
    print_endline (string_of_int (hfs_m 4));
    print_endline (string_of_int (hfs_f 4))

let () = main ()
