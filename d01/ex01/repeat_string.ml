
let repeat_string ?(str="") n =
    let s = str in
    let rec concat str n =
    if n < 0 then
        "Error"
    else if n = 0 || n = 1 then
        str
    else
        concat (str ^ s) (n - 1)
    in concat s n


let main () =
    print_endline (repeat_string (-1));
    print_endline (repeat_string 0);
    print_endline (repeat_string ~str:"Toto" 1);
    print_endline (repeat_string ~str:"xx" 2);
    print_endline (repeat_string ~str:"a" 4);
    print_endline (repeat_string ~str:"What" 3)


let () = main ()
