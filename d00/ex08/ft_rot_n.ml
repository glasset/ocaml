
let is_alpha c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

let change_char c =
    if (c = 'Z' || c = 'z') then
        char_of_int((int_of_char c) - 25)
    else if is_alpha c then
        char_of_int((int_of_char c) + 1)
    else
        c


let rec ft_rot_n n str =
    if n > 0 then
        let str2 = String.map change_char str in
        ft_rot_n (n - 1) str2
    else
        str

(* TEST *)

let main () =
    print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 3 "0123456789");
    print_endline (ft_rot_n 42 "abcdef");
    print_endline (ft_rot_n 2 "OI2EAS67B9");
    print_endline (ft_rot_n 1 "");
    print_endline (ft_rot_n 0 "Damned !");
    print_endline (ft_rot_n 1 "NBzlk qnbjr !")


let () = main ()
