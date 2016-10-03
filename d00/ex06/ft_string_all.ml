let ft_string_all ft str =
    let len = String.length str in
    let rec loop c_len =
        if c_len >= 0 then
            let c = String.get str c_len in
            if ft c then
                loop (c_len - 1)
            else
                -1
        else if c_len = 0 then
            1
        else
            0
    in
    if loop (len - 1) = 0 then
        true
    else
        false



(* TEST *)

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = c >= 'a' && c <= 'z'

let main () =
   print_string (string_of_bool (ft_string_all is_digit "ihavenodigit"));
   print_char '\n';
   print_string (string_of_bool (ft_string_all is_digit "12ihavedigit34"));
   print_char '\n';
   print_string (string_of_bool (ft_string_all is_digit "012345"));
   print_char '\n';
   print_string (string_of_bool (ft_string_all is_alpha "ihavesomedigit1234"));
   print_char '\n';
   print_string (string_of_bool (ft_string_all is_alpha "imfullalpha"));
   print_char '\n'

let () = main ()
