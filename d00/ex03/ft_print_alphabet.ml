
let ft_print_alphabet () =
    let first_char_ascii = int_of_char 'a' in
    let last_char_ascii = int_of_char 'z' in
    let rec loop current_char_ascii =
        if current_char_ascii <= last_char_ascii then
            let current_char = char_of_int current_char_ascii in
            print_char current_char;
            loop (current_char_ascii + 1)
    in
    loop first_char_ascii;
    print_char '\n'


let () = ft_print_alphabet ()
