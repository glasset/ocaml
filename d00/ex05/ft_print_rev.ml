
let ft_print_rev str =
    let len = String.length str in
    let rec getchar c_len =
        if c_len >= 0 then
            let c = String.get str c_len in
            print_char c;
            getchar (c_len - 1)
    in
    getchar (len - 1);
    print_char '\n'

let () = ft_print_rev "salut !"
let () = ft_print_rev "Hello world !"
let () = ft_print_rev ""
let () = ft_print_rev "): em esrever"
