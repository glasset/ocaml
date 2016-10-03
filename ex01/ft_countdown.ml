
let rec decr n =
    if n >= 0 then
        let current = n in
        print_int current;
        print_char '\n';
        decr (current - 1)


let ft_countdown a =
    if a < 0 then
        begin
        print_char '0';
        print_char '\n'
        end
    else
       decr a

let () = ft_countdown (3)
let () = ft_countdown (0)
let () = ft_countdown (-1)
