
let ft_print_comb () =
    let x = 0 in
    let y = 1 in
    let z = 2 in
    let rec calc a b c =
        print_int a;
        print_int b;
        print_int c;
        if a < 7 then
            begin
                print_string ", ";
                if c < 9 then
                    calc a b (c + 1)
                else if b < 8 then
                    calc a (b + 1) (b + 2)
                else if a < 7 then
                    calc (a + 1) (a + 2) (a + 3)
                else
                    print_string "\n"
            end
    in
    calc x y z

let () = ft_print_comb ()
