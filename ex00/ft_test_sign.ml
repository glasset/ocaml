
let ft_test_sign a =
    if a >= 0 then
        print_endline "positive"
      else    print_endline "negative"


let () = ft_test_sign 42
let () = ft_test_sign (-42)
let () = ft_test_sign 0
