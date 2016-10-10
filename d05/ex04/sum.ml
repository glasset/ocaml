let sum (a:float) (b:float) = a +. b


let () =
    print_float (sum 2.2 2.2);
    print_newline ();
    print_float (sum (-2.2) 2.2);
    print_newline ();
    print_float (sum (-2.2) (-2.2));
    print_newline ();
    print_float (sum 1.8 1.2);
    print_newline ();
    print_float (sum 12394993249124912.2 210402140120401244012.1);
    print_newline ();
    print_float (sum 2.2 2.1);
    print_newline ()
