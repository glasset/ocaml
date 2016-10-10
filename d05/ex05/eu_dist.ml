let eu_dist (a:float array) (b:float array) =
    let sum = ref 0. in
    let i = ref 0 in
    while !i < Array.length a do
        sum := !sum +. ((a.(!i) -. b.(!i)) *. (a.(!i) -. b.(!i)));
        incr i
    done;
    sqrt !sum


let () =
    print_float (eu_dist [| 1. ; 1.; 1. |] [| 2. ; 2.; 2. |]);
    print_newline ();
    print_float (eu_dist [| 1. ; 1. |] [| 3. ; 3. |]);
    print_newline ();
    print_float (eu_dist [| 1. ; 1.; 1. |] [| 4. ; 4.; 4. |]);
    print_newline ();
    print_float (eu_dist [| 3. ; 2.; 1. |] [| 1. ; 2.; 3. |]);
    print_newline ()
