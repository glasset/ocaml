let ft_sum f i max =
    let rec count f i max sum =
        if i > max then
           sum
        else
            count f (i + 1) max (sum + (int_of_float(f i)))
    in
    float_of_int (count f i max 0)

let main () =
   print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 1 10));
   print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 3 6))


let () = main ()

