let main () =
    let rec display all = match all with
          | hd::tl -> print_string ((Color.toString hd) ^ ": " ^ (Color.toStringVerbose hd) ^ "\n"); display tl
          | [] -> ()
    in display Color.all


let () = main ()
