let main () =
    let rec display all = match all with
       | hd::tl ->
       begin
           print_string ((Value.toString hd) ^ ": " ^ (Value.toStringVerbose hd) ^ " and ");
           print_string ("next is:" ^ (Value.toString (Value.next hd)) ^ "\n");
           display tl
       end
          | [] -> ()
    in display Value.all

let () = main ()
