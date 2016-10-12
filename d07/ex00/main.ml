let main () =
    let me = new People.people "glasset" in
        print_endline me#to_string;
        me#talk;
        me#die



let () = main ()
