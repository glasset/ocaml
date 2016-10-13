let () =
    print_endline (new Atom.hydrogen#to_string);
    print_endline (new Atom.carbon#to_string);
    print_endline (new Atom.oxygen#to_string);
    print_endline (new Atom.nitrogen#to_string);
    print_endline (new Atom.titanium#to_string);
    print_endline (new Atom.neon#to_string);
    print_endline (new Atom.calcium#to_string);
    print_endline (new Atom.cobalt#to_string);
    print_endline (new Atom.arsenic#to_string);
    print_endline (string_of_bool((new Atom.cobalt)#equals new Atom.cobalt));
    print_endline (string_of_bool((new Atom.cobalt)#equals new Atom.calcium))
