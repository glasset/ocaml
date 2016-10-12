let main () =
    let me = new Doctor.doctor "doctor" 20 in
        print_endline me#to_string;
        me#talk;
        me#travel_in_time 10 14;
        me#travel_in_time 14 10;
		me#use_sonic_screwdriver



let () = main ()
