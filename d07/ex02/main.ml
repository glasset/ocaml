let main () =
    let dalek = new Dalek.dalek in
    let doctor = new Doctor.doctor "Doctor" 42 in
    let people = new People.people "peopleLow" in
        print_endline dalek#to_string;
        dalek#talk;
        dalek#exterminate people;
        dalek#talk;
        print_endline ("dalek has shield? " ^ (string_of_bool dalek#get_shield));
        dalek#die;
        print_endline ("people alive? " ^ (string_of_bool people#isAlive));
        print_endline "Doctor cannot yet die, isn't here :/";
        doctor#travel_in_time 10 14


let () = main ()
