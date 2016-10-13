class galifrey dalek doctor people =
    object (self)
        val _dalek : Dalek.dalek list = dalek
        val _doctor : Doctor.doctor list = doctor
        val _people : People.people list = people

        method removeDeadPeople (lst:People.people list) =
            let rec loop people (res:People.people list) = match people with
                | hd::tl ->
                        if hd#isAlive = false then begin
                            loop tl res end
                        else
                            loop tl (res @ [hd])
                | [] -> res
            in loop lst []

        method removeDeadDalek (lst:Dalek.dalek list) =
            let rec loop dalek (res:Dalek.dalek list) = match dalek with
                | hd::tl ->
                        if hd#isAlive = false then
                            loop tl res
                        else
                            loop tl (res @ [hd])
                | [] -> res
            in loop lst []

        method do_time_war =
            let rec loop dalek people doctor =
                let attack =  Random.int 3 in match attack with
                    | 0 when List.length doctor > 0 && List.length people > 0 ->
                        let hit = Random.int 151 in
                            print_endline ("doc cast spell with " ^ (string_of_int hit) ^ " damage to " ^ (List.hd people)#get_name);
                            (List.hd people)#get_dam hit;
                            if (List.hd people)#isAlive = false then print_endline ("Doctor has killed " ^ (List.hd people)#get_name)
                            else begin print_endline ("Doctor hasn't killed " ^ (List.hd people)#get_name)end;
                            (List.hd people)#get_dam hit;
                            (List.hd doctor)#talk;
                            (List.hd doctor)#travel_in_time 10 (Random.int 42);
                            print_endline "doctor run away";
                        loop (self#removeDeadDalek dalek) (self#removeDeadPeople people) (List.tl doctor)
                    | 1 when List.length doctor > 0 && List.length dalek > 0 ->
                        let hit = Random.int 151 in
                        let cur_dal = (List.hd dalek) in
                            if cur_dal#get_shield then
                                begin
                                    print_endline (cur_dal#get_name ^ " is immune doctor miss her spell");
                                    cur_dal#talk
                                end
                            else
                                begin
                                    (List.hd doctor)#use_sonic_screwdriver;
                                print_endline ("doc cast spell with " ^ (string_of_int hit) ^ " damage to " ^ cur_dal#get_name);
                                cur_dal#get_dam (Random.int 123)
                                end;
                        print_endline "doctor run away";
                        (List.hd doctor)#travel_in_time 10 (Random.int 42);
                        loop (self#removeDeadDalek dalek) (self#removeDeadPeople people) (List.tl doctor)
                    | 2 when List.length dalek > 0 && List.length people > 0 ->
                            (List.hd dalek)#exterminate (List.hd people);
                        loop (self#removeDeadDalek dalek) (self#removeDeadPeople people) doctor
                    | _ ->
                    if (List.length doctor = 0 || List.length dalek = 0) && List.length people = 0 then
                        print_endline "thanks for watching, don't forget to subscribe"
                    else if List.length doctor <> 0 then
                        loop (self#removeDeadDalek dalek) (self#removeDeadPeople people) (List.tl doctor)
                    else begin
                        loop (self#removeDeadDalek dalek) (self#removeDeadPeople people) doctor end
            in loop _dalek _people _doctor

    end
