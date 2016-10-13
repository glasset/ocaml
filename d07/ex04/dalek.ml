class dalek =
    object (self)
        val _name = "Dalek" ^ (String.init 3
            (function i -> Random.self_init (); char_of_int(65 +  (Random.int 25))))
        val mutable _hp = 100
        val mutable _shield = true

        method to_string = "dalek (name: " ^ _name ^ ", hp: " ^ (string_of_int _hp) ^ ", shield: " ^ (string_of_bool _shield)^")"

        method get_name = _name
        method exterminate (low:People.people) =
            low#get_dam 100;
            _shield <- not _shield;
            print_endline (_name ^ " take a ez kill on " ^ low#get_name);
        method isAlive = if _hp <= 0 then false else true
        method get_dam x = _hp <- _hp - x; if self#isAlive = false then self#die
        method get_shield = _shield
        method die = print_endline "Emergency Temporal Shift!"
        method talk =
            let x = Random.int 4 in match x with
                | 0 -> print_endline "Explain! Explain!"
                | 1 -> print_endline "Exterminate! Exterminate!"
                | 2 -> print_endline "I obey!"
                | 3 -> print_endline "You are the Doctor! You are the enemy of the Daleks!"
                | _ -> ()
    end
