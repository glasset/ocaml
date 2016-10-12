class people (name:string) =
    object
        val _name = name
        val _hp = 100

        initializer print_endline "people initializer called"
        method to_string = "people (name: " ^ _name ^ ", hp: " ^ (string_of_int _hp) ^ ")"
        method talk = print_endline ("I’m " ^ _name ^ "! Do you know the Doctor?")
        method die = print_endline "Aaaarghh!"
    end
