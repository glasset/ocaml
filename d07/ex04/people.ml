class people (name:string) =
    object (self)
        val _name = name
        val mutable _hp = 100

        initializer print_endline "people initializer called"
        method to_string = "people (name: " ^ _name ^ ", hp: " ^ (string_of_int _hp) ^ ")"
        method talk = print_endline ("I’m " ^ _name ^ "! Do you know the Doctor?")
        method get_name = _name
        method isAlive = if _hp <= 0 then false else true
        method get_dam x = _hp <- _hp - x; if self#isAlive = false then self#die
        method die = print_endline "Aaaarghh!"
    end
