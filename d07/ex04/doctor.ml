class doctor (name:string) (age:int) =
    object (self)
    val _name = name
    val _age = age
    val mutable _hp = 0
    val _sidekick = new People.people name

    initializer print_endline "doctor initializer called"; self#regenarate
    method to_string = "doctor (name: " ^ _name ^ ", age: " ^ (string_of_int _age) ^ ", hp: " ^ (string_of_int _hp) ^ ", sidekick: " ^ _sidekick#to_string ^ ")"
    method talk = print_endline "Hi! I’m the Doctor!"
    method travel_in_time (start:int) (arrival:int) = 
		if start > arrival then
			begin print_endline ((string_of_int (start - arrival)) ^ " years ago") end
		else
			begin print_endline ((string_of_int (arrival - start)) ^ " years later") end;

    method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    method private regenarate = _hp <- 100
    end
