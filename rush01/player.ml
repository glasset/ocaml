class player =
    object (this)
        val mutable _health = 100
        val mutable _energy = 100
        val mutable _hygiene = 100
        val mutable _happiness = 100
        val mutable _isAlive = true


        initializer
            begin try
                let file = open_in "save.itama" in
                  let rec loop i str =
                    this#setVal i (int_of_string str);
                    loop (i + 1) (input_line file)
                in ignore(loop 0 (input_line file)); close_in file
                with
                    | Sys_error err -> this#save
                    | Failure err -> this#save;
                    | End_of_file -> ()
            end

        method private setVal i x = match i with
            | 0 -> _health <- x
            | 1 -> _energy <- x
            | 2 -> _hygiene <- x
            | 3 -> _happiness <- x
            | _ -> ()

        method private add x y =
            let sum = x + y in
            if sum > 100 then 100
            else sum

        method private rem x y =
            let sub = x - y in
            if sub <= 0 then begin _isAlive <- false; 0 end
            else sub

        method save =
            let file = open_out "save.itama" in
                output_string file ((string_of_int _health) ^ "\n");
                output_string file ((string_of_int _energy) ^ "\n");
                output_string file ((string_of_int _hygiene) ^ "\n");
                output_string file ((string_of_int _happiness) ^ "\n");
                flush file;
                close_out file


        method isAlive = _isAlive

        method oneHit = _health <- (this#rem _health 1)

        method regen = _health <- 100; _energy <- 100; _hygiene <- 100; _happiness <- 100; _isAlive <- true

        method eat =
            _health <- (this#add _health 25);
            _energy <- (this#rem _energy 10);
            _hygiene <- (this#rem _hygiene 20);
            _happiness <- (this#add _happiness 5)

        method thunder =
            _health <- (this#rem _health 20);
            _energy <- (this#add _energy 25);
            _happiness <- (this#rem _happiness 20)

        method bath =
            _health <- (this#rem _health 20);
            _energy <- (this#rem _energy 10);
            _hygiene <- (this#add _hygiene 25);
            _happiness <- (this#add _happiness 5)

        method kill =
            _health <- (this#rem _health 20);
            _energy <- (this#rem _energy 10);
            _happiness <- (this#add _happiness 20)

        method getStats = [
            ("Health" , float_of_int(_health));
            ("Energy" , float_of_int(_energy));
            ("Hygiene" , float_of_int(_hygiene));
            ("Happy" ,float_of_int( _happiness))
        ]

    end
