class player =
    object (this)
        val mutable _health = 100
        val mutable _energy = 100
        val mutable _hygiene = 100
        val mutable _happiness = 100
        val mutable _isAlive = true

        method private add x y =
            let sum = x + y in
            if sum > 100 then 100
            else sum

        method private rem x y =
            let sub = x - y in
            if sub <= 0 then begin _isAlive <- false; 0 end
            else sub

        method isAlive = _isAlive

        method eat =
            print_endline "eat";
            _health <- (this#add _health 25);
            _energy <- (this#rem _energy 10);
            _hygiene <- (this#rem _hygiene 20);
            _happiness <- (this#add _happiness 5)

        method thunder =
            print_endline "thunder";
            _health <- (this#rem _health 20);
            _energy <- (this#add _energy 25);
            _happiness <- (this#rem _happiness 20)

        method bath =
            print_endline "bath";
            _health <- (this#rem _health 20);
            _energy <- (this#rem _energy 10);
            _hygiene <- (this#add _hygiene 25);
            _happiness <- (this#add _happiness 5)

        method kill =
            print_endline "kill";
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
