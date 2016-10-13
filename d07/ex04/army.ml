class ['a] army (soldiers:'a list) =
    object
        val mutable _soldiers = soldiers

        method get_length = List.length _soldiers
        method add a = print_endline ("append: " ^  a#to_string); _soldiers <- _soldiers @ [a]
        method delete = print_endline "delete"; _soldiers <- List.tl _soldiers
        method get_army = soldiers

    end

