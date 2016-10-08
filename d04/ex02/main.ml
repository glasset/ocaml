let main () =
    let rec print cards = match cards with
            | hd::tl -> print_string ((Card.toString hd) ^ ": " ^ (Card.toStringVerbose hd) ^ "\n"); print tl
            | [] -> ()
    in print Card.all;
    let ass = Card.newCard Card.Value.As Card.Color.Spade in
    let jack = Card.newCard Card.Value.Jack Card.Color.Spade in
        print_string "eval As spade vs Jack spade\n";
        print_int (Card.compare ass jack); print_string " (compare)\n";
        print_string ((Card.toString (Card.max ass jack)) ^ " (max)\n");
        print_string ((Card.toString (Card.min ass jack)) ^ " (min)\n");
        print_string ((Card.toString (Card.best [ass ; jack])) ^ " (best)\n");
        if Card.isDiamond ass <> true then
            print_string "card is not diamond\n";
        if Card.isOf ass Card.Color.Spade then
            print_string "card is Spade\n";
        print_string ((Card.toString (Card.best [])) ^ "\n")



let () = main ()
