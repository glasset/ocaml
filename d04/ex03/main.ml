let rec print x = match x with
    | hd::tl ->
    print_string hd;
    print_string " ";
    print tl
    | [] -> ()

let genOne () =
    let deck = Deck.newDeck () in
    let deckToVerb = Deck.toStringListVerbose deck in
    print deckToVerb;
    print_string "\n"

let print_tuples (a, b) =
	print_string (Deck.Card.toString a);
    print_string "\n";
	print (Deck.toStringList b);
    print_string "\n"

let main () =
	genOne ();
	let deck = Deck.newDeck () in 
	let deckStr = Deck.toStringList deck in
		print deckStr;
    	print_string "\n";
		print_tuples (Deck.drawCard deck);
	let rec loopDraw exep = 
		let (a , b) = Deck.drawCard exep in
			loopDraw b
	in loopDraw deck

let () = main ()

