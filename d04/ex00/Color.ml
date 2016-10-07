type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]

let toString c =  match c with
	| Spade -> "S"
	| Heart -> "H"
	| Diamond -> "D"
	| Club -> "C"

let toStringVerbose c = match c with
	| Spade -> "Spade"
	| Heart -> "Heart"
	| Diamond -> "Diamond"
	| Club -> "Club"

