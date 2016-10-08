module Color =
struct
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
end

module Value =
struct
type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [ T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As ]

let toInt c = match c with
	| T2 -> 1
	| T3 -> 2
	| T4 -> 3
	| T5 -> 4
	| T6 -> 5
	| T7 -> 6
	| T8 -> 7
	| T9 -> 8
	| T10 -> 9
	| Jack -> 10
	| Queen -> 11
	| King -> 12
	| As -> 13

let toString c = match c with
	| T2 -> "2"
	| T3 -> "3"
	| T4 -> "4"
	| T5 -> "5"
	| T6 -> "6"
	| T7 -> "7"
	| T8 -> "8"
	| T9 -> "9"
	| T10 -> "10"
	| Jack -> "J"
	| Queen -> "Q"
	| King -> "K"
	| As -> "A"

let toStringVerbose c = match c with
	| T2 -> "2"
	| T3 -> "3"
	| T4 -> "4"
	| T5 -> "5"
	| T6 -> "6"
	| T7 -> "7"
	| T8 -> "8"
	| T9 -> "9"
	| T10 -> "10"
	| Jack -> "Jack"
	| Queen -> "Queen"
	| King -> "King"
	| As -> "As"

let next c = match c with
	| T2 -> T3
	| T3 -> T4
	| T4 -> T5
	| T5 -> T6
	| T6 -> T7
	| T7 -> T8
	| T8 -> T9
	| T9 -> T10
	| T10 -> Jack
	| Jack -> Queen
	| Queen -> King
	| King -> As
	| As -> invalid_arg "Arg is As no next value :/"

let previous c = match c with
	| T2 -> invalid_arg "Arg is T2 no prev value :/"
	| T3 -> T2
	| T4 -> T3
	| T5 -> T4
	| T6 -> T5
	| T7 -> T6
	| T8 -> T7
	| T9 -> T8
	| T10 -> T9
	| Jack -> T10
	| Queen -> Jack
	| King -> Queen
	| As -> King
end

type t =  { color:Color.t; value:Value.t }

let newCard value color = { color = color; value = value }

let rec sort values color = match values with
	| hd::tl -> ({color = color; value = hd}) :: (sort tl color)
	| [] -> []

let allSpades = sort Value.all Color.Spade

let allHearts = sort Value.all Color.Heart

let allDiamonds = sort Value.all Color.Diamond

let allClubs = sort Value.all Color.Club

let all = allSpades @ allHearts @ allDiamonds @ allClubs

let getValue card = card.value

let getColor card = card.color

let toString card = (Value.toString card.value) ^ (Color.toString card.color)

let toStringVerbose card = Printf.sprintf "Card(%s , %s)" (Value.toStringVerbose card.value) (Color.toStringVerbose card.color)

let compare a b =
	let x = Value.toInt a.value in
	let y = Value.toInt b.value in
	 (x - y)

let max a b =
	let x = Value.toInt a.value in
	let y = Value.toInt b.value in
	if x >= y then
		a
	else
		b

let min a b =
	let x = Value.toInt a.value in
	let y = Value.toInt b.value in
	if x > y then
		b
	else
		a

let best lst = match lst with
		| hd::tl -> List.fold_left max hd tl
		| [] -> invalid_arg "can't find best, list is empty"

let isOf card col = card.color = col

let isSpade card = (card.color = Color.Spade)
let isHeart card = (card.color = Color.Heart)
let isDiamond card = (card.color = Color.Diamond)
let isClub card = (card.color = Color.Club)

