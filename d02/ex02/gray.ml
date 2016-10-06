let print_gray s s2 n =
	let rec pr i size str=
		if i = size then
			str
		else if (String.get s i) <> (String.get s2 i) then
			pr (i + 1) size (str ^ "1")
		else
			pr (i + 1) size (str ^ "0")
	in print_char ' '; print_string (pr 0 (String.length s) "")

let gray n =
	let rec gray_r n str x =
		if n = 0 then
			print_gray str ("0" ^ str) x
		else
			let c = (n - 1) in 
			gray_r c (str ^ "0") x;
			gray_r c (str ^ "1") x
			in gray_r n "" n; print_char '\n'


(* TEST *)
let () =
gray 1;
gray 2;
gray 3
