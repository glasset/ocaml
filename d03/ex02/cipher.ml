let is_alpha c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

let change_char c =
    if (c = 'Z' || c = 'z') then
        char_of_int((int_of_char c) - 25)
    else if is_alpha c then
        char_of_int((int_of_char c) + 1)
    else
        c

let rec caesar n str =
    if n > 0 then
        let str2 = String.map change_char str in
        caesar (n - 1) str2
    else
        str

let rot42 str =
    caesar 42 str

let xor key str=
	let xor_c c =
		char_of_int ((int_of_char c) lxor key)
	in String.map xor_c str

let rec ft_crypt str fs = match fs with
		| hd::tl -> ft_crypt (hd str) tl
		| [] -> str



