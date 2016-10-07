let is_alpha c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

let change_char c =
    if (c = 'A' || c = 'a') then
        char_of_int((int_of_char c) + 25)
    else if is_alpha c then
        char_of_int((int_of_char c) - 1)
    else
        c

let rec uncaesar n str =
    if n > 0 then
        let str2 = String.map change_char str in
        uncaesar (n - 1) str2
    else
        str

let unrot42 str =
    uncaesar 42 str

let ft_uncrypt str fs = Cipher.ft_crypt str fs
