let ft_is_palindrome str =
    let len = (String.length str - 1) in
    if len > 0 then
        let rec loop i =
            let c_first = String.get str i in
            let c_last = String.get str (len - i) in
            if i < len && c_first = c_last then
                loop (i + 1)
            else if i = len then
                true
            else
                false
        in
        loop 0
    else
        true

let main () =
   print_string (string_of_bool (ft_is_palindrome "radar"));
   print_char '\n';
   print_string (string_of_bool (ft_is_palindrome "madam"));
   print_char '\n';
   print_string (string_of_bool (ft_is_palindrome "LOL"));
   print_char '\n';
   print_string (string_of_bool (ft_is_palindrome "mon nom"));
   print_char '\n';
   print_string (string_of_bool (ft_is_palindrome ""));
   print_char '\n';
   print_string (string_of_bool (ft_is_palindrome "hiimnotpalindrome"));
   print_char '\n';
   print_string (string_of_bool (ft_is_palindrome "car"));
   print_char '\n'

let () = main ()
