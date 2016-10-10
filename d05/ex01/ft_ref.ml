type 'a ft_ref = {mutable contents: 'a}

let return a = {contents = a}

let get a = a.contents

let set a b = a.contents <- b

let bind a  (f: 'a -> 'b ft_ref) = f a.contents


let to_string a =  return (string_of_int a)

let () =
    let a = return 42 in
    print_int (get a); print_newline ();
        set a 21;
        print_int (get a); print_newline ();
        let b = bind a to_string in
        print_endline (get b);
            set b "imstring";
            print_endline (get b)
