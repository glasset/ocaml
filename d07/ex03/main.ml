let rec ppl i lst =
    if i = 0 then lst
    else ppl (i - 1) (lst @ [new People.people ("ppl" ^ (string_of_int i))])

let rec doc i lst =
    if i = 0 then lst
    else doc (i - 1) (lst @ [new Doctor.doctor ("doc" ^ (string_of_int i)) i])


let rec dalek i lst =
    if i = 0 then lst
    else dalek (i - 1) (lst @ [new Dalek.dalek])

let main () =
    let army1 = new Army.army  (ppl 3 []) in
    let army2 = new Army.army  (doc 2 []) in
    let army3 = new Army.army  (dalek 4 []) in
    Printf.printf "a1: %d, a2: %d, a3: %d\n" army1#get_length army2#get_length army3#get_length;
        army1#add (new People.people "appendPpl");
        army2#add (new Doctor.doctor "appendDoc" 42);
        army3#add (new Dalek.dalek);
    Printf.printf "a1: %d, a2: %d, a3: %d\n" army1#get_length army2#get_length army3#get_length;
        army1#delete;
        army2#delete;
        army3#delete;
    Printf.printf "a1: %d, a2: %d, a3: %d\n" army1#get_length army2#get_length army3#get_length


let () = main ()
