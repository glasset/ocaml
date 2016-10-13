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
    let army1 = new Army.army  (ppl 7 []) in
    let army2 = new Army.army  (doc 4 []) in
    let army3 = new Army.army  (dalek 3 []) in
    let galifrey = new Galifrey.galifrey army3#get_army army2#get_army army1#get_army in
        galifrey#do_time_war


let () = main ()
