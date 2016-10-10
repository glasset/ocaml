

let getArg =
    if Array.length Sys.argv > 1 then
    let value = Array.get Sys.argv 1 in
       try int_of_string value with
            | _ -> 0
    else
        0

let main () =
    let x = getArg in
        Unix.sleep x


let () = main ()
