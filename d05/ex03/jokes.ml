let extractJokes name =
    let allJokes = ref [| |] in
    begin try
    let file = open_in name in
    let str = ref "" in
        while true do
            let line = input_line file in
                if line <> "(**)" then
                    begin
                    if !str <> "" then
                        str := !str ^ "\n" ^ line
                    else
                        str := line
                    end
                else
                    begin
                        allJokes := Array.append !allJokes [| !str |];
                        str := ""
                    end
        done;
        close_in file
    with
        | Sys_error err -> print_endline "Failed to open file"
        | End_of_file -> print_endline !allJokes.(Random.int (Array.length !allJokes))
    end

let () =
    Random.self_init ();
    let arr = Sys.argv in
    if Array.length arr <> 2 then
        print_endline "Need a file as argv"
    else
        extractJokes arr.(1)

