module type App =
    sig
        type project = string * string * int
        val zero : project
        val combine : project -> project -> project
        val fail : project -> project
        val success : project -> project
    end

module App =
    struct
        type project = string * string * int
        let zero = ("", "", 0)
        let combine ((a,b,c):project) ((d,e,f):project) =
            let res = (c + f) / 2 in
            if res >= 80 then (a ^ d, "succeed" , (c + f) / 2)
            else (a ^ d, "" , (c + f) / 2)
        let fail ((a,b,c):project) = (a, "failed", 0)
        let success ((a,b,c):project) = (a, "succeed", 80)
    end

let print_proj (a,b,c) =
    Printf.printf "name '%s' with status '%s' and grade %d\n" a b c

let () =
    let p1 = App.zero in
    let p2 = ("p2", "" , 160) in
    let p3 = App.combine p1 p2 in
    let p4 = App.success ("p4succs", "" , 0) in
    let p5 = App.fail ("p5failed", "" , 0) in
    let p6 = App.combine ("pab", "" , 42) (" pcd", "" , 42) in
        print_proj p1;
        print_proj p2;
        print_proj p3;
        print_proj p4;
        print_proj p5;
        print_proj p6

