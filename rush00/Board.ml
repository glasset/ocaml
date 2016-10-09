module Player =
struct
    type t = { symbol:string; isHuman:bool; isFirst:bool; isInit:bool}

    let newPlayer c a b init = { symbol = c; isHuman = a; isFirst = b; isInit =
        init }

    let toString t = t.symbol ^ " wins the game in "
    let isHuman t = t.isHuman
    let isFirst t = t.isFirst
    let isInit t = t.isInit
    let getSymbol t = String.make 1 (String.get t.symbol 0)
    let getFullName t = t.symbol
end


module Grid =
struct
    type grid = (Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t)
    type t = {grid:grid; isWin:Player.t}


    let newGrid () = let player = Player.newPlayer "-" false false true in
    { grid = (player, player, player, player, player, player, player, player,
    player); isWin = player}

    let toString t n = 
        let (a, b, c, d, e, f, g, h, i) = t.grid in
        match n with
        | 1 -> (Player.getSymbol a) ^ " " ^ (Player.getSymbol b) ^ " " ^
        (Player.getSymbol c) 
        | 2 -> (Player.getSymbol d) ^ " " ^ (Player.getSymbol e) ^ " " ^
        (Player.getSymbol f) 
        | 3 -> (Player.getSymbol g) ^ " " ^ (Player.getSymbol h) ^ " " ^
        (Player.getSymbol i)
        | _ -> "" 

    let getWinPlayer t = t.isWin

    let isWin t player pos =
       let s1 = Player.newPlayer "\\" true true false in 
       let s2 = Player.newPlayer (Player.getSymbol player) true true false in 
       let s3 = Player.newPlayer "/" true true false in 
       let s4 = Player.newPlayer (Player.getSymbol player) true true false in 
       let s5 = Player.newPlayer "-" true true false in 
       let s6 = Player.newPlayer "|" true true false in
       let s7 = Player.newPlayer " " true true false in 
       let print = (Player.getFullName player) ^ " wins grid " ^ (string_of_int
       (pos + 1)  ^ "!") in
       let p2 = (s1,s7, s3, s7, s4, s7, s3, s7, s1) in
       let p1 = (s3, s5, s1, s6, s2, s6, s1, s5, s3) in
       let fn p = if Player.isFirst p then p1 else p2 in
       match t.grid with
       | (a, b, c, _, _, _, _, _,_) when a = b && b = c && Player.isInit a = false -> print_endline print; {grid = (fn player); 
       isWin = player}   
       | (a, _, _, b, _, _, c, _,_) when a = b && b = c && Player.isInit a = false -> print_endline print; {grid = (fn player);
       isWin = player}
       | (_, _, a, _, _, b, _, _,c) when a = b && b = c && Player.isInit a = false -> print_endline print; {grid = (fn player);   
       isWin = player}
       | (_, a, _, _, b, _, _, c,_) when a = b && b = c && Player.isInit a = false -> print_endline print; {grid = (fn player);   
       isWin = player}
       | (_, _, _, a, b, c, _, _,_) when a = b && b = c && Player.isInit a = false -> print_endline print; {grid = (fn player);   
       isWin = player}
       | (a, _, _, _, b, _, _, _,c) when a = b && b = c && Player.isInit a = false -> print_endline print; {grid = (fn player);   
       isWin = player}
       | (_, _, a, _, b, _, c, _,_) when a = b && b = c && Player.isInit a = false -> print_endline print; {grid = (fn player);   
       isWin = player}
       | (_, _, _, _, _, _, a, b,c) when a = b && b = c && Player.isInit a = false -> print_endline print; {grid = (fn player);
       isWin = player}
       | _ -> t

    let isIllegal t x = 
        if Player.isInit t.isWin = false then
            true
        else
            match x, t.grid with
            | 1, (a, _, _, _, _, _, _, _, _) when Player.isInit a = false -> true
            | 2, (_, a, _, _, _, _, _, _, _) when Player.isInit a = false -> true
            | 3, (_, _, a, _, _, _, _, _, _) when Player.isInit a = false -> true
            | 4, (_, _, _, a, _, _, _, _, _) when Player.isInit a = false -> true
            | 5, (_, _, _, _, a, _, _, _, _) when Player.isInit a = false -> true
            | 6, (_, _, _, _, _, a, _, _, _) when Player.isInit a = false -> true
            | 7, (_, _, _, _, _, _, a, _, _) when Player.isInit a = false -> true
            | 8, (_, _, _, _, _, _, _, a, _) when Player.isInit a = false -> true
            | 9, (_, _, _, _, _, _, _, _, a) when Player.isInit a = false -> true
            | _, _ -> false

    let setValue t x p pos = 
        let res =  match x, t.grid with
            | 1, (a, b, c, d, e, f, g, h, i) -> {grid = (p,b,c,d,e,f,g,h,i);
            isWin = t.isWin}
            | 2, (a, b, c, d, e, f, g, h, i) -> {grid = (a,p,c,d,e,f,g,h,i);
            isWin = t.isWin}
            | 3, (a, b, c, d, e, f, g, h, i) -> {grid = (a,b,p,d,e,f,g,h,i);
            isWin = t.isWin}
            | 4, (a, b, c, d, e, f, g, h, i) -> {grid = (a,b,c,p,e,f,g,h,i);
            isWin = t.isWin}
            | 5, (a, b, c, d, e, f, g, h, i) -> {grid = (a,b,c,d,p,f,g,h,i);
            isWin = t.isWin}
            | 6, (a, b, c, d, e, f, g, h, i) -> {grid = (a,b,c,d,e,p,g,h,i);
            isWin = t.isWin}
            | 7, (a, b, c, d, e, f, g, h, i) -> {grid = (a,b,c,d,e,f,p,h,i);
            isWin = t.isWin}
            | 8, (a, b, c, d, e, f, g, h, i) -> {grid = (a,b,c,d,e,f,g,p,i);
            isWin = t.isWin}
            | 9, (a, b, c, d, e, f, g, h, i) -> {grid = (a,b,c,d,e,f,g,h,p);
            isWin = t.isWin}
            | _, _ -> t
        in isWin res p pos



    let isFull t =
        let rec loop t i =
            if i > 9 then
               true
            else if isIllegal t i <> true then
               false
            else  
                loop t (i + 1)
         in loop t 1


end


type t = { grid:Grid.t list; isWin:Player.t; size:int; turn:int}


let rec feedGrid lst i = 
    if i = 0 then
        lst
    else
        feedGrid (lst @ [Grid.newGrid ()]) (i - 1)
    

let newBoard i = { grid = (feedGrid [] (i * i)); isWin = (Player.newPlayer "-" false false
true); size = i; turn = 0} 


let print t =
    let separator = (String.make ( 5 * t.size + ((t.size -1) * 3 * 3)) '-') ^ "\n" in
    let  rec loop grids index pos str bl =
        if bl <= t.size && List.length grids > index && pos < 4 then
            loop grids (index + 1) pos (str ^ " | " ^ (Grid.toString (List.nth grids index
            ) pos) ^ " | ") (bl + 1)
        else if bl = (t.size + 1) then
            begin
                if pos < 3 then
                    loop grids (index - (t.size)) (pos + 1) (str ^ "\n") 1
                else
                    loop grids (index) 1 (str ^ "\n"  ^ separator) 1

            end
        else
            str
    in print_string (loop t.grid 0 1 separator 1)


let rec diag lst prev i size =
    if List.length lst <= i then
        true
    else if (List.nth lst i) <> prev || Player.isInit (Grid.getWinPlayer prev) then
        false
    else
       diag lst (List.nth lst i) (i + size + 1) size

let rec diag2 lst prev i size =
    if ((List.length lst - 1) <= i) then
        true
    else if (List.nth lst i) <> prev || Player.isInit (Grid.getWinPlayer prev) then
        false
    else
        diag2 lst (List.nth lst i) (i + size - 1) size


let rec vert2 lst prev i size =
    if (List.length lst <= i) then
        true
    else if (List.nth lst i) <> prev || Player.isInit (Grid.getWinPlayer prev) then
        false
    else
       vert2 lst (List.nth lst i) (i + size) size

let rec vert lst i size =
    if i > size then
        false
    else if (vert2 lst (List.nth lst i) i  size) then
        true
    else 
        vert lst (i + 1)  size

let rec horiz2 lst prev i size c =
    if c = (size) then
        true
    else if (List.nth lst i) <> prev || Player.isInit (Grid.getWinPlayer prev) then
        false
    else
        horiz2 lst (List.nth lst i) (i + 1) size (c + 1) 

 
let rec horiz lst i size =
    if i > ((size * size) - 1) then
        false
    else if (horiz2 lst (List.nth lst i) i size) 0 then
        true
    else
        horiz lst (i + size) size
        
let isWin t =
    if diag t.grid (List.nth t.grid 0) 0 t.size
        || diag2 t.grid (List.nth t.grid (t.size - 1)) (t.size - 1) t.size
        || vert t.grid 0 t.size
        || horiz t.grid 0 t.size then
            true
    else
        false


let isIllegal t x y = 
    if Grid.isIllegal (List.nth t.grid (x - 1)) y = true then
        true
    else
        false

let setTurn player x =

if Player.isFirst player = false then (x + 1) else x

let setValue t player (a, b) =
    let updateGrid x old = 
        if x = (a - 1) then
            Grid.setValue old b player x
        else
            old
    in {grid = (List.mapi updateGrid t.grid); isWin = t.isWin; size = t.size;
    turn = (setTurn player t.turn ) }


let getTurn t = t.turn

let getSize t = t.size

let isFull t =
    if  List.for_all (Grid.isFull) t.grid then
        true
    else
        false
