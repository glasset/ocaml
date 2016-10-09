let rec isNumber str i =
    if (String.length str) = i && str <> "" then
        true
    else if str = "" || String.get str i  < '0' || String.get str i > '9' then
        false
    else
        isNumber str (i + 1)

let isOutOfBound c size =
    if c > size || c < 0 then
        true
    else
        false

let rec getIndexChar c str i =
    if (String.length str) <= i then
        (-1)
    else if (String.get str i) = c then
        i
    else
        getIndexChar c str (i + 1)

let getStrToTuples c str =
    let space = getIndexChar c str 0 in
        if space = (-1) then
            ("", "")
        else
            begin
          ((String.sub str 0 space),
          (String.sub str (space + 1) ((String.length str) - (space + 1)))) end

let extract str = 
    let (a1, a2) = getStrToTuples ' ' str in
        if isNumber a1 0 && isNumber a2 0 then
            let x = int_of_string a1 in
            let y = int_of_string a2 in
                (x, y)
        else
            ((-1), (-1))

let isValid str board = 
    let (x, y) = extract str in 
        if isOutOfBound x ((Board.getSize board) * (Board.getSize board)) || isOutOfBound y 9 then
            begin
            print_endline "Incorrect format.";
            false
            end
        else if Board.isIllegal board x y then
            begin
                print_endline "Illegal move.";
            false
            end
        else
            true

let rec playerHuman player board =
   let str = read_line () in
    if isValid str board then
       Board.setValue board player (extract str)
    else
       playerHuman player board 

let playerAi player board =
    let rec isIll () =
        let size = Board.getSize board in
        let rndInt = (Random.int (size * size), Random.int 9) in
        let rnd (x, y) = (x+1, y+1) in
        let (x, y) = rnd rndInt in
        
        if Board.isIllegal board x y <> true then
            Board.setValue board player (x, y)
        else
            isIll () in
    isIll ()


let play player board =
    if Board.Player.isHuman player then
        playerHuman player board
    else
        playerAi player board

let nextPlayer t player =
    match t with
    | (x, y) when x = player -> y
    | (x, _) -> x

let getPlayer1 (x, y) = x

type opt = { boardSize:int; p1:string; p2:string; vsBot:bool }

let rec printArgv argv = match argv with
        | hd::tl -> print_endline hd;printArgv tl
        | [] -> () 

let oppositeBool b = if b then false else true

let printTuples (a,b) = Printf.printf "(%s,%s)" a b


let rec setOptValues lst d = 
    match lst, d with
    | (x, y)::tl, {boardSize; p1;p2;vsBot} when x = "p1"  && String.length y > 0
    &&  y <> p2 -> setOptValues tl {boardSize; p1 =
        y;p2;vsBot}
    | (x, y)::tl, {boardSize; p1;p2;vsBot} when x = "p2" && String.length y > 0
    && y <> p1 -> setOptValues tl {boardSize; p1; p2 = y;vsBot}
    | (x, y)::tl, {boardSize; p1;p2;vsBot} when x = "size" && String.length y >
    0 && y <> "1" && y <> "2" -> let size = int_of_string y
    in setOptValues tl {boardSize =  size ; p1;p2;vsBot}
    | (x, y)::tl, {boardSize; p1;p2;vsBot} when x = "vsAi" && String.length y > 0 -> let bot = bool_of_string y
    in setOptValues tl {boardSize; p1; p2; vsBot = bot}
    | _ -> d

let getArgv () =
    let default = {boardSize = 3; p1 = "O"; p2 = "X"; vsBot = true; } in
    let argv = List.tl (Array.to_list Sys.argv) in
    let tuplesL = List.map (getStrToTuples '=') argv in
        setOptValues tuplesL default    

let rec start_newGame () =
    print_endline "Retry ? [y/n]";
    let str = read_line () in
        if str = "y" || str = "yes" || str = "Y" then
            true
        else
            false

let main () =
   
   Random.self_init ();
   let options = getArgv () in
   let board = Board.newBoard options.boardSize in
   let players = ((Board.Player.newPlayer options.p1 true true false),
   (Board.Player.newPlayer options.p2 (oppositeBool options.vsBot)  false false)) in
   let rec loop player board =
        if Board.isWin board then 
            begin 
                print_endline (Board.Player.toString (nextPlayer players player)
            ^ string_of_int ((Board.getTurn board) + 1) ^ " turns!"); 
            Board.print board;
            if start_newGame () then
                loop player (Board.newBoard options.boardSize)

            end
        else if Board.isFull board then
            begin
            print_endline "It's a tie!";
            Board.print board;
            if start_newGame () then
                loop player (Board.newBoard options.boardSize)
            end
        else
            begin
            Board.print board;
            print_endline ((Board.Player.getFullName player) ^ "'s turn to play.");
            loop (nextPlayer players player) (play player board)
            end
     in loop (getPlayer1 players) board

let () = main ()
