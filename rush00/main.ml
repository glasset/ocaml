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

let rec getIndexSpace str i =
    if (String.length str) <= i then
        (-1)
    else if (String.get str i) = ' ' then
        i
    else
        getIndexSpace str (i + 1)

let getStrToTuples str =
    let space = getIndexSpace str 0 in
        if space = (-1) then
            ("", "")
        else
            begin
          ((String.sub str 0 space),
          (String.sub str (space + 1) ((String.length str) - (space + 1)))) end

let extract str = 
    let (a1, a2) = getStrToTuples str in
        if isNumber a1 0 && isNumber a2 0 then
            let x = int_of_string a1 in
            let y = int_of_string a2 in
                (x - 1, y) (* TODO fix x - 1 *)
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

let playerAi player board = board (* TODO *)

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

let main () =
   let board = Board.newBoard 3 in (* TODO bonus param size *)
   let players = ((Board.Player.newPlayer "O" true true false), (* TODO remove
   isPlaying see nextPlayer *) 
   (Board.Player.newPlayer "X" true false false)) in
   let rec loop player board =
        if Board.isWin board then 
            begin 
                print_endline (Board.Player.toString (nextPlayer players player)); (* win message *)
            Board.print board
            end
        else
            begin
            Board.print board;
            print_endline ((Board.Player.getSymbol player) ^ "'s turn to play.");
            loop (nextPlayer players player) (play player board)
            end
     in loop (getPlayer1 players) board

let () = main ()
