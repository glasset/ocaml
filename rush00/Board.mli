module Player :
sig
    type t = { symbol:string; isHuman:bool; isPlaying:bool; isInit:bool}

    val newPlayer : string -> bool -> bool -> bool -> t
    val toString : t -> string
    val isHuman : t -> bool
    val isPlaying : t -> bool
    val isInit : t -> bool
    val getSymbol : t -> string
end


module Grid :
sig
    type grid = (Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t)
    type t = {grid:grid; isWin:Player.t}

    val newGrid : unit -> t (* t.grid = (Nil, Nil, Nil)  isWin:Nil *)
    val toString : t -> int -> string
    val isWin : t -> Player.t -> t

    val isIllegal : t -> int -> bool
    val setValue :  t -> int -> Player.t -> t
end

type t

val newBoard : int -> t
val print : t -> unit (* toStringBoard *)
val isWin : t -> bool 

val isIllegal : t -> int -> int -> bool
val setValue : t -> Player.t -> (int * int) -> t
val getSize : t -> int
