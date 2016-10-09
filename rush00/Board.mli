module Player :
sig
    type t = { symbol:string; isHuman:bool; isFirst:bool; isInit:bool}

    val newPlayer : string -> bool -> bool -> bool -> t
    val toString : t -> string
    val isHuman : t -> bool
    val isFirst : t -> bool
    val isInit : t -> bool
    val getSymbol : t -> string
    val getFullName : t -> string
end


module Grid :
sig
    type grid = (Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t)
    type t = {grid:grid; isWin:Player.t}

    val newGrid : unit -> t
    val toString : t -> int -> string
    val isWin : t -> Player.t -> int -> t

    val isIllegal : t -> int -> bool
    val setValue :  t -> int -> Player.t -> int -> t
    val isFull : t -> bool
end

type t

val newBoard : int -> t
val print : t -> unit
val isWin : t -> bool 

val getTurn : t -> int
val isIllegal : t -> int -> int -> bool
val setValue : t -> Player.t -> (int * int) -> t
val getSize : t -> int
val isFull : t -> bool
