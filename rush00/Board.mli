module Player:
sig
    type t = { symbol:char; isHuman:bool; isPlaying:bool; isInit:bool}

    val toString : t -> string
end


module Grid :
sig
    type grid = (Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t)
    type t = {grid:grid; isWin:Player.t}

    val newGrid : unit -> t (* t.grid = (Nil, Nil, Nil)  isWin:Nil *)
    val toString : t -> string
    val isWin : t -> t

    val isIllegal : t -> int -> bool
    val setValue :  t -> int -> t
end

type t

val newBoard : int -> int -> t
val print : t -> unit (* toStringBoard *)
val isWin : t -> t

val isIllegal : t -> int -> int -> bool
val setValue : t -> int -> int -> t







