module Player =
struct
    type t = { symbol:char; isHuman:bool; isPlaying:bool; isInit:bool}
end


module Grid =
struct
    type grid = (Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t * Player.t)
    type t = {grid:grid; isWin:Player.t}

end


type t = { grid:Grid.t list; isWin:Player.t; width:int; height:int}

