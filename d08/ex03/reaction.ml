
let sort a b =
    if a#getFormula = b#getFormula then 0
    else if a#getFormula > b#getFormula then
        1
    else
        (-1)

class virtual reaction start result=
    object (this)
    val _start = List.sort sort start
    val _result = List.sort sort result

    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method is_balanced = if this#get_start = this#get_result then true else false
    end


