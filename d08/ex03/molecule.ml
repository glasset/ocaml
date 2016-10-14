type elem = (string * int)

let rec toString lst str = match lst with
        | hd::tl -> if snd hd > 1
        then toString tl (str ^ (fst hd) ^ (string_of_int(snd hd)))
        else toString tl (str ^ (fst hd))
        | [] -> str

let append atom lst =
    try
        (List.remove_assoc atom#getSymbol lst) @ [(atom#getSymbol, (List.assoc atom#getSymbol lst) + 1)]
    with
        | Not_found -> lst @ [(atom#getSymbol, 1)]


let sort (a, b) (d, e) =
    if a = d then
        0
    else if a > d then
        1
    else
        (-1)

let rec genFormula atoms acc = match atoms with
    | hd::tl -> genFormula tl (append hd acc)
    | [] -> (toString (List.sort sort acc) "")


class virtual molecule (name :string) (atoms: Atom.atom list) =
    object (this)
        val _name = name
        val _formula = (genFormula atoms [])

        method getName = _name
        method getFormula = _formula
        method to_string = ("molecule " ^ _name ^ " has formula " ^ _formula ^ ".")
        method equals (that: molecule) = (this#getName = this#getName && this#getFormula = that#getFormula)
    end

class trinitrotoluene =
    object
        inherit molecule "trinitrotoluene" [new Atom.nitrogen; new Atom.nitrogen; new Atom.nitrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon]
    end

class water =
    object
        inherit molecule "water" [new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen]
    end

class carbon_dioxyde =
    object
        inherit molecule "carbon dioxyde" [new Atom.carbon; new Atom.oxygen; new Atom.oxygen]
    end

class nitric_oxide =
    object
        inherit molecule "nitric oxide" [new Atom.nitrogen; new Atom.oxygen]
    end

class nitrogen_monohydride =
    object
        inherit molecule "nitrogen monohydride" [new Atom.nitrogen; new Atom.hydrogen]
    end

class pentynylidyne =
    object
        inherit molecule "pentynylidyne" [new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.nitrogen]
    end

class methane =
    object
        inherit molecule "methane" [new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;new Atom.carbon]
    end

class octane =
    object
        inherit molecule "octane" [new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;]
    end

class ethane =
    object
        inherit molecule "ethane" [new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.carbon; new Atom.carbon]
    end



