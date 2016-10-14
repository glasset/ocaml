
let rec genCarbon n acc =
    if n = 0 then
        acc
    else
        genCarbon (n - 1) (acc @ [new Atom.carbon])

let rec genHydrogen n acc =
    if n = 0 then
        acc
    else
        genHydrogen (n - 1) (acc @ [new Atom.hydrogen])

let genName n str = match n with
    | 1 -> "meth" ^ str
    | 2 -> "eth" ^ str
    | 3 -> "prop" ^ str
    | 4 -> "but" ^ str
    | 5 -> "pent" ^ str
    | 6 -> "hex" ^ str
    | 7 -> "hept" ^ str
    | 8 -> "oct" ^ str
    | 9 -> "non" ^ str
    | 10 -> "dec" ^ str
    | 11 -> "undec" ^ str
    | 12 -> "dodec" ^ str
    | _ -> "unknown"


class alkane (n: int) =
    object
        inherit Molecule.molecule (genName n "ane") ((genCarbon n []) @ (genHydrogen (2 * n + 2) []))
    end


class methane =
    object
        inherit alkane 1
    end

class ethane =
    object
        inherit alkane 2
    end

class octane =
    object
        inherit alkane 8
    end

class undecane =
    object
        inherit alkane 11
    end

class nonane =
    object
        inherit alkane 9
    end



