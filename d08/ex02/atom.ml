class virtual atom (name: string) (symbol: string) (atomic_number: int) =
    object (this)
        val _name = name
        val _symbol = symbol
        val _atomic_number = atomic_number

        method getName = _name
        method getSymbol = _symbol
        method getAtomic_number = _atomic_number
        method to_string = ("atom " ^ _name ^ " has symbol " ^ _symbol ^ " and number " ^ (string_of_int _atomic_number) ^ ".")
        method equals (that: atom) = (this#getName = this#getName && this#getSymbol = that#getSymbol && this#getAtomic_number = that#getAtomic_number)
    end


class hydrogen =
    object
        inherit atom "hydrogen" "H" 1
    end

class carbon =
    object
        inherit atom "carbon" "C" 6
    end

class oxygen =
    object
        inherit atom "oxygen" "O" 8
    end

class nitrogen =
    object
        inherit atom "nitrogen" "N" 7
    end

class titanium =
    object
        inherit atom "titanium" "Ti" 22
    end

class neon =
    object
        inherit atom "neon" "Ne" 10
    end

class calcium =
    object
        inherit atom "calcium" "Ca" 20
    end

class cobalt =
    object
        inherit atom "cobalt" "Co" 27
    end

class arsenic =
    object
        inherit atom "arsenic" "As" 33
    end
