module type Watchtower =
sig
type hour = int
val zero : hour
val add : hour -> hour -> hour
val sub : hour -> hour -> hour
end

module Watchtower =
	struct
		type hour = int
		let zero = 12
		let add (h1:hour) (h2:hour) =
			let res =  (h1 + h2) mod zero in
				if res = 0 then zero
				else res
		let sub (h1:hour) (h2:hour) =
            let res = (h1 - h2) mod zero in
				if res = 0 then zero
				else if res < 0 then res * (-1)
				else res
	end

let () =
    let hour =  Watchtower.zero in
        let w1 = Watchtower.add hour 3 in
        let w2 = Watchtower.sub w1 11 in
        let w3 = Watchtower.sub 7 w2 in
        let w4 = Watchtower.add w3 4 in
        print_endline "12 + 3";
            print_endline (string_of_int w1);
        print_endline "w1 - 11";
            print_endline (string_of_int w2);
        print_endline "7 - w2";
            print_endline (string_of_int w3);
        print_endline "w3 + 4";
            print_endline (string_of_int w4)

