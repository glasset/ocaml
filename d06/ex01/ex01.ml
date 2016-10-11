module DatHash =
 struct
	type t = string
	let equal (a:t) (b:t) = if a = b then true else false
	let hash (a:t) =
		let rec loop i key =
			if i >= String.length a then
				key
			else
			    loop (i + 1) ((3 * key + int_of_char(String.get a i)) mod 128)
		in loop 0 0
 end

module StringHashtbl = Hashtbl.Make (DatHash)


let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
