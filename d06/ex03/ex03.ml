module type FIXED = sig
type t
val of_float : float -> t
val of_int : int -> t
val to_float : t -> float
val to_int : t -> int
val to_string : t -> string
val zero : t
val one : t
val succ : t -> t
val pred : t -> t
val min : t -> t -> t
val max : t -> t -> t
val gth : t -> t -> bool
val lth : t -> t -> bool
val gte : t -> t -> bool
val lte : t -> t -> bool
val eqp : t -> t -> bool (** physical equality *)
val eqs : t -> t -> bool (** structural equality *)
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig val bits : int end

module type MAKE =
	functor (Fract : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
	functor (Fract : FRACTIONNAL_BITS) ->
		struct
			type t = int
			let of_int x = x lsl Fract.bits
			let of_float x =  int_of_float (x *. (2. ** float_of_int(Fract.bits)))
			let to_float t = (float_of_int t) /. (2. ** float_of_int(Fract.bits))
			let to_int t = t lsr Fract.bits
			let to_string t = string_of_float (to_float t)
			let zero = of_int 0
			let one = of_int 1
			let succ t = t + 1
			let pred t = t - 1
			let min a b = if a <= b then a else b
			let max a b = if a >= b then a else b
			let gth a b = if a > b then true else false
			let lth a b = if a < b then true else false
			let gte a b = if a >= b then true else false
			let lte a b = if a <= b then true else false
			let eqp a b = if a == b then true else false
			let eqs a b = if a = b then true else false
			let add a b = a + b
			let sub a b = a - b
			let mul a b = a * b
			let div a b = a / b
			let foreach a b c =
					let rec loop i x =
						if i > x then ()
						else begin c i;loop (i + 1) x end
					in loop a b
		end



module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
let x8 = Fixed8.of_float 21.10 in
let y8 = Fixed8.of_float 21.32 in
let r8 = Fixed8.add x8 y8 in
print_endline (Fixed8.to_string r8);
Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
