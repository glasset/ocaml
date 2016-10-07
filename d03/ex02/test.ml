
let main () =
	let s1 = (Cipher.rot42 "salut")
	in print_string s1;print_char '\n';
	print_string (Uncipher.unrot42 s1);print_char '\n';
	let s2 = (Cipher.caesar 10 "salut")
	in print_string s2;print_char '\n';
	print_string (Uncipher.uncaesar 10 s2);print_char '\n';
	let s3 = (Cipher.xor 2 "ab")
	in print_string s3;print_char '\n';
	print_string (Cipher.xor 2  s3);print_char '\n';

	let s4 = (Cipher.ft_crypt "salutimstring" [Cipher.xor 10; Cipher.caesar 8])
	in print_string s4; print_char '\n';
	print_string (Uncipher.ft_uncrypt s4 [Uncipher.uncaesar 8; Cipher.xor 10]); print_char '\n'


let () = main ()
