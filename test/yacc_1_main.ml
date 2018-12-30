let buf = Lexing.from_channel stdin


let rec main _ =
  let v = Yacc_1.toplevel Yacc_1_lex.main buf in
	print_int v;
	print_string "\n";
	if v < 0 then () else main ()

let _ = main ()

