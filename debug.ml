type debug_data = string * int * int

let data2str (fname,st,gl) = 
	let ic = open_in fname in
	Printf.printf "%d %d\n" st gl;
	let nl = ref 0 in
	let bnl = ref (-1) in
	let ln = ref 0 in
	let _ = seek_in ic 0 in
	while (!nl) < st do
		let cl = String.length (input_line ic) in
		bnl := !nl;
		nl := (!nl) + cl + 1;
		ln := (!ln) + 1
	done;
	let nld = (!nl) - st + 1 in
	let s1 = Printf.sprintf "file %s line %d char %d-%d " fname (if nld == 1 then (!ln)+1 else (!ln)) (st - (!bnl) + 1) (gl - (!bnl) + 1) in
	let _ = seek_in ic st in
	let rec f np res = 
		if np == gl then res else
			f (np+1) (res ^ Char.escaped (input_char ic))
	in
	let s2 = f st "" in
		s1 ^ s2
