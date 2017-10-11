open Lexing

type debug_data = string * Lexing.position * Lexing.position

let filename = ref ""

let default_debug_data = ("",Lexing.dummy_pos,Lexing.dummy_pos)
let get_debug_data () = (!filename, (Parsing.symbol_start_pos ()), (Parsing.symbol_end_pos ()))


(*
   0x457793 <floor+352991>:     mov    eax,DWORD PTR [ebp-0x170]
   0x457799 <floor+352997>:     mov    ebx,DWORD PTR [ebp-0x16c]
   0x45779f <floor+353003>:     mov    ecx,DWORD PTR [ebp-0x168] //-360
   0x4577a5 <floor+353009>:     mov    DWORD PTR [eax+ebx*4],ecx
   0x4577a8 <floor+353012>:     mov    DWORD PTR [ebp-0x18],eax
   0x4577ab <floor+353015>:     mov    eax,DWORD PTR [ebp-0x164]
   0x4577b1 <floor+353021>:     mov    DWORD PTR [ebp-0x158],eax
   0x4577b7 <floor+353027>:     mov    DWORD PTR [ebp-0x154],0x1
   0x4577c1 <floor+353037>:     mov    eax,DWORD PTR [ebp+0xc]
   0x4577c4 <floor+353040>:     mov    DWORD PTR [ebp-0x160],eax
   
   
   これは、元のソースの
    image_center.(0) <- size_x / 2;

@t_44_image_center
は
@k_75 に代入されている。

 であるが、これはebp-368. == 0x170. のはず。
 
109097		mov eax,dword [ebp-356]
109098		mov dword [ebp-368],eax

109112		mov eax,dword [ebp-368]
109113		mov ebx,dword [ebp-364]
109114		mov ecx,dword [ebp-360]
109115		mov dword [eax+4*ebx],ecx
109116		mov dword [ebp-24],eax
109117		mov eax,dword [ebp-356]
109118		mov dword [ebp-344],eax
109119	; ../raytracer/min-rt.ml@2292:1;2292:13../raytracer/min-rt.ml@2292:1;2292:13
109120		mov dword [ebp-340],1
109121	; ../raytracer/min-rt.ml@2292:15;2292:16
109122		mov eax,dword [ebp+12]

このへん、で、

108554	@t_44_image_center :: [ebp-356]

つってて、あー、外部ほげかな。



*)

let pos2str p = Printf.sprintf "%d:%d" p.pos_lnum (p.pos_cnum - p.pos_bol)

let debug_data2simple (fname,st,gl) = Printf.sprintf "%s@%s;%s" fname (pos2str st) (pos2str gl)

let debug_data2str (fname,st,gl) = Printf.sprintf "file %s %s -> %s " fname (pos2str st) (pos2str gl)
(*
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
*)
