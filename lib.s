BITS 32

section .data
two:
	dd 2.0
half:
	dd 0.5

; あまりにも嘘っぽいが
half_to_int:
	dd 0.4999998


;read_int と read_floatのための。
@const_1000000321:
	dd 0.100000
@const_1000000322:
	dd 10.000000
@const_1000000323:
	dd 0.100000
@const_1000000324:
	dd 0.000000
@const_1000000325:
	dd 0.000000

section .text

float_of_int: ; int -> float
	fild dword [esp+0x4]
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret


; OCaml .. 0に近い方に切り捨て
; fistp .. 符号を見ずに四捨五入して、符号を戻す
; ので、0.5引いて(負なら足して)、fistpして、戻せばよさそう。
int_of_float: ; float -> int
	push ebp
	mov ebp,esp
	sub esp,0x4
	mov eax,dword [ebp+0x8]
	mov dword [ebp-0x4],eax
	
	push dword [ebp+0x8]
	call fiszero
	add esp,0x4
	test eax,eax
	jne int_of_float_exact_zero
	
	push dword [ebp+0x8]
	call fisneg
	add esp,0x4
	test eax,eax
	jne int_of_float_neg

; int_of_float_pos
	fld dword [ebp-0x4]
	fld dword [half_to_int]
	fsubp
	fstp dword [ebp-0x4]
	jmp int_of_float_exact_zero
	
int_of_float_neg:
	fld dword [ebp-0x4]
	fld dword [half_to_int]
	faddp
	fstp dword [ebp-0x4]

int_of_float_exact_zero:
	fld dword [ebp-0x4]
	fistp dword [esp-0x4]
	mov eax,[esp-0x4]
	
	add esp,0x4
	pop ebp
	ret

fless:
	xor eax,eax
	fld dword [esp+0x8]
	fld dword [esp+0x4]
	fcompp
	fstsw ax
	fstp st0
	shr eax,8 
	and eax,1
	ret

fiszero:
	xor eax,eax
	fld dword [esp+0x4]
	ftst
	fstsw ax
	fstp st0
	shr eax,14
	and eax,1
	ret

fisneg:
	xor eax,eax
	fld dword [esp+0x4]
	ftst
	fstsw ax
	fstp st0
	shr eax,8
	and eax,1
	ret

fhalf:
	fld dword [esp+0x4]
	fld dword [half]
	fmulp
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret	

fispos:
	xor eax,eax
	fld dword [esp+0x4]
	ftst
	fstsw ax
	fstp st0
	mov ebx,eax
	shr eax,14
	shr ebx,8 
	or eax,ebx
	and eax,1
	xor eax,1
	ret


fneg:
	fld dword [esp+0x4]
	fchs
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret

fsqr:
	fld dword [esp+0x4]
	fld dword [esp+0x4]
	fmulp
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret

sqrt:
	fld dword [esp+0x4]
	fsqrt
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret

cos:
	fld dword [esp+0x4]
	fcos
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret

sin:
	fld dword [esp+0x4]
	fsin
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret

atan:
	fld dword [esp+0x4]
	fld1
	fpatan
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret

fabs:
	fld dword [esp+0x4]
	fabs
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret

;
;
floor:
;とりあえず、壊れたままで
	push dword [esp+0x4]
	call int_of_float
	add esp,4
	push eax
	call float_of_int
	add esp,4
	ret

	push dword [esp+0x4]
	call fisneg
	add esp,4
	test eax,eax
	jne floor_neg
	push dword [esp+0x4]
	call int_of_float
	add esp,4
	push eax
	call float_of_int
	add esp,4
	ret
floor_neg:
	push dword [esp+0x4]
	call fneg
	add esp,4
	push eax
	call floor
	add esp,4
	
	mov [esp-0x4],eax
	fld dword [esp+0x4]
	fld dword [esp-0x4]
	faddp
	fstp dword [esp-0x4]
	sub esp,4
	call fiszero
	add esp,4
	test eax,eax
	jne floor_not_addone
floor_addone:
	fld dword [esp+0x4]
	fld1
	fsubp
	fstp dword [esp+0x4]

floor_not_addone:
	push dword [esp+0x4]
	call fneg
	mov [esp],eax
	call floor
	mov [esp],eax
	call fneg
	add esp,4
	ret



print_hex_err:
	push ebp
	mov ebp,esp
	sub esp,4
	mov dword [ebp-0x4],7
loop:
	mov eax,[ebp-0x4]
	mov ebx,[ebp+0x8]
	mov dl,4
	mul dl
	mov ecx,eax
	mov eax,0xf
	shl eax,cl
	and ebx,eax
	shr ebx,cl
	xor eax,eax
	cmp ebx,10
	setae al
	mov ecx,39
	mul ecx
	add eax,48
	add eax,ebx
	push eax
	call print_char_err
	add esp,4
	mov eax,[ebp-0x4]
	dec eax
	mov [ebp-0x4],eax
	inc eax
	test eax,eax
	jne loop
	
	add esp,4
	pop ebp
	ret







@a_1000000253f:
	push ebp
	mov ebp,esp
	sub esp,60
	push edi
	call read_char
	mov dword [ebp-20],eax
	add esp,0
	pop edi
; ../complib.ml@3:10;3:22 ../complib.ml@3:10;3:19
	mov dword [ebp-56],47
; ../complib.ml@4:9;4:11
	mov ebx,dword [ebp-56]
	mov eax,dword [ebp-20]
	sub eax,ebx
	mov dword [ebp-48],eax
; ../complib.ml@4:7;4:11 ::= Osub ../complib.ml@4:7;4:8 ../complib.ml@4:9;4:11
	mov dword [ebp-52],58
; ../complib.ml@4:14;4:16
	mov ebx,dword [ebp-20]
	mov eax,dword [ebp-52]
	sub eax,ebx
	mov dword [ebp-44],eax
; ../complib.ml@4:14;4:18 ::= Osub ../complib.ml@4:14;4:16 ../complib.ml@4:17;4:18
	mov ebx,dword [ebp-44]
	mov eax,dword [ebp-48]
	mul ebx
	mov dword [ebp-40],eax
; ../complib.ml@4:6;4:19 ::= Omul ../complib.ml@4:7;4:11 ../complib.ml@4:14;4:18
	mov dword [ebp-36],0
; ../complib.ml@4:20;4:21
	mov ebx,dword [ebp-36]
	mov eax,dword [ebp-40]
	xor ecx,ecx
	cmp eax,ebx
	setg cl
	mov eax,ecx
	mov dword [ebp-32],eax
; ../complib.ml@4:6;4:21 ::= Ogt ../complib.ml@4:6;4:19 ../complib.ml@4:20;4:21
	mov dword [ebp-28],1
; ../complib.ml@4:6;4:21
	mov eax,dword [ebp-32]
	mov ebx,dword [ebp-28]
	cmp eax,ebx
	jne @virtual_label_1000000319
	mov dword [ebp-24],10
; ../complib.ml@4:34;4:36
	mov ebx,dword [ebp-24]
	mov eax,dword [ebp+8]
	mul ebx
	mov dword [ebp-12],eax
; ../complib.ml@4:30;4:36 ::= Omul ../complib.ml@4:30;4:33 ../complib.ml@4:34;4:36
	mov dword [ebp-16],48
; ../complib.ml@4:40;4:42
	mov ebx,dword [ebp-16]
	mov eax,dword [ebp-20]
	sub eax,ebx
	mov dword [ebp-8],eax
; ../complib.ml@4:38;4:42 ::= Osub ../complib.ml@4:38;4:39 ../complib.ml@4:40;4:42
	mov ebx,dword [ebp-8]
	mov eax,dword [ebp-12]
	add eax,ebx
	mov dword [ebp-4],eax
; ../complib.ml@4:30;4:43 ::= Oadd ../complib.ml@4:30;4:36 ../complib.ml@4:38;4:42
	push edi
	push dword dword [ebp-4]
	call @a_1000000253f
	mov dword [ebp-60],eax
	add esp,4
	pop edi
; ../complib.ml@3:2;4:53 ../complib.ml@4:27;4:28
	jmp @virtual_label_1000000320
@virtual_label_1000000319:
	mov eax,dword [ebp+8]
	mov dword [ebp-60],eax
; ../complib.ml@3:2;4:53 ::<= ../complib.ml@4:50;4:53
@virtual_label_1000000320:
	mov eax,dword [ebp-60]
	add esp,60
	pop ebp
	ret
read_int:
	push ebp
	mov ebp,esp
	sub esp,60
	push edi
	call read_char
	mov dword [ebp-12],eax
	add esp,0
	pop edi
; ../complib.ml@6:10;6:22 ../complib.ml@6:10;6:19
	mov dword [ebp-56],45
; ../complib.ml@7:10;7:12
	mov eax,dword [ebp-12]
	mov ebx,dword [ebp-56]
	cmp eax,ebx
	jne @virtual_label_1000000315
	mov dword [ebp-52],0
; ../complib.ml@7:22;7:23
	push edi
	push dword dword [ebp-52]
	call @a_1000000253f
	mov dword [ebp-48],eax
	add esp,4
	pop edi
; ../complib.ml@7:20;7:23 ../complib.ml@7:20;7:21
	mov eax,dword [ebp-48]
	neg eax
	mov dword [ebp-60],eax
; ../complib.ml@2:1;8:53 ::= Ominus ../complib.ml@7:20;7:23
	jmp @virtual_label_1000000316
@virtual_label_1000000315:
	mov dword [ebp-44],47
; ../complib.ml@8:10;8:12
	mov ebx,dword [ebp-44]
	mov eax,dword [ebp-12]
	sub eax,ebx
	mov dword [ebp-36],eax
; ../complib.ml@8:8;8:12 ::= Osub ../complib.ml@8:8;8:9 ../complib.ml@8:10;8:12
	mov dword [ebp-40],58
; ../complib.ml@8:15;8:17
	mov ebx,dword [ebp-12]
	mov eax,dword [ebp-40]
	sub eax,ebx
	mov dword [ebp-32],eax
; ../complib.ml@8:15;8:19 ::= Osub ../complib.ml@8:15;8:17 ../complib.ml@8:18;8:19
	mov ebx,dword [ebp-32]
	mov eax,dword [ebp-36]
	mul ebx
	mov dword [ebp-28],eax
; ../complib.ml@8:7;8:20 ::= Omul ../complib.ml@8:8;8:12 ../complib.ml@8:15;8:19
	mov dword [ebp-24],0
; ../complib.ml@8:21;8:22
	mov ebx,dword [ebp-24]
	mov eax,dword [ebp-28]
	xor ecx,ecx
	cmp eax,ebx
	setg cl
	mov eax,ecx
	mov dword [ebp-20],eax
; ../complib.ml@8:7;8:22 ::= Ogt ../complib.ml@8:7;8:20 ../complib.ml@8:21;8:22
	mov dword [ebp-16],1
; ../complib.ml@8:7;8:22
	mov eax,dword [ebp-20]
	mov ebx,dword [ebp-16]
	cmp eax,ebx
	jne @virtual_label_1000000317
	mov dword [ebp-8],48
; ../complib.ml@8:33;8:35
	mov ebx,dword [ebp-8]
	mov eax,dword [ebp-12]
	sub eax,ebx
	mov dword [ebp-4],eax
; ../complib.ml@8:31;8:35 ::= Osub ../complib.ml@8:31;8:32 ../complib.ml@8:33;8:35
	push edi
	push dword dword [ebp-4]
	call @a_1000000253f
	mov dword [ebp-60],eax
	add esp,4
	pop edi
; ../complib.ml@2:1;8:53 ../complib.ml@8:28;8:29
	jmp @virtual_label_1000000318
@virtual_label_1000000317:
	push edi
	call read_int
	mov dword [ebp-60],eax
	add esp,0
	pop edi
; ../complib.ml@2:1;8:53 ../complib.ml@8:42;8:50
@virtual_label_1000000318:
@virtual_label_1000000316:
	mov eax,dword [ebp-60]
	add esp,60
	pop ebp
	ret
@a_1000000167g:
	push ebp
	mov ebp,esp
	sub esp,68
	push edi
	call read_char
	mov dword [ebp-28],eax
	add esp,0
	pop edi
; ../complib.ml@12:10;12:22 ../complib.ml@12:10;12:19
	mov dword [ebp-64],47
; ../complib.ml@13:9;13:11
	mov ebx,dword [ebp-64]
	mov eax,dword [ebp-28]
	sub eax,ebx
	mov dword [ebp-56],eax
; ../complib.ml@13:7;13:11 ::= Osub ../complib.ml@13:7;13:8 ../complib.ml@13:9;13:11
	mov dword [ebp-60],58
; ../complib.ml@13:14;13:16
	mov ebx,dword [ebp-28]
	mov eax,dword [ebp-60]
	sub eax,ebx
	mov dword [ebp-52],eax
; ../complib.ml@13:14;13:18 ::= Osub ../complib.ml@13:14;13:16 ../complib.ml@13:17;13:18
	mov ebx,dword [ebp-52]
	mov eax,dword [ebp-56]
	mul ebx
	mov dword [ebp-48],eax
; ../complib.ml@13:6;13:19 ::= Omul ../complib.ml@13:7;13:11 ../complib.ml@13:14;13:18
	mov dword [ebp-44],0
; ../complib.ml@13:20;13:21
	mov ebx,dword [ebp-44]
	mov eax,dword [ebp-48]
	xor ecx,ecx
	cmp eax,ebx
	setg cl
	mov eax,ecx
	mov dword [ebp-40],eax
; ../complib.ml@13:6;13:21 ::= Ogt ../complib.ml@13:6;13:19 ../complib.ml@13:20;13:21
	mov dword [ebp-36],1
; ../complib.ml@13:6;13:21
	mov eax,dword [ebp-40]
	mov ebx,dword [ebp-36]
	cmp eax,ebx
	jne @virtual_label_1000000313
	mov eax,[@const_1000000321]
	mov dword [ebp-32],eax
	fld dword [ebp+8]
	fld dword [ebp-32]
	fmulp
	fstp dword [ebp-8]
; ../complib.ml@13:30;13:38 ::= Ofmul ../complib.ml@13:30;13:31 ../complib.ml@13:35;13:38
	mov dword [ebp-24],48
; ../complib.ml@13:66;13:68
	mov ebx,dword [ebp-24]
	mov eax,dword [ebp-28]
	sub eax,ebx
	mov dword [ebp-20],eax
; ../complib.ml@13:64;13:68 ::= Osub ../complib.ml@13:64;13:65 ../complib.ml@13:66;13:68
	push edi
	push dword dword [ebp-20]
	call float_of_int
	mov dword [ebp-16],eax
	add esp,4
	pop edi
; ../complib.ml@13:50;13:69 ../complib.ml@13:50;13:62
	fld dword [ebp+8]
	fld dword [ebp-16]
	fmulp
	fstp dword [ebp-12]
; ../complib.ml@13:46;13:70 ::= Ofmul ../complib.ml@13:46;13:47 ../complib.ml@13:50;13:69
	fld dword [ebp+12]
	fld dword [ebp-12]
	faddp
	fstp dword [ebp-4]
; ../complib.ml@13:41;13:70 ::= Ofadd ../complib.ml@13:41;13:44 ../complib.ml@13:46;13:70
	push edi
	push dword dword [ebp-4]
	push dword dword [ebp-8]
	call @a_1000000167g
	mov dword [ebp-68],eax
	add esp,8
	pop edi
; ../complib.ml@12:2;13:80 ../complib.ml@13:27;13:28
	jmp @virtual_label_1000000314
@virtual_label_1000000313:
	mov eax,dword [ebp+12]
	mov dword [ebp-68],eax
; ../complib.ml@12:2;13:80 ::<= ../complib.ml@13:77;13:80
@virtual_label_1000000314:
	mov eax,dword [ebp-68]
	add esp,68
	pop ebp
	ret
@a_1000000170f:
	push ebp
	mov ebp,esp
	sub esp,72
	push edi
	call read_char
	mov dword [ebp-12],eax
	add esp,0
	pop edi
; ../complib.ml@16:10;16:22 ../complib.ml@16:10;16:19
	mov dword [ebp-68],47
; ../complib.ml@17:9;17:11
	mov ebx,dword [ebp-68]
	mov eax,dword [ebp-12]
	sub eax,ebx
	mov dword [ebp-60],eax
; ../complib.ml@17:7;17:11 ::= Osub ../complib.ml@17:7;17:8 ../complib.ml@17:9;17:11
	mov dword [ebp-64],58
; ../complib.ml@17:14;17:16
	mov ebx,dword [ebp-12]
	mov eax,dword [ebp-64]
	sub eax,ebx
	mov dword [ebp-56],eax
; ../complib.ml@17:14;17:18 ::= Osub ../complib.ml@17:14;17:16 ../complib.ml@17:17;17:18
	mov ebx,dword [ebp-56]
	mov eax,dword [ebp-60]
	mul ebx
	mov dword [ebp-52],eax
; ../complib.ml@17:6;17:19 ::= Omul ../complib.ml@17:7;17:11 ../complib.ml@17:14;17:18
	mov dword [ebp-48],0
; ../complib.ml@17:20;17:21
	mov ebx,dword [ebp-48]
	mov eax,dword [ebp-52]
	xor ecx,ecx
	cmp eax,ebx
	setg cl
	mov eax,ecx
	mov dword [ebp-44],eax
; ../complib.ml@17:6;17:21 ::= Ogt ../complib.ml@17:6;17:19 ../complib.ml@17:20;17:21
	mov dword [ebp-40],1
; ../complib.ml@17:6;17:21
	mov eax,dword [ebp-44]
	mov ebx,dword [ebp-40]
	cmp eax,ebx
	jne @virtual_label_1000000309
	mov eax,[@const_1000000322]
	mov dword [ebp-36],eax
	fld dword [ebp+8]
	fld dword [ebp-36]
	fmulp
	fstp dword [ebp-24]
; ../complib.ml@17:30;17:39 ::= Ofmul ../complib.ml@17:30;17:33 ../complib.ml@17:35;17:39
	mov dword [ebp-32],48
; ../complib.ml@17:58;17:60
	mov ebx,dword [ebp-32]
	mov eax,dword [ebp-12]
	sub eax,ebx
	mov dword [ebp-28],eax
; ../complib.ml@17:56;17:60 ::= Osub ../complib.ml@17:56;17:57 ../complib.ml@17:58;17:60
	push edi
	push dword dword [ebp-28]
	call float_of_int
	mov dword [ebp-20],eax
	add esp,4
	pop edi
; ../complib.ml@17:42;17:61 ../complib.ml@17:42;17:54
	fld dword [ebp-24]
	fld dword [ebp-20]
	faddp
	fstp dword [ebp-16]
; ../complib.ml@17:30;17:62 ::= Ofadd ../complib.ml@17:30;17:39 ../complib.ml@17:42;17:61
	push edi
	push dword dword [ebp-16]
	call @a_1000000170f
	mov dword [ebp-72],eax
	add esp,4
	pop edi
; ../complib.ml@16:2;18:38 ../complib.ml@17:27;17:28
	jmp @virtual_label_1000000310
@virtual_label_1000000309:
	mov dword [ebp-8],46
; ../complib.ml@18:11;18:13
	mov eax,dword [ebp-12]
	mov ebx,dword [ebp-8]
	cmp eax,ebx
	jne @virtual_label_1000000311
	mov eax,[@const_1000000323]
	mov dword [ebp-4],eax
	push edi
	push dword dword [ebp+8]
	push dword dword [ebp-4]
	call @a_1000000167g
	mov dword [ebp-72],eax
	add esp,8
	pop edi
; ../complib.ml@16:2;18:38 ../complib.ml@18:19;18:20
	jmp @virtual_label_1000000312
@virtual_label_1000000311:
	mov eax,dword [ebp+8]
	mov dword [ebp-72],eax
; ../complib.ml@16:2;18:38 ::<= ../complib.ml@18:34;18:37
@virtual_label_1000000312:
@virtual_label_1000000310:
	mov eax,dword [ebp-72]
	add esp,72
	pop ebp
	ret
read_float:
	push ebp
	mov ebp,esp
	sub esp,68
	push edi
	call read_char
	mov dword [ebp-16],eax
	add esp,0
	pop edi
; ../complib.ml@20:10;20:22 ../complib.ml@20:10;20:19
	mov dword [ebp-64],45
; ../complib.ml@21:10;21:12
	mov eax,dword [ebp-16]
	mov ebx,dword [ebp-64]
	cmp eax,ebx
	jne @virtual_label_1000000305
	mov eax,[@const_1000000324]
	mov dword [ebp-56],eax
	mov eax,[@const_1000000325]
	mov dword [ebp-60],eax
	push edi
	push dword dword [ebp-60]
	call @a_1000000170f
	mov dword [ebp-52],eax
	add esp,4
	pop edi
; ../complib.ml@21:26;21:31 ../complib.ml@21:26;21:27
	fld dword [ebp-56]
	fld dword [ebp-52]
	fsubp
	fstp dword [ebp-68]
; ../complib.ml@11:1;22:70 ::= Ofsub ../complib.ml@21:19;21:22 ../complib.ml@21:26;21:31
	jmp @virtual_label_1000000306
@virtual_label_1000000305:
	mov dword [ebp-48],47
; ../complib.ml@22:10;22:12
	mov ebx,dword [ebp-48]
	mov eax,dword [ebp-16]
	sub eax,ebx
	mov dword [ebp-40],eax
; ../complib.ml@22:8;22:12 ::= Osub ../complib.ml@22:8;22:9 ../complib.ml@22:10;22:12
	mov dword [ebp-44],58
; ../complib.ml@22:15;22:17
	mov ebx,dword [ebp-16]
	mov eax,dword [ebp-44]
	sub eax,ebx
	mov dword [ebp-36],eax
; ../complib.ml@22:15;22:19 ::= Osub ../complib.ml@22:15;22:17 ../complib.ml@22:18;22:19
	mov ebx,dword [ebp-36]
	mov eax,dword [ebp-40]
	mul ebx
	mov dword [ebp-32],eax
; ../complib.ml@22:7;22:20 ::= Omul ../complib.ml@22:8;22:12 ../complib.ml@22:15;22:19
	mov dword [ebp-28],0
; ../complib.ml@22:21;22:22
	mov ebx,dword [ebp-28]
	mov eax,dword [ebp-32]
	xor ecx,ecx
	cmp eax,ebx
	setg cl
	mov eax,ecx
	mov dword [ebp-24],eax
; ../complib.ml@22:7;22:22 ::= Ogt ../complib.ml@22:7;22:20 ../complib.ml@22:21;22:22
	mov dword [ebp-20],1
; ../complib.ml@22:7;22:22
	mov eax,dword [ebp-24]
	mov ebx,dword [ebp-20]
	cmp eax,ebx
	jne @virtual_label_1000000307
	mov dword [ebp-12],48
; ../complib.ml@22:47;22:49
	mov ebx,dword [ebp-12]
	mov eax,dword [ebp-16]
	sub eax,ebx
	mov dword [ebp-8],eax
; ../complib.ml@22:45;22:49 ::= Osub ../complib.ml@22:45;22:46 ../complib.ml@22:47;22:49
	push edi
	push dword dword [ebp-8]
	call float_of_int
	mov dword [ebp-4],eax
	add esp,4
	pop edi
; ../complib.ml@22:31;22:50 ../complib.ml@22:31;22:43
	push edi
	push dword dword [ebp-4]
	call @a_1000000170f
	mov dword [ebp-68],eax
	add esp,4
	pop edi
; ../complib.ml@11:1;22:70 ../complib.ml@22:28;22:29
	jmp @virtual_label_1000000308
@virtual_label_1000000307:
	push edi
	call read_float
	mov dword [ebp-68],eax
	add esp,0
	pop edi
; ../complib.ml@11:1;22:70 ../complib.ml@22:57;22:67
@virtual_label_1000000308:
@virtual_label_1000000306:
	mov eax,dword [ebp-68]
	add esp,68
	pop ebp
	ret

	

