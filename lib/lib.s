BITS 32

section .data
two:
	dd 2.0
half:
	dd 0.5

; あまりにも嘘っぽいが
half_to_int:
	dd 0.4990998


;read_int と read_floatのための。
zero:
	dd 0.000000
ten:
	dd 10.0
iten:
	dd 0.1
one:
	dd 1.0
minusone:
	dd -1.0

section .text

float_of_int: ; int -> float
	fild dword [esp+0x4]
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret


; OCaml .. 0に近い方に切り捨て
; fistp .. 符号を見ずに四捨五入して、符号を戻す
; ので、0.5引いて(負なら足して)、fistpして、戻せばよさそう。

; これ、OCamlと乖離させる。
; またOCamlに合わせ直す
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
	fld dword [half]
	fsubp
	fstp dword [ebp-0x4]
	jmp int_of_float_exact_zero
	
int_of_float_neg:
	fld dword [ebp-0x4]
	fld dword [half]
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



; これもOcamlと乖離させる。
; OCamlに戻します

floor:
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
	mov [esp],eax
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

; ここまで read_int


isnum:
	mov eax,dword [esp+0x4]
	xor ecx,ecx
	cmp eax,0x30
	setl cl
	mov ebx,ecx
	xor ecx,ecx
	cmp eax,0x39
	setg cl
	or ebx,ecx
	xor ebx,1
	mov eax,ebx
	ret

get_until_num:
	call read_char
	cmp eax,0x2d
	je ok_get_until_num
	cmp eax,0x2e
	je ok_get_until_num
	push eax
	call isnum
	mov ebx,eax
	pop eax
	cmp ebx,0
	je retry_get_until_num
ok_get_until_num:
	ret	
retry_get_until_num:
	call get_until_num
	ret


read_float:
	finit
	sub esp,0x4
	call get_until_num
	cmp eax,0x2d
	jne isplus
ismunis:
	fld dword [minusone]
	call read_char
	jmp read_body_float
isplus:
	fld dword [one]
read_body_float:
	fld dword [zero]
read_body_loop:
	; 既にeaxに読み込まれている
	push eax
	call isnum
	mov ebx,eax
	pop eax
	cmp ebx,0
	je read_dot
	; 数字なのでやっていく
	fld dword [ten]
	fmulp
	; top *= 10.0
	sub eax,0x30
	push eax
	fild dword [esp]
	pop eax
	faddp
	; top += readc
	call read_char
	jmp read_body_loop
read_dot:
	cmp eax,0x2e
	jne read_float_finish
	; '.'でなければ、もう終了
	push dword [one]
	fld dword [zero]
read_tenika_loop:
	call read_char
	push eax
	call isnum
	mov ebx,eax
	pop eax
	cmp ebx,0
	je finish_tenika_loop
	
	; 数字なのでやっていく
	sub eax,0x30
	push eax
	fild dword [esp]
	pop ebx
	
	fld dword [esp]
	fld dword [iten]
	fmulp
	fst dword [esp]
	fmulp
	faddp
	jmp read_tenika_loop
finish_tenika_loop:
	faddp
	pop eax
read_float_finish:
	fmulp
	fstp dword [esp]
	mov eax,dword [esp]
	add esp,0x4
	ret






