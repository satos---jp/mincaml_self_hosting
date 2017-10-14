BITS 32

section .data
two:
	dd 2.0
half:
	dd 0.5

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
	fsqrt
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]
	ret


sqrt:
	fld dword [esp+0x4]
	fsqrt
	fistp dword [esp-0x4]
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

floor:
	push dword [esp+0x4]
	call int_of_float
	add esp,4
	push eax
	call float_of_int
	add esp,4
	ret
