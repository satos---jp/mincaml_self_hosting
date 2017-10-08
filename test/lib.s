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

int_of_float: ; float -> int
	fld dword [esp+0x4]
	fist dword [esp-0x4]
	mov eax,[esp-0x4]
	ret

fless:
	xor eax,eax
	fld dword [esp+0x4]
	fld dword [esp+0x8]
	fcompp
	fstsw ax
	shr eax,8 
	and eax,1
	ret

fiszero:
	xor eax,eax
	fld dword [esp+0x4]
	ftst
	fstsw ax
	shr eax,14
	and eax,1
	ret

fisneg:
	xor eax,eax
	fld dword [esp+0x4]
	ftst
	fstsw ax
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
	fist dword [esp-0x4]
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
	fld dword [esp+0x4]
	fld dword [half]
	fadd
	fist dword [esp-0x4]
	fild dword [esp-0x4]
	mov eax,[esp-0x4]
	ret
	
