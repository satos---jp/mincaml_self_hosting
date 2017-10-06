BITS 32

section .text
	
print_char:
	mov edx,1
	lea ecx,[esp+0x4]
	mov ebx,1
	mov eax,4
	int 0x80
	ret

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

