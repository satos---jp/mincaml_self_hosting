BITS 32

; nasm tes.s -f elf32 -g -o tes.o; gcc -m32 tes.o

section .data
hoge:
	dd 3.14

huga:
	db "Hello world" 
	db 0xa

piyo:
	dd 0.00

one:
	dd 1.0

two:
	dd 2.0


section .text
	
global _start
_start:
	fistp dword [piyo]
	fld dword [one]
	fld dword [two]
	xor ecx,ecx
	fucomip
	seta cl

	fld dword [two]
	fld dword [one]
	xor ecx,ecx
	fucomip
	seta cl
	hlt
	
	mov edx,12
	mov ecx,huga
	mov ebx,1
	mov eax,4
	int 0x80
	finit
	fld dword [hoge]
	fstp dword [ebp+0x8]
	
	inc eax
	add byte [hoge],dl
	
	fstp st0
	push dword [ebp+0x8]
	call [ebp+0x8]
	mov eax,[hoge]
	mov eax,[edi+0x10]
	mov [edi-0x10],eax
	mov [ebp+0x8],eax
	add esp,0x10
	pop ebp
	ret

