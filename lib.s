BITS 32

section .text
	
global print_char
print_char:
	push ebp
	mov ebp,esp
	mov edx,1
	lea ecx,[ebp+0x8]
	mov ebx,1
	mov eax,4
	int 0x80
	pop ebp
	ret


