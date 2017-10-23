BITS 32

section .text

extern _putchar
extern _getchar
extern _write

print_char:
	jmp _putchar

read_char:
	jmp _getchar

print_char_err:
	push ebp
	mov ebp,esp
	push dword 1
	lea eax,[ebp+0x8]
	push eax
	push dword 2
	call _write
	add esp,12
	pop ebp
	ret
