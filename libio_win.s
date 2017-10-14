BITS 32

section .text

extern _putchar
extern _getchar

print_char:
	jmp _putchar

read_char:
	jmp _getchar
