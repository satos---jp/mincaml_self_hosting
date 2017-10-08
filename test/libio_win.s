BITS 32

section .text

extern  _putchar


print_char:
	jmp _putchar

read_char:
	hlt
