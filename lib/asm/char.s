BITS 32

global Char@code

section .data
Char@code:
	dd Char@code_p
Char@code_p:
	dd Char@code_

section .text

Char@code_:
	mov eax,dword [esp+4]
	ret

