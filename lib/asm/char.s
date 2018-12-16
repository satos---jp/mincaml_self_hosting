BITS 32

extern lib_malloc
global Char@code
global Char@chr
global Char@escaped

section .data
Char@code:
	dd Char@code_p
Char@code_p:
	dd Char@code_

Char@chr:
	dd Char@chr_p
Char@chr_p:
	dd Char@chr_

Char@escaped:
	dd Char@escaped_p
Char@escaped_p:
	dd Char@escaped_

section .text

Char@code_:
	mov eax,dword [esp+4]
	ret

Char@chr_:
	mov eax,dword [esp+4]
	ret


; TODO(satos) ちゃんとエスケープする 
Char@escaped_:
	push dword 2
	call lib_malloc
	add esp,4
	
	mov dword [eax],1
	mov bl,byte [esp+4]
	mov byte [eax+4],bl
	xor eax,0x20000000
	ret

