BITS 32

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


; TODO(satos) ちゃんとエスケープする / heapから取る際に関数呼ぶようにする
Char@escaped_:
	push esi
	mov dword [esi],1
	add esi,4
	mov al,byte [esp+8]
	mov byte [esi],al
	add esi,4
	pop eax
	xor eax,0x20000000
	ret

