BITS 32

global String@@

section .data
String@@:
	dd String@@_p
String@@_p:
	dd String@@_


section .text

; TODO ヒープから取ってくる際に、ちゃんと関数を呼ぶようにする
String@@_:
	mov ebx,dword [esp+4]
	xor ebx,0x20000000
	mov ecx,dword [esp+8]
	xor ecx,0x20000000
	mov eax,dword [ebx]
	mov edx,dword [ecx]
	add edx,eax
	
	push esi
	mov eax,esi
	mov dword [eax],edx
	add esi,edx
	add esi,4
	
	add eax,4
	
	mov ebx,dword [esp+8]
	xor ebx,0x20000000
	mov ecx,dword [ebx]
	add ebx,4
concat_loop_1:
	test ecx,ecx
	jz concat_loop_end_1
	mov dl, byte [ebx]
	mov byte [eax], dl
	inc eax
	inc ebx
	dec ecx
	jmp concat_loop_1
concat_loop_end_1:

	mov ebx,dword [esp+0xc]
	xor ebx,0x20000000
	mov ecx,dword [ebx]
	add ebx,4
concat_loop_2:
	test ecx,ecx
	jz concat_loop_end_2
	mov dl, byte [ebx]
	mov byte [eax], dl
	inc eax
	inc ebx
	dec ecx
	jmp concat_loop_2
concat_loop_end_2:

	pop eax
	xor eax,0x20000000
	ret









