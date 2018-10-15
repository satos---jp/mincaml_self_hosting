BITS 32

global String@@
global String@length
global String@get

section .data
String@@:
	dd String@@_p
String@@_p:
	dd String@@_
String@length:
	dd String@length_p
String@length_p:
	dd String@length_
String@get:
	dd String@get_p
String@get_p:
	dd String@get_

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


String@length_:
	mov eax,dword [esp+4]
	xor eax,0x20000000
	mov eax,dword [eax]
	xor eax,0x40000000
	ret


String@get_:
	mov eax,dword [esp+4]
	xor eax,0x20000000
	mov ebx,dword [esp+8]
	xor ebx,0x40000000
	mov ecx,dword [eax]
	test ebx,0x80000000
	jnz raise_Invalid_argument
	cmp ebx,ecx
	jge raise_Invalid_argument
	add eax,4
	add eax,ebx
	mov al,byte [eax]
	and eax,0xff
	xor eax,0x40000000
	ret

raise_Invalid_argument:
	int 0x3





