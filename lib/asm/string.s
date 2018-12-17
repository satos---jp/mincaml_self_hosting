BITS 32

extern raise_match_failure
extern lib_malloc
extern gen_string_malloc_c

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

@invalid_get_error:
	db "invalid String@get at", 0

section .text

String@@_:
	mov ebx,dword [esp+4]
	xor ebx,0x20000000
	mov ecx,dword [esp+8]
	xor ecx,0x20000000
	mov eax,dword [ebx]
	mov edx,dword [ecx]
	add edx,eax
	
	mov eax,edx
	sar eax,2
	add eax,2
	push eax
	mov dword [gen_string_malloc_c],1 ; TODO(satos) これ消したい気持ちはある
	call lib_malloc
	add esp,4
	push eax ; TODO(satos) これ忘れると手前でセグフォるのにゃーんなのでもっときれいに書き直す
	
	mov dword [eax],edx
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
	jnz raise_Invalid_argument_get
	cmp ebx,ecx
	jge raise_Invalid_argument_get
	add eax,4
	add eax,ebx
	mov al,byte [eax]
	and eax,0xff
	xor eax,0x40000000
	ret

raise_Invalid_argument_get:
	push dword @invalid_get_error
	mov eax,dword [raise_match_failure+0]
	mov edi,[eax+4]
	call [eax]




