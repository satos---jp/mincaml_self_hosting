BITS 32

global heap_init
global lib_malloc
global register_localheap

extern register_heap_c
extern heap_init_c
extern lib_malloc_c

section .bss
heap1:
;	resb 0x3000000
	resb 0x3f00000
heap1_end:
heap2:
	resb 0x3f00000
;	resb 0x5000000
heap2_end:

section .text
heap_init:
	push esp
	
	mov edx,0
	mov ecx,0x1000
	mov ebx,heap2_end
	sub ebx,0x1000
	and ebx,0xfffff000
	push ebx
	mov eax,125
	int 0x80
	
	push dword heap2

	mov edx,0
	mov ecx,0x1000
	mov ebx,heap1_end
	sub ebx,0x1000
	and ebx,0xfffff000
	push ebx
	mov eax,125
	int 0x80
	
	push dword heap1
	call heap_init_c
	add esp,0x14

	; mov dword [heap_pos],heap1
	
	ret

register_localheap:
	push dword [esp+8]
	push dword [esp+8]
	call register_heap_c
	add esp,8
	ret

lib_malloc:
	push ebx
	push ecx
	push edx
	push edi
	
	; mov eax,dword [heap_pos]
	; mov ecx,eax
	; mov ebx,dword [esp+0x4+0x10]
	; shl ebx,2
	; add ecx,ebx
	; mov dword [heap_pos],ecx
	
	push dword [esp+0x4+0x10]
	call lib_malloc_c
	add esp,4
	
	xor esi,esi ; これ後で消してよい。
	pop edi
	pop edx
	pop ecx
	pop ebx
	ret

