BITS 32

global heap_init
global lib_malloc

section .bss
heap:
	resb 0x4000000
heap_end:

section .data
heap_pos:
	dd 0

section .text
heap_init:
	mov edx,0
	mov ecx,0x1000
	mov ebx,heap_end
	sub ebx,0x1000
	and ebx,0xfffff000
	mov eax,125
	int 0x80
	mov dword [heap_pos],heap
	ret

lib_malloc:
	push ebx
	push ecx
	push edx
	push edi
	
	mov eax,dword [heap_pos]
	mov ecx,eax
	mov ebx,dword [esp+0x4+0x10]
	shl ebx,2
	add ecx,ebx
	mov dword [heap_pos],ecx
	
	xor esi,esi ; これ後で消してよい。
	pop edi
	pop edx
	pop ecx
	pop ebx
	ret

