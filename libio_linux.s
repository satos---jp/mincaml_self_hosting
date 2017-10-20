BITS 32

section .text

print_char:
	mov edx,1
	lea ecx,[esp+0x4]
	mov ebx,1
	mov eax,4
	int 0x80
	ret

read_char:
	mov edx,1
	lea ecx,[esp-0x4]
	mov ebx,0
	mov eax,3
	int 0x80
	mov eax,[esp-0x4]
	and eax,0xff
	ret

print_char_err:
	mov edx,1
	lea ecx,[esp+0x4]
	mov ebx,1
	mov eax,4
	int 0x80
	ret

