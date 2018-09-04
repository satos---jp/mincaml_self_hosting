BITS 32

global print_char
global read_char
global print_char_err
global puts_err

section .data
print_char:
	dd print_char_p
print_char_p:
	dd print_char_

section .text

print_char_:
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
	mov ebx,2
	mov eax,4
	int 0x80
	ret

puts_err:
	mov edx,dword [esp+0x8]
	mov ecx,dword [esp+0x4]
	mov ebx,2
	mov eax,4
	int 0x80
	ret

