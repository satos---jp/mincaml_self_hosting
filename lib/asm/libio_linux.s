BITS 32

global print_char
global read_char
global print_char_err
global puts_err
global print_string

section .data
print_char:
	dd print_char_p
print_char_p:
	dd print_char_

read_char:
	dd read_char_p
read_char_p:
	dd read_char_

print_char_err:
	dd print_char_err_p
print_char_err_p:
	dd print_char_err_

puts_err:
	dd puts_err_p
puts_err_p:
	dd puts_err_

print_string:
	dd print_string_p
print_string_p:
	dd print_string_

section .text

print_char_:
	mov edx,1
	lea ecx,[esp+0x4]
	mov ebx,1
	mov eax,4
	int 0x80
	ret

; eofなら-1を返すようにする
read_char_:
	mov edx,1
	lea ecx,[esp-0x4]
	mov ebx,0
	mov eax,3
	int 0x80
	test eax,eax
	jz ret_eof
	mov eax,[esp-0x4]
	and eax,0xff
	or eax,0x40000000
	ret
ret_eof:
	mov eax,-1
	and eax,0x8fffffff
	or eax,0x40000000
	ret

print_char_err_:
	mov edx,1
	lea ecx,[esp+0x4]
	mov ebx,2
	mov eax,4
	int 0x80
	ret

puts_err_:
	mov edx,dword [esp+0x8]
	mov ecx,dword [esp+0x4]
	mov ebx,2
	mov eax,4
	int 0x80
	ret


print_string_:
	mov eax,dword [esp+0x4]
	xor eax,0x20000000
	mov edx,dword [eax]
	mov ecx,eax
	add ecx,4
	mov ebx,1
	mov eax,4
	int 0x80
	ret

