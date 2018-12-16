BITS 32


extern String@@
extern Pervasive@string_of_int
extern print_string
extern lib_malloc
global Printf@printf
global Printf@sprintf

section .data
Printf@printf:
	dd Printf@printf_p
Printf@printf_p:
	dd Printf@printf_
Printf@sprintf:
	dd Printf@sprintf_p
Printf@sprintf_p:
	dd Printf@sprintf_


print_save_ret_addr:
	dd 0

section .text


save_regs:
	push eax
	push ebx
	push ecx
	push edx
	push edi
	
	push dword [esp+0x4*5]
	ret

restore_regs:
	pop eax
	mov dword [esp+0x4*5],eax
	
	pop edi
	pop edx
	pop ecx
	pop ebx
	pop eax
	
	ret


; [ebp-0x8] :: format_pos(入力何文字目までformatしたか)

sprintf_function_:
	push ebp
	mov ebp,esp
	sub esp,0x20
	
	mov dword [ebp-0x8],0
	push dword 1
	call lib_malloc
	add esp,4
	mov edx,eax
	mov dword [edx],0
	xor edx,0x20000000
	; edi に文字列へのポインタ(通常の関数クロージャと違って例外的)
	; esp+4 以降に引数
	; が入っている
	mov eax,edi
	xor eax,0x20000000
	mov ecx,dword [eax]
	mov ebx,eax
	add eax,4
conv_loop:
	test ecx,ecx
	jz conv_loop_end
	
	mov bl,byte [eax]
	cmp bl,0x25
	je escaped_char
	
normal_char:
	call save_regs

	push dword 2
	call lib_malloc
	add esp,4
	
	mov dword [eax],1
	mov byte [eax+4],bl
	xor eax,0x20000000
	
	push eax
	push edx
	mov edi,0
	mov eax,[String@@]
	call [eax]
	add esp,8
	
	mov dword [ebp-0x4],eax
	call restore_regs
	mov edx,dword [ebp-0x4]
	
	add eax,1
	sub ecx,1
	jmp conv_loop

escaped_char:
	add eax,1
	mov bl,byte [eax]
	add eax,1
	cmp ecx,1
	je invalid_format_string
	sub ecx,2
	cmp bl,100 ;'d'
	je escape_int
	cmp bl,115 ;'s'
	je escape_string
	jmp invalid_format_string

escape_int:
	mov dword [ebp-0x4],edx
	call save_regs
	
	mov eax,dword [ebp-0x8]
	push dword [ebp+eax+0x8]
	add eax,4
	mov dword [ebp-0x8],eax
	mov edi,0
	mov eax,[Pervasive@string_of_int]
	call [eax]
	add esp,4
	
	mov edx,dword [ebp-0x4]
	push eax
	push edx
	mov edi,0
	mov eax,[String@@]
	call [eax]
	add esp,8
	
	mov dword [ebp-0x4],eax
	call restore_regs
	mov edx,dword [ebp-0x4]
	jmp conv_loop

escape_string:
	call save_regs
	
	mov eax,dword [ebp-0x8]
	push dword [ebp+eax+0x8]
	add eax,4
	mov dword [ebp-0x8],eax
	push edx
	mov edi,0
	mov eax,[String@@]
	call [eax]
	add esp,8
	
	mov dword [ebp-0x4],eax
	call restore_regs
	mov edx,dword [ebp-0x4]
	jmp conv_loop

conv_loop_end:
	mov eax,edx
	mov esp,ebp
	pop ebp
	ret

Printf@sprintf_:
	push dword 2
	call lib_malloc
	add esp,4
	mov dword [eax],sprintf_function_
	mov ebx,[esp+0x4]
	mov dword [eax+4],ebx
	ret

invalid_format_string:
	int 0x3
	ret

printf_function_:
	pop ebx
	mov [print_save_ret_addr],ebx
	call sprintf_function_

	push eax
	mov edi,0
	mov eax,[print_string]
	call [eax]
	add esp,4
	
	mov ebx,[print_save_ret_addr]
	push ebx
	ret

Printf@printf_:
	push dword 2
	call lib_malloc
	add esp,4
	mov dword [eax],printf_function_
	mov ebx,[esp+0x4]
	mov dword [eax+4],ebx
	ret

