BITS 32

global int_of_float
global float_of_int
global raise_match_failure

extern print_char_err

section .data
int_of_float:
	dd int_of_float_p
int_of_float_p:
	dd int_of_float_
float_of_int:
	dd float_of_int_p
float_of_int_p:
	dd float_of_int_

raise_match_failure:
	dd raise_match_failure_p
raise_match_failure_p:
	dd raise_match_failure_

two:
	dd 2.0
half:
	dd 0.5

; あまりにも嘘っぽいが
half_to_int:
	dd 0.4990998


;read_int と read_floatのための。
zero:
	dd 0.000000
ten:
	dd 10.0
iten:
	dd 0.1
one:
	dd 1.0
minusone:
	dd -1.0

section .text

;tag系のやつで使うことになる

set_tag_int:
	mov eax,[esp+0x4]
	and eax,0x8fffffff
	or  eax,0x40000000
	ret

unset_tag_int:
	mov eax,[esp+0x4]
	mov edx,0x80000000
	and edx,eax
	shr edx,1
	xor eax,edx
	xor eax,0x40000000
	shr edx,1
	xor eax,edx
	shr edx,1
	xor eax,edx
	ret

set_tag_float:
	mov eax,[esp+0x4]
	ror eax,4
	and eax,0x8fffffff
	or  eax,0x40000000
	ret

unset_tag_float:
	mov eax,[esp+0x4]
	and eax,0x8fffffff
	rol eax,4
	ret

float_of_int_: ; int -> float
	mov eax,dword [esp+0x4]
	push eax
	call unset_tag_int
	add esp,4
	mov dword [esp+4], eax
	
	fild dword [esp+0x4]
	fstp dword [esp-0x4]
	mov eax,[esp-0x4]

	push eax
	call set_tag_float
	add esp,4
	ret


; OCaml .. 0に近い方に切り捨て
; fistp .. 符号を見ずに四捨五入して、符号を戻す
; ので、0.5引いて(負なら足して)、fistpして、戻せばよさそう。

; これ、OCamlと乖離させる。
; またOCamlに合わせ直す
int_of_float_: ; float -> int
	push ebp
	mov ebp,esp
	sub esp,0x4
	mov eax,dword [ebp+0x8]
	
	push eax
	call unset_tag_float
	add esp,0x4

	mov dword [ebp-0x4],eax
	
	push dword [ebp+0x8]
	call fiszero_
	add esp,0x4
	test eax,eax
	jne int_of_float_exact_zero
	
	push dword [ebp+0x8]
	call fisneg_
	add esp,0x4
	test eax,eax
	jne int_of_float_neg

; int_of_float_pos
	fld dword [ebp-0x4]
	fld dword [half]
	fsubp
	fstp dword [ebp-0x4]
	jmp int_of_float_exact_zero
	
int_of_float_neg:
	fld dword [ebp-0x4]
	fld dword [half]
	faddp
	fstp dword [ebp-0x4]

int_of_float_exact_zero:
	fld dword [ebp-0x4]
	fistp dword [esp-0x4]
	mov eax,[esp-0x4]
	
	push eax
	call set_tag_int
	add esp,0x4
	
	add esp,0x4
	pop ebp
	ret

fiszero_:
	xor eax,eax
	fld dword [esp+0x4]
	ftst
	fstsw ax
	fstp st0
	shr eax,14
	and eax,1
	ret

fisneg_:
	xor eax,eax
	fld dword [esp+0x4]
	ftst
	fstsw ax
	fstp st0
	shr eax,8
	and eax,1
	ret


fispos_:
	xor eax,eax
	fld dword [esp+0x4]
	ftst
	fstsw ax
	fstp st0
	mov ebx,eax
	shr eax,14
	shr ebx,8 
	or eax,ebx
	and eax,1
	xor eax,1
	ret



print_hex_err:
	push ebp
	mov ebp,esp
	sub esp,4
	mov dword [ebp-0x4],7
loop:
	mov eax,[ebp-0x4]
	mov ebx,[ebp+0x8]
	mov dl,4
	mul dl
	mov ecx,eax
	mov eax,0xf
	shl eax,cl
	and ebx,eax
	shr ebx,cl
	xor eax,eax
	cmp ebx,10
	setae al
	mov ecx,39
	mul ecx
	add eax,48
	add eax,ebx
	push eax
	call print_char_err
	add esp,4
	mov eax,[ebp-0x4]
	dec eax
	mov [ebp-0x4],eax
	inc eax
	test eax,eax
	jne loop
	
	add esp,4
	pop ebp
	ret


estr:
	db "match failure", 10, 0
	
print_string_err:
	push ebp
	mov ebp,esp
	sub esp,8
	
	mov eax,[ebp+0x8]
	mov [ebp-0x4],eax
	xor ecx,ecx
print_string_err_loop:
	mov eax,[ebp-0x4]
	mov cl,[eax]
	inc eax
	mov [ebp-0x4],eax
	mov [ebp-0x8],ecx
	push ecx
	call print_char_err
	add esp,4
	mov ecx,[ebp-0x8]
	cmp ecx,0
	jne print_string_err_loop
	
	mov esp,ebp
	pop ebp
	ret

raise_match_failure_:
	push estr
	call print_string_err
	add esp,4
	int 0x3



