BITS 32

global int_of_float
global float_of_int
global raise_match_failure
global failwith

global ref
global @ref@set
global @ref@get

global data_eq

extern print_char_err
extern lib_malloc

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
	
failwith:
	dd failwith_p
failwith_p:
	dd raise_match_failure_

ref:
	dd ref_p
ref_p:
	dd ref_
@ref@set:
	dd @ref@set_p
@ref@set_p:
	dd @ref@set_
@ref@get:
	dd @ref@get_p
@ref@get_p:
	dd @ref@get_

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



	
print_string_err:
	push ebp
	mov ebp,esp
	sub esp,8
	mov eax,[ebp+0x8]
	mov [ebp-0x4],eax
	xor ecx,ecx

	test eax,0x20000000
	jz print_string_err_loop
	
	xor eax,0x20000000
	add eax,4
	mov [ebp-0x4],eax

print_string_err_loop:
	mov eax,[ebp-0x4]
	mov cl,[eax]
	inc eax
	mov [ebp-0x4],eax
	mov [ebp-0x8],ecx
	push ecx
	mov edx,dword [print_char_err]
	call [edx]
	add esp,4
	mov ecx,[ebp-0x8]
	cmp ecx,0
	jne print_string_err_loop
	
	mov esp,ebp
	pop ebp
	ret

estr:
	db "match failure at ", 10, 0

raise_match_failure_:
	push estr
	call print_string_err
	add esp,4
	push dword [esp+4]
	call print_string_err
	add esp,4
	int 0x3


not_comparable_str:
	db "UnComparable", 10, 0

type_checker_broken_str:
	db "Invalid Compare(of eax vs. ebx) . Maybe Typechecker is broken", 10, 0

data_eq:
	mov eax,[esp+0x4]
	mov ebx,[esp+0x8]
	
	mov ecx,eax
	xor ecx,ebx
	test ecx,0x70000000
	jz data_eq_valid
	
	push eax
	push ebx
	push type_checker_broken_str
	call print_string_err
	add esp,4
	pop ebx
	pop eax
	int 0x3

data_eq_valid:
	test eax,0x40000000
	jz not_int_float
	cmp eax,ebx
	sete al
	and eax,1
	ret
not_int_float:
	test eax,0x20000000
	; int 0x3
	jz not_string
	xor eax,0x20000000
	xor ebx,0x20000000
	mov ecx,dword [eax]
	mov edx,dword [ebx]
	cmp ecx,edx
	jne ret_neq
	add eax,4
	add ebx,4
eq_len:
	test ecx,ecx
	jz ret_eq
	push ecx
	xor ecx,ecx
	xor edx,edx
	mov cl,byte [eax]
	mov dl,byte [ebx]
	cmp ecx,edx
	pop ecx
	jne ret_neq
	inc eax
	inc ebx
	dec ecx
	jmp eq_len
not_string:
	test eax,0x10000000
	jz not_tuple
	xor eax,0x10000000
	xor ebx,0x10000000
	mov ecx,dword [eax]
	mov edx,dword [ebx]
	cmp ecx,edx
	jne ret_neq
	add eax,4
	add ebx,4
eq_tuple:
	test ecx,ecx
	jz ret_eq
	push eax
	push ebx
	push ecx
	push dword [eax]
	push dword [ebx]
	call data_eq
	add esp,8
	test eax,eax
	pop ecx
	pop ebx
	pop eax
	jz ret_neq
	add eax,4
	add ebx,4
	dec ecx
	jmp eq_tuple
	
not_tuple:
	push not_comparable_str
	call print_string_err
	add esp,4
	int 0x3

ret_eq:
	mov eax,1
	ret
ret_neq:
	mov eax,0
	ret


ref_:
	push dword 1
	call lib_malloc
	add esp,4
	mov ebx,[esp+0x4]
	mov [eax],ebx
	ret

@ref@get_:
	mov eax,[esp+0x4]
	mov eax,[eax]
	ret

@ref@set_:
	mov eax,[esp+0x4]
	mov ebx,[esp+0x8]
	mov [eax],ebx
	ret


	




