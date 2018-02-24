@a_16777365mod_:
	push ebp
	mov ebp,esp
	sub esp,12
	mov ebx,dword [ebp+12]
	mov eax,dword [ebp+8]
	xor edx,edx
	div ebx
	mov dword [ebp-8],eax
; lib.ml@2:22;2:25 ::= Odiv lib.ml@2:22;2:23 lib.ml@2:24;2:25
	mov ebx,dword [ebp+12]
	mov eax,dword [ebp-8]
	mul ebx
	mov dword [ebp-4],eax
; lib.ml@2:21;2:28 ::= Omul lib.ml@2:22;2:25 lib.ml@2:27;2:28
	mov ebx,dword [ebp-4]
	mov eax,dword [ebp+8]
	sub eax,ebx
	mov dword [ebp-12],eax
; lib.ml@2:19;2:28 ::= Osub lib.ml@2:19;2:20 lib.ml@2:21;2:28
	mov eax,dword [ebp-12]
	add esp,12
	pop ebp
	pop ebx
	add esp,8
	push ebx
	ret
@a_16777368print_int_base:
	push ebp
	mov ebp,esp
	sub esp,68
	mov dword [ebp-64],10
; lib.ml@4:8;4:10
	mov ebx,dword [ebp-64]
	mov eax,dword [ebp+8]
	xor ecx,ecx
	cmp eax,ebx
	setl cl
	mov eax,ecx
	mov dword [ebp-60],eax
; lib.ml@4:4;4:10 ::= Olt lib.ml@4:4;4:5 lib.ml@4:8;4:10
	mov dword [ebp-56],1
; lib.ml@4:4;4:10
	mov eax,dword [ebp-60]
	mov ebx,dword [ebp-56]
	cmp eax,ebx
	jne @virtual_label_16777487
	mov dword [ebp-52],0
; lib.ml@4:20;4:21
	mov ebx,dword [ebp+8]
	mov eax,dword [ebp-52]
	xor ecx,ecx
	cmp eax,ebx
	setl cl
	mov eax,ecx
	mov dword [ebp-48],eax
; lib.ml@4:20;4:25 ::= Olt lib.ml@4:20;4:21 lib.ml@4:24;4:25
	mov dword [ebp-44],1
; lib.ml@4:20;4:25
	mov eax,dword [ebp-48]
	mov ebx,dword [ebp-44]
	cmp eax,ebx
	jne @virtual_label_16777489
	mov dword [ebp-40],48
; lib.ml@4:45;4:47
	mov ebx,dword [ebp-40]
	mov eax,dword [ebp+8]
	add eax,ebx
	mov dword [ebp-36],eax
; lib.ml@4:43;4:47 ::= Oadd lib.ml@4:43;4:44 lib.ml@4:45;4:47
	push edi
	push dword dword [ebp-36]
	call print_char
	mov dword [ebp-68],eax
	add esp,4
	pop edi
; lib.ml@4:1;6:30 lib.ml@4:31;4:41
; indir : nontail
	mov eax,dword [ebp-68]
	add esp,68
	pop ebp
	pop ebx
	add esp,4
	push ebx
	ret
@virtual_label_16777489:
	mov dword [ebp-68],esi
	add esi,0
; lib.ml@4:1;6:30
@virtual_label_16777490:
	mov eax,dword [ebp-68]
	add esp,68
	pop ebp
	pop ebx
	add esp,4
	push ebx
	ret
@virtual_label_16777487:
	mov dword [ebp-32],10
; lib.ml@5:21;5:23
	mov ebx,dword [ebp-32]
	mov eax,dword [ebp+8]
	xor edx,edx
	div ebx
	mov dword [ebp-28],eax
; lib.ml@5:19;5:23 ::= Odiv lib.ml@5:19;5:20 lib.ml@5:21;5:23
	push edi
	push dword dword [ebp-28]
	call @a_16777368print_int_base
	mov dword [ebp-8],eax
	pop edi
; lib.ml@5:3;5:24 lib.ml@5:3;5:17
; dir : nontail
	mov dword [ebp-24],10
; lib.ml@6:22;6:24
	push edi
	push dword dword [ebp-24]
	push dword dword [ebp+8]
	call @a_16777365mod_
	mov dword [ebp-20],eax
	pop edi
; lib.ml@6:15;6:24 lib.ml@6:15;6:19
; dir : nontail
	mov dword [ebp-16],48
; lib.ml@6:26;6:28
	mov ebx,dword [ebp-16]
	mov eax,dword [ebp-20]
	add eax,ebx
	mov dword [ebp-12],eax
; lib.ml@6:14;6:28 ::= Oadd lib.ml@6:15;6:24 lib.ml@6:26;6:28
	push edi
	push dword dword [ebp-12]
	call print_char
	mov dword [ebp-4],eax
	add esp,4
	pop edi
; lib.ml@6:2;6:29 lib.ml@6:2;6:12
; indir : nontail
	mov eax,dword [ebp-4]
	mov dword [ebp-68],eax
; lib.ml@4:1;6:30 ::<= lib.ml@6:2;6:29
@virtual_label_16777488:
	mov eax,dword [ebp-68]
	add esp,68
	pop ebp
	pop ebx
	add esp,4
	push ebx
	ret
print_int:
	push ebp
	mov ebp,esp
	sub esp,44
	mov dword [ebp-40],0
; lib.ml@9:8;9:9
	mov eax,dword [ebp+8]
	mov ebx,dword [ebp-40]
	cmp eax,ebx
	jne @virtual_label_16777483
	mov dword [ebp-36],48
; lib.ml@9:26;9:28
	push edi
	push dword dword [ebp-36]
	call print_char
	mov dword [ebp-44],eax
	add esp,4
	pop edi
; lib.ml@9:1;10:79 lib.ml@9:15;9:25
; indir : nontail
	mov eax,dword [ebp-44]
	add esp,44
	pop ebp
	pop ebx
	add esp,4
	push ebx
	ret
@virtual_label_16777483:
	mov dword [ebp-32],0
; lib.ml@10:14;10:15
	mov ebx,dword [ebp-32]
	mov eax,dword [ebp+8]
	xor ecx,ecx
	cmp eax,ebx
	setl cl
	mov eax,ecx
	mov dword [ebp-28],eax
; lib.ml@10:10;10:15 ::= Olt lib.ml@10:10;10:11 lib.ml@10:14;10:15
	mov dword [ebp-24],1
; lib.ml@10:10;10:15
	mov eax,dword [ebp-28]
	mov ebx,dword [ebp-24]
	cmp eax,ebx
	jne @virtual_label_16777485
	mov dword [ebp-20],45
; lib.ml@10:32;10:34
	push edi
	push dword dword [ebp-20]
	call print_char
	mov dword [ebp-8],eax
	add esp,4
	pop edi
; lib.ml@10:21;10:34 lib.ml@10:21;10:31
; indir : nontail
	mov dword [ebp-16],0
; lib.ml@10:52;10:53
	mov ebx,dword [ebp+8]
	mov eax,dword [ebp-16]
	sub eax,ebx
	mov dword [ebp-12],eax
; lib.ml@10:52;10:55 ::= Osub lib.ml@10:52;10:53 lib.ml@10:54;10:55
	push edi
	push dword dword [ebp-12]
	call @a_16777368print_int_base
	mov dword [ebp-4],eax
	pop edi
; lib.ml@10:36;10:56 lib.ml@10:36;10:50
; dir : nontail
	mov eax,dword [ebp-4]
	mov dword [ebp-44],eax
; lib.ml@9:1;10:79 ::<= lib.ml@10:36;10:56
	mov eax,dword [ebp-44]
	add esp,44
	pop ebp
	pop ebx
	add esp,4
	push ebx
	ret
@virtual_label_16777485:
	push edi
	push dword dword [ebp+8]
	call @a_16777368print_int_base
	mov dword [ebp-44],eax
	pop edi
; lib.ml@9:1;10:79 lib.ml@10:62;10:76
; dir : nontail
@virtual_label_16777486:
@virtual_label_16777484:
	mov eax,dword [ebp-44]
	add esp,44
	pop ebp
	pop ebx
	add esp,4
	push ebx
	ret

