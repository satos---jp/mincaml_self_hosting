

float_of_int:
	lw r5,r1,$0
	itof f1,r5
	fst f1,r1,$-4
	lw r5,r1,$-4
	ret

int_of_float:
	fld f1,r1,$0
	ftoi r5,f1
	ret

sqrt:
	fld f1,r1,$0
	fsqrt f2,f1
	fst f2,r1,$-4
	lw r5,r1,$-4
	ret

; itof,ftoiは
; 負、正とともに小数点切り捨てだが、
; floor は、負のほうは切り捨てする。

@a_228dbl:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$4
@cfg_label_475:
	fld f1,r2,$8
	fld f2,r2,$8
	fadd f1,f1,f2
	fst f1,r2,$-4
; ../tes.ml@1:16;1:22 ::= Ofadd ../tes.ml@1:16;1:17 ../tes.ml@1:21;1:22
	lw r5,r2,$-4
	addi r1,r1,$4
	pop r2
	pop r6
	jr r6
@a_264iloop:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$76
@cfg_label_474:
@cfg_label_442:
	li r5,$0
	sw r5,r2,$-4
; ../tes.ml@10:25;10:26
	lw r5,r2,$8
	lw r6,r2,$-4
	bne r5,r6,@cfg_label_470
	j @cfg_label_469
	j @cfg_label_471
@cfg_label_470:
	fld f1,r2,$-12
	fld f2,r2,$-16
	fsub f1,f1,f2
	fst f1,r2,$-8
; ../tes.ml@11:27;11:37 ::= Ofsub ../tes.ml@11:27;11:30 ../tes.ml@11:34;11:37
	fld f1,r2,$-8
	fld f2,r2,$28
	fadd f1,f1,f2
	fst f1,r2,$-20
; ../tes.ml@11:27;11:43 ::= Ofadd ../tes.ml@11:27;11:37 ../tes.ml@11:41;11:43
	push r4
	lw r5,r2,$12
	push r5
	jal @a_228dbl
	sw r5,r2,$-24
	addi r1,r1,$4
	pop r4
; ../tes.ml@12:27;12:33 ../tes.ml@12:27;12:30
	fld f1,r2,$-24
	fld f2,r2,$-32
	fmul f1,f1,f2
	fst f1,r2,$-28
; ../tes.ml@12:27;12:39 ::= Ofmul ../tes.ml@12:27;12:33 ../tes.ml@12:37;12:39
	fld f1,r2,$-28
	fld f2,r2,$32
	fadd f1,f1,f2
	fst f1,r2,$-32
; ../tes.ml@12:27;12:45 ::= Ofadd ../tes.ml@12:27;12:39 ../tes.ml@12:43;12:45
	fld f1,r2,$-20
	fld f2,r2,$-20
	fmul f1,f1,f2
	fst f1,r2,$-12
; ../tes.ml@15:28;15:36 ::= Ofmul ../tes.ml@15:28;15:30 ../tes.ml@15:34;15:36
	fld f1,r2,$-32
	fld f2,r2,$-32
	fmul f1,f1,f2
	fst f1,r2,$-16
; ../tes.ml@16:28;16:36 ::= Ofmul ../tes.ml@16:28;16:30 ../tes.ml@16:34;16:36
	fmovi f1,$4.000000000000000000000000000000
	fst f1,r2,$-36
	fld f1,r2,$-12
	fld f2,r2,$-16
	fadd f1,f1,f2
	fst f1,r2,$-40
; ../tes.ml@17:37;17:47 ::= Ofadd ../tes.ml@17:37;17:40 ../tes.ml@17:44;17:47
	fld f1,r2,$-36
	fld f2,r2,$-40
	flt f1,f2
	bft @emit_label_476
	li r5,$0
	j @emit_label_477
@emit_label_476:
	li r5,$1
@emit_label_477:
	sw r5,r2,$-44
; ../tes.ml@17:21;17:48 ::= Olt ../tes.ml@17:22;17:32 ../tes.ml@17:37;17:47
	li r5,$1
	sw r5,r2,$-48
; ../tes.ml@17:21;17:48
	lw r5,r2,$-44
	lw r6,r2,$-48
	bne r5,r6,@cfg_label_455
	j @cfg_label_454
	j @cfg_label_456
@cfg_label_455:
	li r5,$1
	sw r5,r2,$-52
; ../tes.ml@18:29;18:30
	lw r6,r2,$-52
	lw r5,r2,$8
	sub r5,r5,r6
	sw r5,r2,$-56
; ../tes.ml@18:25;18:30 ::= Osub ../tes.ml@18:25;18:26 ../tes.ml@18:29;18:30
	lw r5,r2,$28
	sw r5,r2,$-60
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@18:46;18:48
	lw r5,r2,$32
	sw r5,r2,$-64
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@18:49;18:51
	lw r5,r2,$-56
	sw r5,r2,$8
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@10:18;18:51
	lw r5,r2,$-20
	sw r5,r2,$12
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@10:18;18:51
	lw r5,r2,$-32
	sw r5,r2,$16
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@10:18;18:51
	lw r5,r2,$-12
	sw r5,r2,$20
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@10:18;18:51
	lw r5,r2,$-16
	sw r5,r2,$24
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@10:18;18:51
	lw r5,r2,$-60
	sw r5,r2,$28
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@10:18;18:51
	lw r5,r2,$-64
	sw r5,r2,$32
; ../tes.ml@10:18;18:51 ::<= ../tes.ml@10:18;18:51
	j @cfg_label_442
	j @cfg_label_474
@cfg_label_456:
@cfg_label_454:
	li r5,$0
	sw r5,r2,$-68
; ../tes.ml@17:64;17:65
	push r4
	lw r5,r2,$-68
	push r5
	jal print_int
	sw r5,r2,$-72
	addi r1,r1,$4
	pop r4
; ../tes.ml@10:18;18:51 ../tes.ml@17:54;17:63
	lw r5,r2,$-72
	addi r1,r1,$76
	pop r2
	pop r6
	jr r6
@cfg_label_471:
@cfg_label_469:
	li r5,$1
	sw r5,r2,$-76
; ../tes.ml@10:42;10:43
	push r4
	lw r5,r2,$-76
	push r5
	jal print_int
	sw r5,r2,$-72
	addi r1,r1,$4
	pop r4
; ../tes.ml@10:18;18:51 ../tes.ml@10:32;10:41
	lw r5,r2,$-72
	addi r1,r1,$76
	pop r2
	pop r6
	jr r6
@a_236xloop:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$108
@cfg_label_441:
@cfg_label_409:
	li r5,$2
	sw r5,r2,$-4
; ../tes.ml@5:12;5:13
	lw r6,r2,$-4
	lw r5,r2,$8
	slt r5,r5,r6
	li r6,$1
	xor r5,r5,r6
	sw r5,r2,$-8
; ../tes.ml@5:7;5:13 ::= Ogeq ../tes.ml@5:7;5:8 ../tes.ml@5:12;5:13
	li r5,$1
	sw r5,r2,$-12
; ../tes.ml@5:7;5:13
	lw r5,r2,$-8
	lw r6,r2,$-12
	bne r5,r6,@cfg_label_435
	j @cfg_label_434
	j @cfg_label_436
	j @cfg_label_437
@cfg_label_436:
@cfg_label_434:
	sw r3,r2,$-16
	addi r3,r3,$0
; ../tes.ml@5:4;20:21
	lw r5,r2,$-16
	addi r1,r1,$108
	pop r2
	pop r6
	jr r6
@cfg_label_437:
@cfg_label_435:
	push r4
	lw r5,r2,$8
	push r5
	jal print_int
	sw r5,r2,$-20
	addi r1,r1,$4
	pop r4
; ../tes.ml@6:7;6:18 ../tes.ml@6:7;6:16
	li r5,$32
	sw r5,r2,$-24
; ../tes.ml@6:31;6:33
	push r4
	lw r5,r2,$-24
	push r5
	jal print_char
	sw r5,r2,$-28
	addi r1,r1,$4
	pop r4
; ../tes.ml@6:20;6:33 ../tes.ml@6:20;6:30
	push r4
	lw r5,r2,$12
	push r5
	jal print_int
	sw r5,r2,$-32
	addi r1,r1,$4
	pop r4
; ../tes.ml@6:35;6:46 ../tes.ml@6:35;6:44
	li r5,$10
	sw r5,r2,$-36
; ../tes.ml@6:59;6:61
	push r4
	lw r5,r2,$-36
	push r5
	jal print_char
	sw r5,r2,$-40
	addi r1,r1,$4
	pop r4
; ../tes.ml@6:48;6:61 ../tes.ml@6:48;6:58
	push r4
	lw r5,r2,$8
	push r5
	jal float_of_int
	sw r5,r2,$-44
	addi r1,r1,$4
	pop r4
; ../tes.ml@7:30;7:44 ../tes.ml@7:30;7:42
	push r4
	lw r5,r2,$-44
	push r5
	jal @a_228dbl
	sw r5,r2,$-48
	addi r1,r1,$4
	pop r4
; ../tes.ml@7:25;7:45 ../tes.ml@7:25;7:28
	fmovi f1,$4.000000000000000000000000000000
	fst f1,r2,$-52
	fld f1,r2,$-48
	fld f2,r2,$-52
	fdiv f1,f1,f2
	fst f1,r2,$-56
; ../tes.ml@7:25;7:52 ::= Ofdiv ../tes.ml@7:25;7:45 ../tes.ml@7:49;7:52
	push r4
	lw r5,r2,$12
	push r5
	jal float_of_int
	sw r5,r2,$-60
	addi r1,r1,$4
	pop r4
; ../tes.ml@8:30;8:44 ../tes.ml@8:30;8:42
	push r4
	lw r5,r2,$-60
	push r5
	jal @a_228dbl
	sw r5,r2,$-64
	addi r1,r1,$4
	pop r4
; ../tes.ml@8:25;8:45 ../tes.ml@8:25;8:28
	fmovi f1,$4.000000000000000000000000000000
	fst f1,r2,$-68
	fld f1,r2,$-64
	fld f2,r2,$-68
	fdiv f1,f1,f2
	fst f1,r2,$-72
; ../tes.ml@8:25;8:52 ::= Ofdiv ../tes.ml@8:25;8:45 ../tes.ml@8:49;8:52
	li r5,$1000
	sw r5,r2,$-76
; ../tes.ml@19:22;19:26
	fmovi f1,$0.000000000000000000000000000000
	fst f1,r2,$-80
	fmovi f1,$0.000000000000000000000000000000
	fst f1,r2,$-84
	fmovi f1,$0.000000000000000000000000000000
	fst f1,r2,$-88
	fmovi f1,$0.000000000000000000000000000000
	fst f1,r2,$-92
	push r4
	lw r5,r2,$-72
	push r5
	lw r5,r2,$-56
	push r5
	lw r5,r2,$-92
	push r5
	lw r5,r2,$-88
	push r5
	lw r5,r2,$-84
	push r5
	lw r5,r2,$-80
	push r5
	lw r5,r2,$-76
	push r5
	jal @a_264iloop
	sw r5,r2,$-96
	addi r1,r1,$28
	pop r4
; ../tes.ml@19:16;19:48 ../tes.ml@19:16;19:21
	li r5,$1
	sw r5,r2,$-100
; ../tes.ml@20:16;20:17
	lw r6,r2,$-100
	lw r5,r2,$8
	add r5,r5,r6
	sw r5,r2,$-104
; ../tes.ml@20:12;20:17 ::= Oadd ../tes.ml@20:12;20:13 ../tes.ml@20:16;20:17
	lw r5,r2,$12
	sw r5,r2,$-108
; ../tes.ml@5:4;20:21 ::<= ../tes.ml@20:19;20:20
	lw r5,r2,$-104
	sw r5,r2,$8
; ../tes.ml@5:4;20:21 ::<= ../tes.ml@5:4;20:21
	lw r5,r2,$-108
	sw r5,r2,$12
; ../tes.ml@5:4;20:21 ::<= ../tes.ml@5:4;20:21
	j @cfg_label_409
	j @cfg_label_441
@a_230yloop:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$40
@cfg_label_408:
@cfg_label_393:
	li r5,$2
	sw r5,r2,$-4
; ../tes.ml@3:9;3:10
	lw r6,r2,$-4
	lw r5,r2,$8
	slt r5,r5,r6
	li r6,$1
	xor r5,r5,r6
	sw r5,r2,$-8
; ../tes.ml@3:4;3:10 ::= Ogeq ../tes.ml@3:4;3:5 ../tes.ml@3:9;3:10
	li r5,$1
	sw r5,r2,$-12
; ../tes.ml@3:4;3:10
	lw r5,r2,$-8
	lw r6,r2,$-12
	bne r5,r6,@cfg_label_402
	j @cfg_label_401
	j @cfg_label_403
	j @cfg_label_404
@cfg_label_403:
@cfg_label_401:
	sw r3,r2,$-16
	addi r3,r3,$0
; ../tes.ml@3:1;23:15
	lw r5,r2,$-16
	addi r1,r1,$40
	pop r2
	pop r6
	jr r6
@cfg_label_404:
@cfg_label_402:
	li r5,$0
	sw r5,r2,$-20
; ../tes.ml@21:9;21:10
	push r4
	lw r5,r2,$8
	push r5
	lw r5,r2,$-20
	push r5
	jal @a_236xloop
	sw r5,r2,$-24
	addi r1,r1,$8
	pop r4
; ../tes.ml@21:3;21:12 ../tes.ml@21:3;21:8
	li r5,$10
	sw r5,r2,$-28
; ../tes.ml@22:12;22:14
	push r4
	lw r5,r2,$-28
	push r5
	jal print_char
	sw r5,r2,$-32
	addi r1,r1,$4
	pop r4
; ../tes.ml@22:1;22:14 ../tes.ml@22:1;22:11
	li r5,$1
	sw r5,r2,$-36
; ../tes.ml@23:13;23:14
	lw r6,r2,$-36
	lw r5,r2,$8
	add r5,r5,r6
	sw r5,r2,$-40
; ../tes.ml@23:9;23:14 ::= Oadd ../tes.ml@23:9;23:10 ../tes.ml@23:13;23:14
	lw r5,r2,$-40
	sw r5,r2,$8
; ../tes.ml@3:1;23:15 ::<= ../tes.ml@3:1;23:15
	j @cfg_label_393
	j @cfg_label_408
main:
	mflr r7
	push r7
	mov r31,r3
	addi r3,r3,$4
	push r2
	mov r2,r1
	subi r1,r1,$4
@cfg_label_391:
	li r5,$0
	sw r5,r31,$0
; ../tes.ml@24:6;24:7
	push r4
	lw r5,r31,$0
	push r5
	jal @a_230yloop
	sw r5,r2,$-4
	addi r1,r1,$4
	pop r4
; @0:-1;0:-1 ../tes.ml@24:0;24:5
	li r5,$0
	hlt
