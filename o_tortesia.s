

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

@a_190fless:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$4
@cfg_label_445:
	fld f1,r2,$8
	fld f2,r2,$12
	flt f1,f2
	bft @emit_label_446
	li r5,$0
	j @emit_label_447
@emit_label_446:
	li r5,$1
@emit_label_447:
	sw r5,r2,$-4
; lib_tortesia.ml@3:20;3:25 ::= Olt lib_tortesia.ml@3:20;3:21 lib_tortesia.ml@3:24;3:25
	lw r5,r2,$-4
	addi r1,r1,$4
	pop r2
	pop r6
	jr r6
@a_209dbl:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$4
@cfg_label_444:
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
@a_237iloop:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$76
@cfg_label_443:
@cfg_label_411:
	li r5,$0
	sw r5,r2,$-4
; ../tes.ml@9:25;9:26
	lw r5,r2,$8
	lw r6,r2,$-4
	bne r5,r6,@cfg_label_439
	j @cfg_label_438
	j @cfg_label_440
@cfg_label_439:
	fld f1,r2,$20
	fld f2,r2,$24
	fsub f1,f1,f2
	fst f1,r2,$-8
; ../tes.ml@10:27;10:37 ::= Ofsub ../tes.ml@10:27;10:30 ../tes.ml@10:34;10:37
	fld f1,r2,$-8
	fld f2,r2,$28
	fadd f1,f1,f2
	fst f1,r2,$-12
; ../tes.ml@10:27;10:43 ::= Ofadd ../tes.ml@10:27;10:37 ../tes.ml@10:41;10:43
	push r4
	lw r5,r2,$12
	push r5
	jal @a_209dbl
	sw r5,r2,$-16
	addi r1,r1,$4
	pop r4
; ../tes.ml@11:27;11:33 ../tes.ml@11:27;11:30
	fld f1,r2,$-16
	fld f2,r2,$16
	fmul f1,f1,f2
	fst f1,r2,$-20
; ../tes.ml@11:27;11:39 ::= Ofmul ../tes.ml@11:27;11:33 ../tes.ml@11:37;11:39
	fld f1,r2,$-20
	fld f2,r2,$32
	fadd f1,f1,f2
	fst f1,r2,$-24
; ../tes.ml@11:27;11:45 ::= Ofadd ../tes.ml@11:27;11:39 ../tes.ml@11:43;11:45
	fld f1,r2,$-12
	fld f2,r2,$-12
	fmul f1,f1,f2
	fst f1,r2,$-28
; ../tes.ml@14:28;14:36 ::= Ofmul ../tes.ml@14:28;14:30 ../tes.ml@14:34;14:36
	fld f1,r2,$-24
	fld f2,r2,$-24
	fmul f1,f1,f2
	fst f1,r2,$-32
; ../tes.ml@15:28;15:36 ::= Ofmul ../tes.ml@15:28;15:30 ../tes.ml@15:34;15:36
	fmovi f1,$4.000000000000000000000000000000
	fst f1,r2,$-36
	fld f1,r2,$-28
	fld f2,r2,$-32
	fadd f1,f1,f2
	fst f1,r2,$-40
; ../tes.ml@16:41;16:51 ::= Ofadd ../tes.ml@16:41;16:44 ../tes.ml@16:48;16:51
	push r4
	lw r5,r2,$-40
	push r5
	lw r5,r2,$-36
	push r5
	jal @a_190fless
	sw r5,r2,$-44
	addi r1,r1,$8
	pop r4
; ../tes.ml@16:21;16:52 ../tes.ml@16:21;16:26
	li r5,$1
	sw r5,r2,$-48
; ../tes.ml@16:21;16:52
	lw r5,r2,$-44
	lw r6,r2,$-48
	bne r5,r6,@cfg_label_424
	j @cfg_label_423
	j @cfg_label_425
@cfg_label_424:
	li r5,$1
	sw r5,r2,$-52
; ../tes.ml@17:29;17:30
	lw r6,r2,$-52
	lw r5,r2,$8
	sub r5,r5,r6
	sw r5,r2,$-56
; ../tes.ml@17:25;17:30 ::= Osub ../tes.ml@17:25;17:26 ../tes.ml@17:29;17:30
	lw r5,r2,$28
	sw r5,r2,$-60
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@17:46;17:48
	lw r5,r2,$32
	sw r5,r2,$-64
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@17:49;17:51
	lw r5,r2,$-56
	sw r5,r2,$8
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@9:18;17:51
	lw r5,r2,$-12
	sw r5,r2,$12
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@9:18;17:51
	lw r5,r2,$-24
	sw r5,r2,$16
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@9:18;17:51
	lw r5,r2,$-28
	sw r5,r2,$20
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@9:18;17:51
	lw r5,r2,$-32
	sw r5,r2,$24
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@9:18;17:51
	lw r5,r2,$-60
	sw r5,r2,$28
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@9:18;17:51
	lw r5,r2,$-64
	sw r5,r2,$32
; ../tes.ml@9:18;17:51 ::<= ../tes.ml@9:18;17:51
	j @cfg_label_411
	j @cfg_label_443
@cfg_label_425:
@cfg_label_423:
	li r5,$0
	sw r5,r2,$-68
; ../tes.ml@16:68;16:69
	push r4
	lw r5,r2,$-68
	push r5
	jal print_int
	sw r5,r2,$-72
	addi r1,r1,$4
	pop r4
; ../tes.ml@9:18;17:51 ../tes.ml@16:58;16:67
	lw r5,r2,$-72
	addi r1,r1,$76
	pop r2
	pop r6
	jr r6
@cfg_label_440:
@cfg_label_438:
	li r5,$1
	sw r5,r2,$-76
; ../tes.ml@9:42;9:43
	push r4
	lw r5,r2,$-76
	push r5
	jal print_int
	sw r5,r2,$-72
	addi r1,r1,$4
	pop r4
; ../tes.ml@9:18;17:51 ../tes.ml@9:32;9:41
	lw r5,r2,$-72
	addi r1,r1,$76
	pop r2
	pop r6
	jr r6
@a_217xloop:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$100
@cfg_label_410:
@cfg_label_380:
	li r5,$40
	sw r5,r2,$-4
; ../tes.ml@5:12;5:14
	lw r6,r2,$-4
	lw r5,r2,$8
	slt r5,r5,r6
	li r6,$1
	xor r5,r5,r6
	sw r5,r2,$-8
; ../tes.ml@5:7;5:14 ::= Ogeq ../tes.ml@5:7;5:8 ../tes.ml@5:12;5:14
	li r5,$1
	sw r5,r2,$-12
; ../tes.ml@5:7;5:14
	lw r5,r2,$-8
	lw r6,r2,$-12
	bne r5,r6,@cfg_label_404
	j @cfg_label_403
	j @cfg_label_405
	j @cfg_label_406
@cfg_label_405:
@cfg_label_403:
	sw r3,r2,$-16
	addi r3,r3,$0
; ../tes.ml@5:4;19:20
	lw r5,r2,$-16
	addi r1,r1,$100
	pop r2
	pop r6
	jr r6
@cfg_label_406:
@cfg_label_404:
	push r4
	lw r5,r2,$8
	push r5
	jal float_of_int
	sw r5,r2,$-20
	addi r1,r1,$4
	pop r4
; ../tes.ml@6:30;6:44 ../tes.ml@6:30;6:42
	push r4
	lw r5,r2,$-20
	push r5
	jal @a_209dbl
	sw r5,r2,$-24
	addi r1,r1,$4
	pop r4
; ../tes.ml@6:25;6:45 ../tes.ml@6:25;6:28
	fmovi f1,$40.000000000000000000000000000000
	fst f1,r2,$-28
	fld f1,r2,$-24
	fld f2,r2,$-28
	fdiv f1,f1,f2
	fst f1,r2,$-32
; ../tes.ml@6:25;6:53 ::= Ofdiv ../tes.ml@6:25;6:45 ../tes.ml@6:49;6:53
	fmovi f1,$1.500000000000000000000000000000
	fst f1,r2,$-36
	fld f1,r2,$-32
	fld f2,r2,$-36
	fsub f1,f1,f2
	fst f1,r2,$-40
; ../tes.ml@6:25;6:60 ::= Ofsub ../tes.ml@6:25;6:53 ../tes.ml@6:57;6:60
	push r4
	lw r5,r2,$12
	push r5
	jal float_of_int
	sw r5,r2,$-44
	addi r1,r1,$4
	pop r4
; ../tes.ml@7:30;7:44 ../tes.ml@7:30;7:42
	push r4
	lw r5,r2,$-44
	push r5
	jal @a_209dbl
	sw r5,r2,$-48
	addi r1,r1,$4
	pop r4
; ../tes.ml@7:25;7:45 ../tes.ml@7:25;7:28
	fmovi f1,$40.000000000000000000000000000000
	fst f1,r2,$-52
	fld f1,r2,$-48
	fld f2,r2,$-52
	fdiv f1,f1,f2
	fst f1,r2,$-56
; ../tes.ml@7:25;7:53 ::= Ofdiv ../tes.ml@7:25;7:45 ../tes.ml@7:49;7:53
	fmovi f1,$1.000000000000000000000000000000
	fst f1,r2,$-60
	fld f1,r2,$-56
	fld f2,r2,$-60
	fsub f1,f1,f2
	fst f1,r2,$-64
; ../tes.ml@7:25;7:60 ::= Ofsub ../tes.ml@7:25;7:53 ../tes.ml@7:57;7:60
	li r5,$1000
	sw r5,r2,$-68
; ../tes.ml@18:22;18:26
	fmovi f1,$0.000000000000000000000000000000
	fst f1,r2,$-72
	fmovi f1,$0.000000000000000000000000000000
	fst f1,r2,$-76
	fmovi f1,$0.000000000000000000000000000000
	fst f1,r2,$-80
	fmovi f1,$0.000000000000000000000000000000
	fst f1,r2,$-84
	push r4
	lw r5,r2,$-64
	push r5
	lw r5,r2,$-40
	push r5
	lw r5,r2,$-84
	push r5
	lw r5,r2,$-80
	push r5
	lw r5,r2,$-76
	push r5
	lw r5,r2,$-72
	push r5
	lw r5,r2,$-68
	push r5
	jal @a_237iloop
	sw r5,r2,$-88
	addi r1,r1,$28
	pop r4
; ../tes.ml@18:16;18:48 ../tes.ml@18:16;18:21
	li r5,$1
	sw r5,r2,$-92
; ../tes.ml@19:16;19:17
	lw r6,r2,$-92
	lw r5,r2,$8
	add r5,r5,r6
	sw r5,r2,$-96
; ../tes.ml@19:12;19:17 ::= Oadd ../tes.ml@19:12;19:13 ../tes.ml@19:16;19:17
	lw r5,r2,$12
	sw r5,r2,$-100
; ../tes.ml@5:4;19:20 ::<= ../tes.ml@19:19;19:20
	lw r5,r2,$-96
	sw r5,r2,$8
; ../tes.ml@5:4;19:20 ::<= ../tes.ml@5:4;19:20
	lw r5,r2,$-100
	sw r5,r2,$12
; ../tes.ml@5:4;19:20 ::<= ../tes.ml@5:4;19:20
	j @cfg_label_380
	j @cfg_label_410
@a_211yloop:
	mflr r7
	push r7
	push r2
	mov r2,r1
	subi r1,r1,$40
@cfg_label_379:
@cfg_label_364:
	li r5,$40
	sw r5,r2,$-4
; ../tes.ml@3:9;3:11
	lw r6,r2,$-4
	lw r5,r2,$8
	slt r5,r5,r6
	li r6,$1
	xor r5,r5,r6
	sw r5,r2,$-8
; ../tes.ml@3:4;3:11 ::= Ogeq ../tes.ml@3:4;3:5 ../tes.ml@3:9;3:11
	li r5,$1
	sw r5,r2,$-12
; ../tes.ml@3:4;3:11
	lw r5,r2,$-8
	lw r6,r2,$-12
	bne r5,r6,@cfg_label_373
	j @cfg_label_372
	j @cfg_label_374
	j @cfg_label_375
@cfg_label_374:
@cfg_label_372:
	sw r3,r2,$-16
	addi r3,r3,$0
; ../tes.ml@3:1;22:15
	lw r5,r2,$-16
	addi r1,r1,$40
	pop r2
	pop r6
	jr r6
@cfg_label_375:
@cfg_label_373:
	li r5,$0
	sw r5,r2,$-20
; ../tes.ml@20:9;20:10
	push r4
	lw r5,r2,$8
	push r5
	lw r5,r2,$-20
	push r5
	jal @a_217xloop
	sw r5,r2,$-24
	addi r1,r1,$8
	pop r4
; ../tes.ml@20:3;20:12 ../tes.ml@20:3;20:8
	li r5,$10
	sw r5,r2,$-28
; ../tes.ml@21:12;21:14
	push r4
	lw r5,r2,$-28
	push r5
	jal print_char
	sw r5,r2,$-32
	addi r1,r1,$4
	pop r4
; ../tes.ml@21:1;21:14 ../tes.ml@21:1;21:11
	li r5,$1
	sw r5,r2,$-36
; ../tes.ml@22:13;22:14
	lw r6,r2,$-36
	lw r5,r2,$8
	add r5,r5,r6
	sw r5,r2,$-40
; ../tes.ml@22:9;22:14 ::= Oadd ../tes.ml@22:9;22:10 ../tes.ml@22:13;22:14
	lw r5,r2,$-40
	sw r5,r2,$8
; ../tes.ml@3:1;22:15 ::<= ../tes.ml@3:1;22:15
	j @cfg_label_364
	j @cfg_label_379
main:
	mflr r7
	push r7
	mov r31,r3
	addi r3,r3,$4
	push r2
	mov r2,r1
	subi r1,r1,$4
@cfg_label_362:
	li r5,$0
	sw r5,r31,$0
; ../tes.ml@23:6;23:7
	push r4
	lw r5,r31,$0
	push r5
	jal @a_211yloop
	sw r5,r2,$-4
	addi r1,r1,$4
	pop r4
; @0:-1;0:-1 ../tes.ml@23:0;23:5
	li r5,$0
	hlt
