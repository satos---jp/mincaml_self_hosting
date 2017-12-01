;print_char:
;	hlt

;print_int:
;	hlt
	
;fless:
;	hlt

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

fiszero:
	fld f1,r1,$0
	fcmp f1,f0
	fbeq fiszero_iszero
	li r5,$0
	ret
fiszero_iszero:
	li r5,$1
	ret

fisneg:
	fld f1,r1,$0
	fcmp f0,f1
	fblt fiszero_isneg
	li r5,$0
	ret
fiszero_isneg:
	li r5,$1
	ret

