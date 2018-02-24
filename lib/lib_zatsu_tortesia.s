
float_of_int:
	lw r5,r1,$0
	rtof f2,r5
	itof f1,f2
	ftor r5,f1
	ret

int_of_float:
	fld f1,r1,$0
	ftoi f2,f1
	ftor r5,f2
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

