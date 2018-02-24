float_of_int:
	rtof f1,r10
	itof f5,f1
	ret

int_of_float:
	ftoi f1,f10
	ftor r9,f1
	ret

sqrt:
	fsqrt f5,f10
	ret


; itof,ftoiは
; 負、正とともに小数点切り捨てだが、
; floor は、負のほうは切り捨てする。

