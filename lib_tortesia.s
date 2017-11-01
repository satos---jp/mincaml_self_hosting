print_char:
	hlt

print_int:
	hlt
	
fless:
	hlt
	
float_of_int:
	lw r5,r2,$0
	itof f0,r5
	fst f0,r2,$-4
	lw r5,r2,$-4
	ret

