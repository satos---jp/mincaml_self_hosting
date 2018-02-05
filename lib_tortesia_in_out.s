read_int:
	ini r5
	ret

read_float:
	inf r5
	rtof f5,r5
	ret

print_char:
	out r10
	ret

