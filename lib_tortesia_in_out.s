read_int:
	in r5
	in r6
	sll r6,r6,$8
	or r5,r5,r6
	in r6
	sll r6,r6,$16
	or r5,r5,r6
	in r6
	sll r6,r6,$24
	or r5,r5,r6
	ret

read_float:
	in r5
	in r6
	sll r6,r6,$8
	or r5,r5,r6
	in r6
	sll r6,r6,$16
	or r5,r5,r6
	in r6
	sll r6,r6,$24
	or r5,r5,r6
	rtof f5,r5
	ret

print_char:
	out r10
	ret


