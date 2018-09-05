s = """
global print_char
global read_char
global print_char_err
global puts_err
"""

s = """
global fless
global int_of_float
global float_of_int
global fiszero
global fispos
global fisneg
global fabs
global floor
global fsqr
global fneg
global fhalf
global sqrt
global sin
global cos
global atan
global read_int
global read_float
global raise_match_failure
"""

s = map(lambda x: x.split(' ')[1],s.split('\n')[1:-1])

t = """
{0}:
	dd {0}_p
{0}_p:
	dd {0}_
"""[:-1]

s = map(lambda x: t.format(x),s)
print ''.join(s)
