

ops = """ 
	| Ominus | Oadd | Osub | Omul | Odiv
	| Ofadd | Ofsub | Ofmul | Ofdiv
	| Oeq | Oneq | Olt | Oleq | Ogt | Ogeq | Osemi1 | Osemi2 | Onot 
	| OArrCrt | OArrRead | OArrWrite
"""


ops = ''.join(ops.split()).split('|')[1:]

for s in ops:
	s = '	| %s -> "%s"' % (s,s)
	print s

print ops

exit(0)


frs = """
	| KConst of Syntax.const
	| KOp        of string * (name list)
	| KIfEq      of name * name * kexp * kexp
	| KIfLeq     of name * name * kexp * kexp
	| KLet       of name * kexp * kexp
	| KVar       of name
	| KLetRec    of name * (name list) * kexp * kexp
	| KApp       of name * (name list)
	| KTuple     of (name list)
	| KLetTuple  of (name list) * name * kexp
"""


for s in frs.split('\n')[1:-1]:
	s = s.split()[1:]
	fr = s[0]
	to = 'C' + fr[1:]
	n = s.count('*')
	c = ord('a')
	args = ['%c,' % chr(i+c) for i in xrange(n)]
	args = '(' + ''.join(args) + chr(n+c) + ')'
	s = '	| %s%s -> %s%s' % (fr,args,to,args)
	print s
