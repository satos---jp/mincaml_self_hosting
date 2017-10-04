ops = """
  | TVar(na)
  | TOp(op,es)
  | TIf(e1,e2,e3)
  | TLet(na,e1,e2)
  | TLetRec(na,vs,e1,e2)
  | TApp(e1,es)
  | TTuple(es)
  | TLetTuple(vs,e1,e2)
"""

ops = ops.split('\n')[1:-1]
for s in ops:
	bo = s.split('(')[1][:-1].split(',')
	rs = s.split('(')[0].split(' ')[-1]
	#print rs,bo
	def conv(x):
		if x == 'na':
			return 'nf na'
		elif x[0] == 'e':
			if x=='es':
				return 'mf es'
			else:
				return 'f ' + x
		elif x=='vs':
			return 'mnf vs'
		elif x=='op':
			return 'op'
		else:
			print x
			raise Hoge
	bo = map(conv,bo)
	bo = '(' + ','.join(bo) + ')'
	s +=  ' -> ' + rs + bo
	
	print s


exit(0)

ops = """
	(Onot,[TyInt],TyInt);
	(Ominus,[TyInt],TyInt);
	(Oadd,[TyInt;TyInt],TyInt);
	(Osub,[TyInt;TyInt],TyInt);
	(Omul,[TyInt;TyInt],TyInt);
	(Odiv,[TyInt;TyInt],TyInt);
	(Ofadd,[TyFloat;TyFloat],TyFloat);
	(Ofsub,[TyFloat;TyFloat],TyFloat);
	(Ofmul,[TyFloat;TyFloat],TyFloat);
	(Ofdiv,[TyFloat;TyFloat],TyFloat);
	(Oeq,[TyInt;TyInt],TyInt);
	(Oneq,[TyInt;TyInt],TyInt);
	(Olt,[TyInt;TyInt],TyInt);
	(Oleq,[TyInt;TyInt],TyInt);
	(Ogt,[TyInt;TyInt],TyInt);
	(Ogeq,[TyInt;TyInt],TyInt);
"""

ops = ops.split('\n')[1:-1]
for s in ops:
	s = ';'.join(s.split(';')[:-1])
	s = s.split(',')
	h = s[0][2:]
	t = ','.join(s[1:])
	t = '(fun () -> (' + t + ')'
	s = '\t\t\t| %s -> %s' % (h,t)
	print s


exit(0)


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
