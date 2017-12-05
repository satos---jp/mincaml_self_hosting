
"""
def finish(ev):
	print(ev)
	while 1:
		s = input('>> ')
		print(eval(s))
	#print(dir(ev))

gdb.events.stop.connect(finish)
"""



src = ""
with open('o_tortesia.s') as fp:
	src = fp.readlines() 

class State:
	def print_state(sl):
		p = sl.code
		cs = ""
		for i in range(-3,4):
			p = sl.code+i
			if p<0 or p>=len(src):
				continue
			if i==0:
				cs += ('%d\t' % p) + '=>' + src[p]
			else:
				cs += ('%d\t' % p) + src[p]
		
		print(cs)
		print(sl.ipc)
		print(sl.regs)
		print(sl.stack)
		print(sl.heap)
	
	def __init__(sl):
		sl.code = int(gdb.execute('i r esi',to_string=True).split()[1][2:],16)
		sl.ipc  = gdb.execute('x/2xw &inst_counter_up',to_string=True)
		sl.regs = gdb.execute('x/40xw &_lr',to_string=True)
		sl.stack = gdb.execute('x/20xw _r1',to_string=True)
		sl.heap = gdb.execute('x/20xw _r3',to_string=True)

gdb.execute('r < fcomp_i.txt')

sts = []

def shell():
	i = 0
	bs = ""
	while 1:
		sts[i].print_state()
		s = input('>> ')
		if s=='':
			s = bs
		if s=='c':
			break
		elif s=='b' and i>0:
			i -= 1
		elif s=='f' and i < len(sts)-1:
			i += 1
		bs = s


t = 0
#gdb.execute('b 0x08048f0f')
while 1:
	#s = gdb.execute('i r $eip',to_string=True)
	#print(repr(s.split()))
	#if s.split()[1]=='0x8048f0f':
	sts.append(State())
	x = gdb.execute('i r eax',to_string=True).split()[1][2:]
	s = gdb.execute('c',to_string=True)
	print('x:: ',x,t,)
	#print(s)
	t += 1
	
	if not ("SIGTRAP" in s):
		shell()
		
		
		
		

