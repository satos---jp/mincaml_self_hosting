
"""
def finish(ev):
	print(ev)
	while 1:
		s = input('>> ')
		print(eval(s))
	#print(dir(ev))

gdb.events.stop.connect(finish)
"""


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
		print('r1 aka stack')
		print(sl.stack)
		print('r3 aka heap')
		print(sl.heap)
	
	def __init__(sl):
		sl.code = int(gdb.execute('i r esi',to_string=True).split()[1][2:],16)
		sl.ipc  = gdb.execute('x/2xw &inst_counter_up',to_string=True)
		sl.regs = gdb.execute('x/60xw &_lr',to_string=True)
		sl.stack = gdb.execute('x/20xw _r1',to_string=True)
		sl.heap = gdb.execute('x/20xw _r3',to_string=True)

#gdb.execute('r < contest.sld')
gdb.execute('r')

sts = []

def shell():
	i = 0
	bs = ""
	while 1:
		print('step %d' % i)
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
		elif s=='tl':
			i = len(sts)-1
		elif s=='hd':
			i = 0
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
	#print('x:: ',x,t,)
	#print(s)
	t += 1
	
	if not ("SIGTRAP" in s):
		shell()
"""

# from peda/peda.py
class Alias(gdb.Command):
    """
    Generic alias, create short command names
    This doc should be changed dynamically
    """
    def __init__(self, alias, command, shorttext=1):
        (cmd, opt) = (command + " ").split(" ", 1)
        if cmd == "peda" or cmd == "pead":
            cmd = opt.split(" ")[0]
        if not shorttext:
            self.__doc__ = pedacmd._get_helptext(cmd)
        else:
            self.__doc__ = green("Alias for '%s'" % command)
        self._command = command
        self._alias = alias
        super(Alias, self).__init__(alias, gdb.COMMAND_NONE)

    def invoke(self, args, from_tty):
        self.dont_repeat()
        gdb.execute("%s %s" %(self._command, args))

    def complete(self, text, word):
        completion = []
        cmd = self._command.split("peda ")[1]
        for opt in getattr(pedacmd, cmd).options: # list of command's options
            if text in opt and opt not in completion:
                completion += [opt]
        if completion != []:
            return completion
        if cmd in ["set", "show"] and text.split()[0] in ["option"]:
            opname = [x for x in config.OPTIONS.keys() if x.startswith(word.strip())]
            if opname != []:
                completion = opname
            else:
                completion = list(config.OPTIONS.keys())
        return completion


import code
class RegisterGDB(gdb.Command):
	def __init__(sl,f,na):
		sl.f = f
		super(RegisterGDB, sl).__init__(na,gdb.COMMAND_NONE)
	
	def invoke(sl,args,istty):
		#code.interact(local={'args':args,'istty':istty})
		#print(args,istty)
		sl.f(args)


libs =  ['nfa', 'parsing', 'list', 'lexing', 'string', 'pervasive']

symbols = []

def ambiguous_search(x):
	"""
	global symbols
	res = []
	for d in symbols:
		if s in d:
			res.append(d)
	"""
	
	# already gdb done
	
	s = gdb.execute('info functions ' + x,to_string=True)
	s = s.split('\n')
	#print(len(s))
	if len(s)>5:
		raise Exception('ambiguous data',x,'to',s)
	
	s = s[-2]
	res = s.split(' ')[0]
	print('%s is solved as %s' % (x,s))
	return res

def collect_filedatas(libpath='lib'):
	global symbols
	gdb.execute('start',to_string=True)
	s = gdb.execute('x/100xi _start',to_string=True)
	s = s.split('\n')[1:]
	ts = []
	for d in s:
		d = d.split('\t')
		if len(d)>=2 and d[1][:4]=='call':
			d = d[1].split('<')[1].split('.mlmain>')[0]
			ts.append(d)
		else:
			break
	s = ts
	print('dependencies',s)
	for d in s:
		if d in libs:
			d = libpath+'/ml/'+d
		d += '.s'
		with open(d,'r') as fp:
			hs = fp.read().split('\n')
			#print(len(hs))
			symbols += list(filter(lambda x: len(x)>0 and x[-1]==':',hs))
	
	symbols = list(map(lambda x: x[:-1],symbols))
	#print(len(symbols))

imask = 0x40000000
tmask = 0x10000000

def data2str(x):
	#print('data2str ' + x)
	d = x
	assert d[:2]=='0x'
	d = int(d[2:],16)
	if d & imask:
		d ^= imask
		if d & 0x80000000:
			d ^= 0x80000000
			d -= 0x10000000
		return str(d)
	elif d & tmask:
		d ^= tmask
		ls = gdb.execute('x/xw ' + hex(d),to_string=True)
		ls = int(ls.split('\t')[-1][2:])
		ds = gdb.execute('x/%dxw %s' % (ls,hex(d+4)),to_string=True)
		ds = ds.split('\t')[1:]
		ds = list(map(lambda s: data2str(s),ds))
		return ('(' + ','.join(ds)) + ')'
		#return ('T',int(ls),hex(d),ds)
		
# _,$,#,@,~ とかいけそう
# だめなの @ ~ # 

def print_data(x):
	v = x.split(' ')
	if len(v)==2 and v[0]=='st':
		v = '$esp+(%s)' % v[1]
	if len(v)==2 and v[0]=='arg':
		v = '$ebp+%d' % (int(v[1])*4+4)
	elif len(v)==1:
		v = v[0]
	else:
		print('invalid input "',x,'" for print_data')
		return
	
	v = gdb.execute('x/xw ' + v,to_string=True)
	v = v.split('\t')[-1]
	s = data2str(v)
	print(s)

"""
nasm a.s -f elf32 -g -o a.o && gcc -m32 -nostdlib a.o -o a.out
"""

def ambiguous_break(s):
	bn = ambiguous_search(s)
	print('breakpoint of ' + bn)
	gdb.execute('b* ' + bn)

RegisterGDB(print_data,'rp')
RegisterGDB(ambiguous_break,'ab')

#collect_filedatas(libpath='../lib')
#print(ambiguous_search('token2id'))
#gdb.execute('r < i.txt')
#gdb.execute('rp st 4')

"""
t = 0
#gdb.execute('b 0x08048f0f')
while 1:
	#s = gdb.execute('i r $eip',to_string=True)
	#print(repr(s.split()))
	#if s.split()[1]=='0x8048f0f':
	sts.append(State())
	x = gdb.execute('i r eax',to_string=True).split()[1][2:]
	s = gdb.execute('c',to_string=True)
	#print('x:: ',x,t,)
	#print(s)
	t += 1
	
	if not ("SIGTRAP" in s):
		shell()
"""

#Alias('hogehuga','hogehuga')


