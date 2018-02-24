#coding: utf-8

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("-w", "--windows", help="for windows",action="store_true")
parser.add_argument("-r", "--register", help="register allocation",action="store_true")
parser.add_argument("-nc", "--nocount", help="don't count instr num",action="store_true")
parser.add_argument("-d", "--debug", help="add int 0x3 before instruction",action="store_true")

args = parser.parse_args()

main_name = "_main" if args.windows else "_start"

head = [
	"BITS 32",
	("%include \"lib/libio_" + ("win" if args.windows else "linux") + ".s\""),
	"%include \"lib/lib.s\"",
	"%include \"lib/lib_tortesia_to_x86.s\"",
	"section .bss",
	"\tresb 0x100000",
	"global_stack:",
	"\tresb 0x100",
	"section .data",
	"inst_counter_up:",
	"\tdd 0x0",
	"inst_counter:",
	"\tdd 0x0",
	"_lr:",
	"\tdd 0",
	"_cr:",
	"\tdd 0",
]

for i in xrange(32):
	head += [
		"_r%d:" % i,
		"\tdd 0" 
	]

for i in xrange(32):
	head += [
		"_f%d:" % i,
		"\tdd 0.0" 
	]

def eprintc(x):
	return [
		("push %d" % ord(x)),
		"call print_char_err",
		"add esp,4"
	]

def eprints(s):
	return sum(map(eprintc,s),[])

text = [
	"section .bss",
	"global_heap:",
	"\tresb 0x40000000",
	"section .text",
	("global " + main_name),
	"get_eip:", #trick from https://stackoverflow.com/questions/4062403/how-to-check-the-eip-value-with-assembly-language
	"\tmov eax,[esp]",
	"\tret",
]

"""
hb 005ef120
ha 0067cb7c
ic 00000003a6d3ec70

157億命令
580メガバイト
"""

if args.register and False:
	libfuncs = [
		('read_char',1),
		
		('print_int',1),
		('sin',1),
		('cos',1),
		('atan',1),
	]
else:
	libfuncs = [
		('print_char',1),
		('read_char',1),
		('read_int',1),
		('read_float',1),
	
		('print_int',1),
		('sin',1),
		('cos',1),
		('atan',1),
	]

#呼び出し規約のトランポリン
for fn,i in libfuncs:
	text += ['_' + fn + ':']

	if args.register:
		#1引数しかないのでアドホックにいく。
		if fn in ['sin','cos','atan']:
			text += [
				"\tmov eax,[_f10]",
				'\tpush dword eax'
			]
		else:
			for k in range(i):
				text += [
					"\tmov eax,[_r%d]" % (k+10),
					'\tpush dword eax'
				]
	else:
		text += [
			"\tmov eax,[_r1]"
		]
		for k in range(i)[::-1]:
			text += ['\tpush dword [eax%+d]' % (k*4) ]
	
	if args.register and fn in ['read_float','sin','cos','atan']:
		text += [
			"\tcall %s" % fn,
			"\tmov [_f5],eax",
		]
	else:
		text += [
			"\tcall %s" % fn,
			"\tmov [_r9],eax",
		]
	
	if fn not in ['print_int']:
		text += ["\tadd esp,%d" % (i*4)]
	
	text += ["\tjmp [_lr]"]


def c2s(x):
	if x[0]=='$':
		return x[1:]
	else:
		return '_'+x

def hlt(v):
	return (
		eprints("ha ") +
		["push dword [_r3]",
		"call print_hex_err",
		"add esp,4"] +
		eprints("\nic ") + 
		[
			"mov eax,[inst_counter_up]",
			"push eax", 
			"call print_hex_err",
			"add esp,4",
			"mov eax,[inst_counter]",
			"push eax", 
			"call print_hex_err",
			"add esp,4"
		] +
		eprintc("\n") + 
		([
			"ret",
		]
		if args.windows else 
		[
			"mov ebx,0",
			"mov eax,1",
			"int 0x80"
		])
	)

def mov(v):
	return [
		"mov eax,[%s]" % v[1],
		"mov [%s],eax" % v[0]
	]

def li(v):
	return [
		"mov dword [%s],%s" % (v[0],c2s(v[1]))
	]

def lw(v):
	return [
		"mov eax,[%s]" % v[1],
		"mov eax,[eax%+d]" % int(v[2][1:]),
		"mov [%s],eax" % v[0]
	]

def sw(v):
	return [
		"mov eax,[%s]" % v[0],
		"mov ebx,[%s]" % v[1],
		"mov [ebx%+d],eax" % int(v[2][1:])
	]

def xor(v):
	return [
		"mov eax,[%s]" % v[1],
		"mov ebx,[%s]" % v[2],
		"xor eax,ebx",
		"mov [%s],eax" % v[0]
	]

def add(v):
	return [
		"mov eax,[%s]" % v[1],
		"mov ebx,[%s]" % v[2],
		"add eax,ebx",
		"mov [%s],eax" % v[0]
	]

def sub(v):
	return [
		"mov eax,[%s]" % v[1],
		"mov ebx,[%s]" % v[2],
		"sub eax,ebx",
		"mov [%s],eax" % v[0]
	]

def addi(v):
	return [
		"mov eax,[%s]" % v[1],
		"add eax,%d" % int(v[2][1:]),
		"mov [%s],eax" % v[0]
	]

def subi(v):
	return [
		"mov eax,[%s]" % v[1],
		"sub eax,%d" % int(v[2][1:]),
		"mov [%s],eax" % v[0]
	]

def sll(v):
	return [
		"mov eax,[%s]" % v[1],
		"shl eax,%d" % int(v[2][1:]),
		"mov [%s],eax" % v[0]
	]

def sra(v):
	return [
		"mov eax,[%s]" % v[1],
		"sar eax,%d" % int(v[2][1:]),
		"mov [%s],eax" % v[0]
	]

def slt(v):
	return [
		#"int 0x3",
		"mov eax,[%s]" % v[1],
		"mov ebx,[%s]" % v[2],
		"xor ecx,ecx",
		"cmp eax,ebx",
		"setl cl",
		"mov [%s],ecx" % v[0]
	]

def slti(v):
	return [
		#"int 0x3",
		"mov eax,[%s]" % v[1],
		"mov ebx,%d" % int(v[2][1:]),
		"xor ecx,ecx",
		"cmp eax,ebx",
		"setl cl",
		"mov [%s],ecx" % v[0]
	]

def seq(v):
	return [
		"mov eax,[%s]" % v[1],
		"mov ebx,[%s]" % v[2],
		"xor ecx,ecx",
		"cmp ebx,eax",
		"sete cl",
		"mov [%s],ecx" % v[0]
	]

def bne(v):
	return [
		"mov eax,[%s]" % v[0],
		"mov ebx,[%s]" % v[1],
		"cmp eax,ebx",
		"jne _%s" % v[2]
	]

def beq(v):
	return [
		"mov eax,[%s]" % v[0],
		"mov ebx,[%s]" % v[1],
		"cmp eax,ebx",
		"je _%s" % v[2]
	]


def j(v):
	return [
		"jmp _%s" % v[0]		
	]

def jal(v):
	return [
		"call get_eip",
		"add eax,13",
		"mov [_lr],eax",
		"jmp _%s" % v[0]
	]

def jr(v):
	return [
		"jmp [%s]" % v[0]		
	]

def jalr(v):
	return [
		"call get_eip",
		"add eax,14",
		"mov [_lr],eax",
		"jmp [%s]" % v[0]
	]

def ret(v):
	return [
		"mov eax,[_lr]",
		"jmp eax"
	]

def mflr(v):
	return [
		"mov eax,[_lr]",
		"mov [%s],eax" % v[0]		
	]

def push(v):
	return [
		"sub dword [_r1],4",
		"mov eax,dword [%s]" % v[0],
		"mov ebx,[_r1]",
		"mov [ebx],eax"
	]

def pop(v):
	return [
		"mov eax,[_r1]",
		"mov eax,[eax]",
		"mov dword [%s],eax" % v[0],
		"add dword [_r1],4"
	]

def itof(v):
	return [
		"push dword [%s]" % v[1],
		"call float_of_int",
		"mov [%s],eax" % v[0],
		"add esp,4"
	]

def ftoi(v):
	return [
		"push dword [%s]" % v[1],
		"call int_of_float",
		"mov [%s],eax" % v[0],
		"add esp,4"
	]
	
def rtof(v):
	return mov(v)

def ftor(v):
	return mov(v)

def fld(v):
	return lw(v)

def fst(v):
	return sw(v)

def fmovi(v):
	global head
	tag = "float_const_%d" % len(head)
	head += [
		"%s:" % tag,
		"\tdd %.30f" % float(v[1][1:])
	]
	return [
		"mov eax,[%s]" % tag,
		"mov [%s],eax" % v[0]
	]

def fmov(v):
	return [
		"mov eax,[%s]" % v[1],
		"mov [%s],eax" % v[0]
	]

def fadd(v):
	return [
		"fld dword [%s]" % v[1],
		"fld dword [%s]" % v[2],
		"faddp",
		"fstp dword [%s]" % v[0] 
	]

def fsub(v):
	return [
		"fld dword [%s]" % v[1],
		"fld dword [%s]" % v[2],
		"fsubp",
		"fstp dword [%s]" % v[0] 
	]

def fmul(v):
	return [
		"fld dword [%s]" % v[1],
		"fld dword [%s]" % v[2],
		"fmulp",
		"fstp dword [%s]" % v[0] 
	]

def fdiv(v):
	return [
		"fld dword [%s]" % v[1],
		"fld dword [%s]" % v[2],
		"fdivp",
		"fstp dword [%s]" % v[0] 
	]


def feq(v):
	return [
		"fld dword [%s]" % v[0],
		"fld dword [%s]" % v[1],
		"xor ecx,ecx",
		"fucomip",
		"sete cl",
		"mov dword [_cr],ecx",
		"push eax",
		"fstp dword [esp]",
		"pop eax",
	]

def flt(v):
	return [
		"fld dword [%s]" % v[0],
		"fld dword [%s]" % v[1],
		"xor ecx,ecx",
		"fucomip",
		"seta cl",
		"mov dword [_cr],ecx",
		"push eax",
		"fstp dword [esp]",
		"pop eax",
	]

def bft(v): #非0のとき
	return [
		"mov eax,dword [_cr]",
		"test eax,eax",
		"jne _%s" % v[0]
	]

def bff(v): #0のとき
	return [
		"mov eax,dword [_cr]",
		"test eax,eax",
		"je _%s" % v[0]
	]

def fsqrt(v):
	return [
		"fld dword [%s]" % v[1],
		"fsqrt",
		"fstp dword [%s]" % v[0] 
	]

def check(v):
	return [
		 "mov eax,[%s]" % v[0]
	]

def ini(v):
	return [
		"push ebx",
		"call read_int",
		"pop ebx",
		"mov [%s],eax" % v[0] 
	]

def inf(v):
	return [
		"push ebx",
		"call read_float",
		"pop ebx",
		"mov [%s],eax" % v[0] 
	]

def out(v):
	return [
		"mov eax,[%s]" % v[0],
		"push eax",
		"call print_char",
		"pop eax"
	]


import sys
import re

def name_conv(x):
	if re.compile('^[rf]\d\d?$').match(x):
		return '_' + x
	else:
		return x 
		


for i,s in enumerate(sys.stdin.readlines()):
	s = s.split()
	if len(s)==0 or s[0][0]==';':
		continue
	
	ns = ""
	if s[0][-1]==':':
		if s[0][:-1] in map(lambda x: x[0],libfuncs):
			continue
		elif s[0]=='main:':
			ns = (
			[
				(main_name + ":"),
				"\tfinit\n",
				"\tmov dword [_r1],global_stack",
				"\tmov dword [_r3],global_heap"
			] + eprints("hb ") + 
			["\tpush dword [_r3]",
			"\tcall print_hex_err",
			"\tadd esp,4"] + 
			eprintc("\n") + []
			#["int 0x3"]
			) 
		else:
			ns = ['_'+s[0]]
			"""
			if s[0]=='fiszero:':
				ns += ["int 0x3"]
			"""
	else:
		#print s
		oc = s[0]
		s = map(name_conv, ''.join(s[1:]).split(','))
		ns = map(lambda c: '\t' + c,eval(oc)(s))
		
		ns += ["mov dword [_r0],0"]
		#if i in range(40,181):
		#if i==39:
		if args.debug:
			ns = ["\tmov esi,%d" % i,"\tint 0x3"] + ns
		
		if not args.nocount:
			ns = [
				"\tmov edx,[inst_counter]", 
				"\tadd edx,1",
				"\tmov [inst_counter],edx",
				"\tsetb dl", 
				"\tand edx,1",
				"\tadd dword [inst_counter_up],edx"
			] + ns
		
		
		
		
	text += ns

#print ts


print '\n'.join(head + text)



