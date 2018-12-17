#! /usr/bin/python3

import os
import copy

if os.system('cd ../src/compiler; make > /dev/null'):
	print('comipler make failed')
	exit(-1)

if os.system('cd ../src/lex; make > /dev/null'):
	print('lexer make failed')
	exit(-1)

if os.system('cd ../src/yacc; make > /dev/null'):
	print('yacc make failed')
	exit(-1)

os.system('cp ../main ./')
os.system('cp ../my_lex ./')
os.system('cp ../my_yacc ./')
os.system('mkdir lib')
os.system('mkdir lib/asm')
os.system('mkdir lib/ml')
os.system('cp ../lib/asm/*.s ./lib/asm/')
os.system('cp ../lib/ml/*.ml ./lib/ml/')
os.system('cp ../lib/ml/*.mli ./lib/ml/')

#TODO(satos) あとで消したい...
os.system('mkdir lib/C')
os.system('cp ../lib/C/*.o ./lib/C/')

with open('test_order.txt','r') as fp:
	ts = fp.read()
	datum = ts.split('\n')
	td = []
	for d in datum:
		if d == '':
			break
		td.append(d)
	datum = td

if len(os.sys.argv)>1:
	datum = [datum[int(os.sys.argv[1])]]

def get_ext(fn):
	fns = fn.split('.')
	ext = '.' + fns[-1]
	rfn = '.'.join(fns[:-1])
	return rfn,ext

def exec_ocaml(fs,ifn,ofn):
	if len(fs)==1:
		os.system('cat test_header.ml > tmp.ml')
		os.system('cat ' + fs[0] + ' >> tmp.ml')
		if ifn is None:
			os.system('ocaml tmp.ml > ' + ofn)
		else:
			os.system('ocaml tmp.ml < ' + ifn + ' > ' + ofn)
		return
	
	fs = list(map(get_ext,fs))
	rm_files = []
	for f,e in fs:
		if e == '.mll':
			os.system('ocamllex ' + (f + e))
			rm_files.append(f + '.ml')
	
	fs = list(map(lambda x: x[0] + '.ml',fs))
	
	os.system('cat test_header.ml > tmp.ml')
	os.system('cat ' + fs[-1] + ' >> tmp.ml')
	fs[-1] = "tmp.ml"
	
	fs = list(map(lambda x: x.split('.ml')[-2],fs))
	for fn in fs:
		if os.path.exists(fn + '.mli'):
			os.system('ocamlc -c ' + fn + '.mli')
	
	for fn in fs:
		os.system('ocamlc -c ' + fn + '.ml')
	
	fs = list(map(lambda x: x + '.cmo',fs))
	os.system('ocamlc ' + ' '.join(fs) + ' -o ocamlout.out')

	if ifn is None:
		os.system('./ocamlout.out > ' + ofn)
	else:
		os.system('./ocamlout.out < ' + ifn + ' > ' + ofn)
	
	os.system('rm *.cmi')
	os.system('rm *.cmo')
	os.system('rm ' + ' '.join(rm_files))


def exec_x86(fs,ifn,ofn):
	fs = list(map(get_ext,fs))
	
	rm_files = []
	for f,e in fs:
		if e == '.mll':
			#os.system('../lexer_compile_test/a.out < ' + (f + e) + ' > ' + f + '.ml')
			os.system('../my_lex ' + (f + e) + ' > ' + f + '.ml')
			os.system('./main ' + (f + '.ml') + ' -mli')
			rm_files.append(f + '.ml')
			rm_files.append(f + '.mli')
		rm_files.append(f + '.s')
	
	fs = list(map(lambda x: x[0] + '.ml',fs))
	print(fs)
	os.system('./main ' + ' '.join(fs) + ' -noinline -o x86.out -asi > /dev/null')
	if ifn is None:
		os.system('./x86.out > ' + ofn)
	else:
		os.system('./x86.out < ' + ifn + ' > ' + ofn)
	
	os.system('rm ' + ' '.join(rm_files))


for d in datum:
	fs = []
	ifn = None
	d = d.split(' ')
	for i,fn in enumerate(d):
		if fn == '-i':
			ifn = d[i+1]
			break
		else:
			fs.append(fn)
	
	os.system('rm -f tmp.ml a.out o_tortesia.s x86.out')
	print("---------------" + ' '.join(d) + "----------------")
	for fn in fs:
		print("----------[" + fn + "]------------")
		print('')
		os.system('cat ' + fn)
	
	os.system('rm -f o_ocaml.txt o_mincaml.txt')
	
	exec_ocaml(fs,ifn,'o_ocaml.txt')
	exec_x86(fs,ifn,'o_mincaml.txt')
	
	if os.system('diff o_ocaml.txt o_mincaml.txt') == 0:
		print('ok')
	else:
		print('test failed')
		break

	os.system('rm -f ocamlout.out')
	if os.system('rm x86.out'):
		print("maybe compile failed")
		break

	os.system('rm -f *.o')
	os.system('rm -f *.s')	

os.system('rm -r lib')
os.system('rm -f tmp.ml')


