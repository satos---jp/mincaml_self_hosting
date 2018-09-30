#! /usr/bin/python3

import os
import copy

if os.system('cd ../src/compiler; make > /dev/null'):
	print('make failed')
	exit(-1)

os.system('cp ../main ./')
os.system('cp ../lib/ ./ -r')

with open('test_order.txt','r') as fp:
	ts = fp.read()
	datum = ts.split('\n')



def exec_ocaml(fs,ifn,ofn):
	if len(fs)==1:
		os.system('cat test_header.ml > tmp.ml')
		os.system('cat ' + fs[0] + ' >> tmp.ml')
		if ifn is None:
			os.system('ocaml tmp.ml > ' + ofn)
		else:
			os.system('ocaml tmp.ml < ' + ifn + ' > ' + ofn)
		
		return
	
	os.system('cat test_header.ml > tmp.ml')
	os.system('cat ' + fs[-1] + ' >> tmp.ml')
	fs = copy.deepcopy(fs)
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



def exec_x86(fs,ifn,ofn):
	# x86で実行
	os.system('./main ' + ' '.join(fs) + ' -noinline -o x86.out -asi > /dev/null')
	if ifn is None:
		os.system('./x86.out > ' + ofn)
	else:
		os.system('./x86.out < ' + ifn + ' > ' + ofn)


for d in datum:
	fs = []
	ifn = None
	d = d.split(' ')
	if len(list(filter(lambda x: x!='',d)))==0:
		break
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


