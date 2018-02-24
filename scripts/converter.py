#coding: utf-8

import sys
import re

def lw(v):
	if v[0][0]=='f':
		return 'fld'
	else:
		return 'lw'

def sw(v):
	if v[0][0]=='f':
		return 'fst'
	else:
		return 'sw'

def conv(s):
	s = s[:-1].split()
	#print s
	if len(s)==0 or s[0][0]==';' or s[0][-1]==':':
		return ''.join(s)
	else:
		#print s
		oc = s[0]
		#print oc,
		s = ''.join(s[1:]).split(',')
		if oc in globals().keys():
			oc = eval(oc)(s)
		return '\t' + oc + ' ' + ','.join(s)


text = map(conv,sys.stdin.readlines())

print '\n'.join(text)





