hd = """
let rec f x =
 if x < 0 then 0
"""

def tocs(s):
   return ''.join(map(lambda c: "print_char %d;\n" % ord(c),s))
  

s1 = "	else if x = "
s2 = " then "

tl = """
 else -1
in
let rec loop1 g n =
   let p = g n in
   if p < 0 then ()
   else (
       %s
       print_int n;
       %s
       print_int p;
       print_char 10;
       loop1 g (n+1)
   )
in
let rec loop2 g n =
   let p = g n in
   if p < 0 then ()
   else (
       print_char p;
       loop2 g (n+1)
   )
in
""" 
#print len(tl)
tl = tl % (tocs(s1),tocs(s2))

tl += tocs(hd)
tl += """
loop1 f 0;
loop2 f 0;
print_char 10
"""
bo = ''.join(map(lambda (i,c): "	else if x = %d then %d\n" % (i,ord(c)),enumerate(tl)))

quine = hd + bo + tl

#print(len(quine))
print(quine)

