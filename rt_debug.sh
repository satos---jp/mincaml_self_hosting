./main ../raytracer/min-rt.ml ../raytracer/globals.ml -t -o o_tortesia.s -noinline -asi -v > o.txt && 
python tortesia2x86.py -r -nc -d < o_tortesia.s > out.s && 
nasm out.s -f elf32 -g -o out.o &&
gcc -m32 -nostdlib out.o -o a.out &&
gdb -q a.out -x gs.py

