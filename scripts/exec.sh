rm a.out
nasm out.s -f elf32 -g -o out.o
gcc -m32 -nostdlib out.o -o a.out
echo "compiled" >&2
./a.out

