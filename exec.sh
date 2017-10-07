rm a.out
nasm out.s -f elf32 -g -o out.o
gcc -m32 out.o
./a.out

