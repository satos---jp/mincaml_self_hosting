rm a.exe
nasm out.s -f elf32 -g -o out.o
gcc -m32 -g out.o -o a.exe
echo "compiled" >&2
./a.exe

