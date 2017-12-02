rm a.exe
nasm out.s -f win32 -o out.o
gcc -m32 out.o -o a.exe
echo "compiled" >&2
./a.exe

