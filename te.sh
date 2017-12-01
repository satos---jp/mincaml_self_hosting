./main $1 -t -o t.s -noinline
python tortesia2x86.py < t.s > out.s
./exec.sh

