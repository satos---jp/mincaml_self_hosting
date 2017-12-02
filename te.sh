./main $1 -t -o t.s -noinline
python tortesia2x86.py $2 < t.s > out.s
if [ -z $2 ]; then
	./exec.sh
else
	./w.sh
fi

