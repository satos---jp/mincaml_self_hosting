./main $1 -t -o o_tortesia.s -noinline -asi -v > o.txt && 
python tortesia2x86.py -r $2 < o_tortesia.s > out.s && 
if [ -z $2 ]; then
	./exec.sh
else
	./w.sh
fi

