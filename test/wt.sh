if ! (cd ..; make > /dev/null); then
	echo "make failed"
	exit 1
fi
cp ../main ./
cp ../main.exe ./
cp ../lib.ml ./
cp ../lib.s ./
cp ../libio_win.s ./
cp ../libio_linux.s ./

cat test_order.txt | while read file
do
	if [ -z $file ]; then 
		break
	fi
	sleep 0.1
	echo "---------------" $file "----------------"
	cat $file
	cat test_header.ml > tmp.ml
	cat $file >> tmp.ml
	ocaml tmp.ml > oo.txt
	rm out.s out.o a a.exe
	./main -w -d $file > /dev/null
	nasm -f win32 -o out.o -g out.s
	gcc -m32 out.o -o a.exe
	./a.exe > oa.txt
	if diff oo.txt oa.txt; then
		echo "ok"
	else
		echo "test failed"
		break
	fi
	rm tmp.ml oo.txt oa.txt
done
