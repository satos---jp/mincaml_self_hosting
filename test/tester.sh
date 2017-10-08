(cd ..; make > /dev/null)
cp ../main.exe ./
cp ../lib.ml ./
cp ../lib.s ./
cp ../libio_win.s ./

cat test_order.txt | while read file
do
	echo "---------------" $file "----------------"
	cat $file
	cat test_header.ml > tmp.ml
	cat $file >> tmp.ml
	ocaml tmp.ml > oo.txt
	rm out.s out.o a.exe
	./main.exe $file > /dev/null
	nasm -f win32 -o out.o -g out.s
	gcc -m32 out.o
	a.exe > oa.txt
	if diff oo.txt oa.txt; then
		echo "ok"
	else
		echo "test failed"
		break
	fi
	rm tmp.ml oo.txt oa.txt
done
