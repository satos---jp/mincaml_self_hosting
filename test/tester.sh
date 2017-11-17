if ! (cd ..; make > /dev/null); then
	echo "make failed"
	exit 1
fi
cp ../main ./
cp ../lib.ml ./
cp ../lib.s ./
cp ../libio_win.s ./
cp ../libio_linux.s ./

cat test_order.txt | while read file
do
	if [ -z $file ]; then 
		break
	fi
	echo "---------------" $file "----------------"
	cat $file
	cat test_header.ml > tmp.ml
	cat $file >> tmp.ml
	ocaml tmp.ml > oo.txt
	rm out.s out.o a.out
	./main -d $file -noinline > /dev/null
	nasm out.s -f elf32 -g -o out.o
	gcc -m32 -nostdlib out.o -o a.out
	./a.out > oa.txt
	if diff oo.txt oa.txt; then
		echo "ok"
	else
		echo "test failed"
		break
	fi
	rm tmp.ml oo.txt oa.txt
done
