if ! (cd ..; make > /dev/null); then
	echo "make failed"
	exit 1
fi
cp ../main ./
cp ../lib.ml ./
cp ../lib.s ./
cp ../libio_win.s ./
cp ../libio_linux.s ./
cp ../lib_tortesia.ml ./
cp ../lib_tortesia.s ./
cp ../lib_tortesia_to_x86.s ./


exec_ocaml(){
	#Ocamlで実行
	cat test_header.ml > tmp.ml
	cat $1 >> tmp.ml
	if [ -z $3 ]; then 
		ocaml tmp.ml > $2
	else
		ocaml tmp.ml < $3 > $2
	fi
	rm out.s out.o a.out tmp.ml
}

exec_x86(){
	#x86で実行
	./main -d $1 ../globals.ml -noinline > /dev/null
	nasm out.s -f elf32 -g -o out.o
	gcc -m32 -nostdlib out.o -o a.out
	if [ -z $3 ]; then 
		./a.out > $2
	else
		./a.out < $3 > $2
	fi	
}

exec_tortesia(){
	#tortesia -> x86 で実行
	./main -d $1 ../globals.ml -noinline -t -o o.s > /dev/null
	python ../tortesia2x86.py < o.s > out.s
	nasm out.s -f elf32 -g -o out.o
	gcc -m32 -nostdlib out.o -o a.out
	if [ -z $3 ]; then 
		./a.out > $2
	else
		./a.out < $3 > $2
	fi
}


cat test_order.txt | while read file input
do
	if [ -z $file ]; then 
		break
	fi
	echo "---------------" $file "----------------"
	cat $file
	
	exec_x86 $file oo.txt $input 
	exec_tortesia $file oa.txt $input
	
	#比較
	if diff oo.txt oa.txt; then
		#cat oo.txt
		echo "ok"
	else
		echo "test failed"
		break
	fi
	rm oo.txt oa.txt
done
