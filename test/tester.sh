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
cp ../lib_zatsu_tortesia.s ./
cp ../lib_tortesia_to_x86.s ./
cp ../lib_sinint.ml ./


exec_ocaml(){
	#Ocamlで実行
	cat test_header.ml > tmp.ml
	cat $1 >> tmp.ml
	if [ -z $3 ]; then 
		ocaml tmp.ml > $2
	else
		ocaml tmp.ml < $3 > $2
	fi
}

exec_x86(){
	#x86で実行
	./main -d $1 -noinline -o o_x86.s > /dev/null
	nasm o_x86.s -f elf32 -g -o out.o
	gcc -m32 -nostdlib out.o -o x86.out
	if [ -z $3 ]; then 
		./x86.out > $2
	else
		./x86.out < $3 > $2
	fi	
}

exec_tortesia_zatsu(){
	#tortesia -> x86 で実行
	./main -d $1 -noinline -t -asi -stack -o o_tortesia.s > /dev/null
	python ../tortesia2x86.py < o_tortesia.s > o_tortesia2x86.s
	nasm o_tortesia2x86.s -f elf32 -g -o out.o
	gcc -m32 -nostdlib out.o -o a.out
	if [ -z $3 ]; then 
		./a.out > $2
	else
		./a.out < $3 > $2
	fi
}

exec_tortesia(){
	#tortesia -> x86 で実行
	./main -d $1 -noinline -t -asi -o o_tortesia.s > /dev/null
	python ../tortesia2x86.py -r < o_tortesia.s > o_tortesia2x86.s
	nasm o_tortesia2x86.s -f elf32 -g -o out.o
	gcc -m32 -nostdlib out.o -o a.out
	if [ -z $3 ]; then 
		./a.out > $2
	else
		./a.out < $3 > $2
	fi
}

cat test_order.txt | while read file input
do
	rm tmp.ml a.out oo.txt oa.txt o_tortesia.s o_x86.s o_tortesia2x86.s
	if [ -z $file ]; then 
		break
	fi
	echo "---------------" $file "----------------"
	cat $file
	
	#exec_ocaml $file oa.txt $input
	exec_x86 $file oo.txt $input 
	#exec_tortesia_zatsu $file oa.txt $input
	exec_tortesia $file oa.txt $input
	
	#比較
	if diff oo.txt oa.txt; then
		#cat oo.txt
		echo "ok"
	else
		echo "test failed"
		break
	fi
	#break
done
