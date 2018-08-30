if ! (cd ../src; make > /dev/null); then
	echo "make failed"
	exit 1
fi

cp ../main ./
cp ../lib/ ./ -r

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

cat test_order.txt | while read file input option
do
	rm -f tmp.ml a.out o_tortesia.s o_x86.s o_tortesia2x86.s x86.out
	if [ -z $file ]; then 
		break
	fi
	echo "---------------" $file "----------------"
	cat $file
	
	rm -f o_ocaml.txt o_mincaml.txt
	
	
	if [ -z $option ]; then
		exec_ocaml $file o_ocaml.txt $input
		exec_x86 $file o_mincaml.txt $input 
	else
		dml=".ml"
		dmli=".mli"
		dcmo=".cmo"
		cat test_header.ml > tmp.ml
		cat $file$dml >> tmp.ml
		ocamlc -c $input$dmli
		ocamlc -c $input$dml
		ocamlc -c tmp.ml
		ocamlc -o ocaml.out $input$dcmo tmp.cmo
		./ocaml.out > o_ocaml.txt
		
		exec_x86 $file$dml o_mincaml.txt
	fi
	
	#比較
	if diff o_ocaml.txt o_mincaml.txt; then
		#cat oo.txt
		echo "ok"
	else
		echo "test failed"
		break
	fi
	#break
done


rm -r lib
rm -f tmp.ml
