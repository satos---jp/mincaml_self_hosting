if ! (cd ../src; make > /dev/null); then
	echo "make failed"
	exit 1
fi

cp ../main ./
cp ../lib/ ./ -r

exec_ocaml(){
	#Ocamlで実行
	garr=$1
	output=$2
	input=$3
	len=${#garr[@]}
	if [ ${#garr[@]} -le 1 ]; then
		cat test_header.ml > tmp.ml
		cat $1 >> tmp.ml
		if [ -z $input ]; then 
			ocaml tmp.ml > $output
		else
			ocaml tmp.ml < $input > $output
		fi
	else
		for i in `seq 0 $(($len-1))`; do
			arr[$i]=${garr[$i]}
		done

		cat test_header.ml > tmp.ml
		cat ${arr[$len-1]} >> tmp.ml
		arr[$len-1]="tmp.ml"
		echo "${arr[@]}"
		
		for i in `seq 0 $(($len-1))`; do
			arr[$i]=`basename ${arr[$i]} .ml`
		done
		for f in ${arr[@]}; do
			if [ -f $f.mli ]; then
				ocamlc -c $f.mli
			fi
		done
		for f in ${arr[@]}; do
			ocamlc -c $f.ml
		done
		for i in `seq 0 $(($len-1))`; do
			arr[$i]=${arr[$i]}.cmo
		done
		ocamlc "${arr[@]}" -o ocamlout.out
		if [ -z $input ]; then
			./ocamlout.out > $output
		else
			./ocamlout.out < $input > $output
		fi
		rm *.cmi
		rm *.cmo
	fi
}

exec_x86(){
	#x86で実行
	arr=$1
	output=$2
	input=$3
	./main -d $1 -noinline -o o_x86.s > /dev/null
	nasm o_x86.s -f elf32 -g -o out.o
	gcc -m32 -nostdlib out.o -o x86.out
	if [ -z $input ]; then 
		./x86.out > $output
	else
		./x86.out < $input > $output
	fi	
}

hoge(){
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
}

cat test_order.txt | while read fs
do
	arr=($fs)
	len=${#arr[@]}
	#echo $len
	input=""
	if [ $len -ge 2 ] && [ ${arr[$len-2]} == "-i" ] ; then
		input=${arr[$len-1]}
		arr=(${arr[@]:0:$len-2})
		len=${#arr[@]}
	elif [ $len = 0 ]; then 
		break
	fi
	
	
	rm -f tmp.ml a.out o_tortesia.s o_x86.s o_tortesia2x86.s x86.out
	echo "---------------" $fs "----------------"
	for f in ${arr[@]}; do
		echo "----------[" $f "]------------"
		echo ""
		cat $f
	done
	
	rm -f o_ocaml.txt o_mincaml.txt
	
	
	exec_ocaml $arr o_ocaml.txt $input
	exec_x86 $arr o_mincaml.txt $input
	
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
