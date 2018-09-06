if ! (cd ../src; make > /dev/null); then
	echo "make failed"
	exit 1
fi

cp ../main ./
cp ../lib/ ./ -r

exec_ocaml(){
	#Ocamlで実行
	arr=$1
	output=$2
	input=$3
	len=${#arr[@]}
	if [ $len -le 1 ]; then
		cat test_header.ml > tmp.ml
		cat $1 >> tmp.ml
		if [ -z $input ]; then 
			ocaml tmp.ml > $output
		else
			ocaml tmp.ml < $input > $output
		fi
	else
		for i in `seq 0 $(($len-1))`; do
			tarr[$i]=${arr[$i]}
		done
		
		cat test_header.ml > tmp.ml
		cat ${tarr[$len-1]} >> tmp.ml
		tarr[$len-1]="tmp.ml"
		
		for i in `seq 0 $(($len-1))`; do
			tarr[$i]=`basename ${tarr[$i]} .ml`
		done
		for f in ${tarr[@]}; do
			if [ -f $f.mli ]; then
				ocamlc -c $f.mli
			fi
		done
		for f in ${tarr[@]}; do
			ocamlc -c $f.ml
		done
		for i in `seq 0 $(($len-1))`; do
			tarr[$i]=${tarr[$i]}.cmo
		done
		ocamlc "${tarr[@]}" -o ocamlout.out
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
	./main "${arr[@]}" -noinline -o x86.out -asi > /dev/null
	if [ -z $input ]; then 
		./x86.out > $output
	else
		./x86.out < $input > $output
	fi
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
	
	rm -f tmp.ml a.out o_tortesia.s x86.out
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
	rm -f *.o
	rm -f *.s
	#break
done


rm -r lib
rm -f tmp.ml
