rm ./srcs/* -f
#exit
cp ../src/lex/*.ml ./srcs/
cp ../src/lex/*.mli ./srcs/
cp ../src/lex/*.mly ./srcs/
cp ../src/lex/*.mll ./srcs/
#rm ./srcs/lexer.ml
#rm ./srcs/parser.ml
#rm ./srcs/parser.mli
#rm ./srcs/nfa.ml
#rm ./srcs/nfa.mli

#exit

cd ./build
cp ../srcs/* ./
#exit
../../my_lex lexer.mll > lexer.ml
#exit
#exit
#../../my_yacc parser.mly > myparser.output
#../../main -s parser.ml -l ../../lib

#../../main syntax.ml parser.ml lexer.ml convert.ml main.ml -l ../../lib
../../main -s lexer.ml -l ../../lib
#OCAMLRUNPARAM=p ../../main -s lexer.ml -l ../../lib
#../../main -s syntax.ml -l ../../lib
#OCAMLRUNPARAM=p ../../main -s convert.ml -l ../../lib
#OCAMLRUNPARAM=p ../../main -s main.ml -l ../../lib

exit

(cd ../../lib/ml;
nasm nfa.s -f elf32 -g -o nfa.o;
../../main -s parsing.ml -l ../;
#../../main -s pervasive.ml -l ../;
#../../main -s list.ml -l ../;
../../main -s lexing.ml -l ../;
nasm parsing.s -f elf32 -g -o parsing.o;
nasm list.s -f elf32 -g -o list.o;
nasm lexing.s -f elf32 -g -o lexing.o;
nasm string.s -f elf32 -g -o string.o;
nasm pervasive.s -f elf32 -g -o pervasive.o
);
(cd ../../lib/asm;
nasm char.s -f elf32 -g -o char.o;
nasm string.s -f elf32 -g -o string.o;
nasm pervasive.s -f elf32 -g -o pervasive.o;
nasm libio_linux.s -f elf32 -g -o libio_linux.o
);

nasm syntax.s -f elf32 -g -o syntax.o
nasm lexer.s -f elf32 -g -o lexer.o
nasm parser.s -f elf32 -g -o parser.o
nasm eval.s -f elf32 -g -o eval.o
nasm main.s -f elf32 -g -o main.o
nasm stub.s -f elf32 -g -o stub.o
gcc -m32 -nostdlib ../../lib/ml/nfa.o ../../lib/ml/parsing.o ../../lib/ml/list.o ../../lib/ml/lexing.o ../../lib/ml/string.o ../../lib/ml/pervasive.o syntax.o lexer.o parser.o eval.o main.o ../../lib/asm/char.o ../../lib/asm/string.o ../../lib/asm/pervasive.o ../../lib/asm/libio_linux.o stub.o -o a.out
mv a.out ../a.out

