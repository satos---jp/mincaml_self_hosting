rm ./srcs/* -f
cp ../src/lex/*.ml ./srcs/
cp ../src/lex/*.mli ./srcs/
cp ../src/lex/*.mly ./srcs/
cp ../src/lex/*.mll ./srcs/
rm ./srcs/lexer.ml
rm -f ./srcs/lexer.mli 
rm ./srcs/parser.ml
rm ./srcs/parser.mli
rm ./srcs/nfa.ml
rm ./srcs/nfa.mli
(cd ./srcs;
patch main.ml < ../patch_main.patch;
); 

cd ./build
rm *
cp ../srcs/* ./
../../my_lex lexer.mll > lexer.ml
../../my_yacc parser.mly > myparser.output
#../../main -mli lexer.ml -l ../../lib/ # 若干Openまわりが壊れてるのでいったん諦める
cp ../lexer.mli lexer.mli
../../main syntax.ml parser.ml lexer.ml convert.ml main.ml -l ../../lib

mv a.out ../a.out

