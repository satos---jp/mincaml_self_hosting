SOURCES = nfa.ml syntax.ml \
	lexer.mll parser.mly convert.ml \
	main.ml 
RESULT = ../../my_lex

YFLAGS = -v
OCAMLFLAGS = 



all: pre_copy byte-code byte-code-library

LIBS = str

-include OCamlMakefile

pre_copy:
	rm -f nfa.ml
	rm -f nfa.mli
	cp ../../lib/ml/nfa.ml ./
	cp ../../lib/ml/nfa.mli ./

