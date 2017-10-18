SOURCES = debug.ml linux_win_diff.ml syntax.ml \
	lexer.mll parser.mly type_checker.ml source2ast.ml \
	knorm.ml beta.ml inline.ml common_sube_elim.ml \
	closure_conv.ml virtual.ml \
	emit_zatsu_x86.ml emit_zatsu_tortesia.ml \
	main.ml 
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile

