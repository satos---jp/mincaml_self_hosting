SOURCES = debug.ml main_option.ml linux_win_diff.ml genint.ml \
	syntax.ml spec.ml \
	lexer.mll parser.mly \
	preprocess.ml \
	source2ast.ml type.ml type_expr.ml type_checker.ml \
	knorm.ml lettuple2dest.ml \
	alpha.ml beta.ml inline.ml common_sube_elim.ml elim_unused.ml assoc.ml const_fold.ml \
	remove_tuple.ml \
	lambda_lift.ml closure_conv.ml \
	op.ml tortesia_register_convention.ml \
	cfg.ml virtual.ml \
	emit_zatsu_x86.ml \
	main.ml 
RESULT = ../../main

YFLAGS = -v
OCAMLFLAGS = 

all: byte-code byte-code-library

LIBS = str

-include OCamlMakefile

