SOURCES = debug.ml syntax.ml lexer.mll parser.mly type_checker.ml source2ast.ml knorm.ml closure_conv.ml virtual.ml main.ml 
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile
