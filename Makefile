SOURCES = syntax.ml lexer.mll parser.mly type_checker.ml source2ast.ml knorm.ml main.ml 
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile
