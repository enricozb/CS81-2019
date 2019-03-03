#
# Makefile for sexpr library.
#

default: repl.cma

#
# Variables.
#

OCAMLLEX   			 = ocamllex
OCAMLYACC  			 = menhir
OCAMLYACC_FLAGS  = --table --explain
OCAMLC	   			 = ocamlfind ocamlc

FLAGS = -package menhirLib

SRC	       = loc ast parser lexer repl
INTERFACES = $(patsubst %,%.cmi,${SRC})
OBJS       = $(patsubst %,%.cmo,${SRC})
LIB        = repl.cma

#
# Compilation targets.
#

${LIB}: ${INTERFACES} ${OBJS}
	${OCAMLC} -a ${OBJS} -o $@

lexer.ml: lexer.mll parser.mli
	$(OCAMLLEX) lexer.mll

parser.ml: parser.mly
	$(OCAMLYACC) $(OCAMLYACC_FLAGS) parser.mly

parser.mli: parser.mly
	$(OCAMLYACC) $(OCAMLYACC_FLAGS) parser.mly

%.cmi: %.mli
	${OCAMLC} ${FLAGS} -c $^

%.cmo: %.ml %.cmi
	${OCAMLC} ${FLAGS} -c $<

lexer.cmo: parser.cmo loc.cmo
parser.cmo: loc.cmo ast.cmo
repl.cmo: parser.cmo lexer.cmo ast.cmo loc.cmo


#
# Cleanup.
#

clean:
	rm -f *.cmo *.cmi *.cma parser.mli parser.ml lexer.ml
	rm parser.conflicts

