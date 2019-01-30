#
# Makefile for sexpr library.
#

default: sexpr.cma lexer_test parser_test repl_test

#
# Variables.
#

OCAMLLEX   = ocamllex
OCAMLC	   = ocamlc

SRC	       = loc sexpr lexer parser repl
INTERFACES = $(patsubst %,%.cmi,${SRC})
OBJS       = $(patsubst %,%.cmo,${SRC})
LIB        = sexpr.cma

#
# Compilation targets.
#

${LIB}: ${INTERFACES} ${OBJS}
	${OCAMLC} -a ${OBJS} -o $@

lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

%.cmi: %.mli
	${OCAMLC} -c $^

%.cmo: %.ml %.cmi
	${OCAMLC} -c $<

lexer.cmo: loc.cmo
parser.cmo: lexer.cmo sexpr.cmo loc.cmo
repl.cmo: parser.cmo lexer.cmo sexpr.cmo loc.cmo


lexer_test: lexer_test.ml lexer.cmo loc.cmo
	${OCAMLC} -c $<
	${OCAMLC} -o lexer_test loc.cmo lexer.cmo lexer_test.cmo

parser_test: parser_test.ml parser.cmo lexer.cmo sexpr.cmo loc.cmo
	${OCAMLC} -c $<
	${OCAMLC} -o parser_test loc.cmo sexpr.cmo \
	  lexer.cmo parser.cmo parser_test.cmo

repl_test: repl_test.ml sexpr.cma
	${OCAMLC} -c $<
	${OCAMLC} -o repl_test sexpr.cma repl_test.cmo


#
# Cleanup.
#

clean:
	rm -f *.cmo lexer.cmi lexer.ml
	rm -f lexer_test parser_test repl_test 

realclean: clean
	rm -f *.cmi *.cma

