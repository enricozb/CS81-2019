NAME = nano_ml
INCS = ../sexpr/*.cmi
LIBS = sexpr.cma
LIBDIR = ../sexpr

SRC = error env type syntax basis eval

MLI = $(patsubst %, %.mli, ${SRC})
ML  = $(patsubst %, %.ml , ${SRC}) nano_ml.ml
CMO = $(patsubst %, %.cmo, ${SRC}) nano_ml.cmo

default: compile

.PHONY: compile test clean

compile:
	ocamlfind ocamlc -I ${LIBDIR} -c ${MLI}
	ocamlfind ocamlc -I ${LIBDIR} -c ${ML}
	ocamlfind ocamlc -I ${LIBDIR} ${LIBS} ${CMO} -o ${NAME}

test:
	python3 run_tests.py

clean:
	rm -f ./${NAME}
	rm -f *.cmi *.cmo
