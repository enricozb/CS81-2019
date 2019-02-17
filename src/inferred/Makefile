NAME = nano_ml
INCS = repl/*.cmi
LIBS = repl.cma
LIBDIR = repl
FLAGS = -package menhirLib -linkpkg -I $(LIBDIR) $(LIBS)

SRC = error env type syntax basis eval

MLI = $(patsubst %, %.mli, ${SRC})
ML  = $(patsubst %, %.ml , ${SRC}) nano_ml.ml
CMO = $(patsubst %, %.cmo, ${SRC}) nano_ml.cmo

default: compile

.PHONY: compile test clean

compile:
	ocamlfind ocamlc ${FLAGS} -I ${LIBDIR} -c ${MLI}
	ocamlfind ocamlc ${FLAGS} -I ${LIBDIR} -c ${ML}
	ocamlfind ocamlc ${FLAGS} -I ${LIBDIR} ${LIBS} ${CMO} -o ${NAME}

test:
	python3 run_tests.py

clean:
	rm -f ./${NAME}
	rm -f *.cmi *.cmo
