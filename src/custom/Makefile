NAME = myth
INCS = repl/*.cmi
LIBS = repl.cma
LIBDIR = repl
FLAGS = -g -package zarith -package menhirLib -linkpkg -I $(LIBDIR) $(LIBS)

SRC = error env type value basis frame eval

MLI = $(patsubst %, %.mli, ${SRC})
ML  = $(patsubst %, %.ml , ${SRC}) main.ml
CMO = $(patsubst %, %.cmo, ${SRC}) main.cmo

default: compile

.PHONY: compile test clean

compile:
	ocamlfind ocamlc ${FLAGS} -I ${LIBDIR} -c ${MLI}
	ocamlfind ocamlc ${FLAGS} -I ${LIBDIR} -c ${ML}
	ocamlfind ocamlc ${FLAGS} -I ${LIBDIR} ${LIBS} ${CMO} -o ${NAME}

clean:
	rm -f ./${NAME}
	rm -f *.cmi *.cmo
