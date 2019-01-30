NAME = uscheme
LIBS  = sexpr.cma
LIBDIR = sexpr
OCAMLC = ocamlfind ocamlc
INCLUDE_FLAGS = -I $(LIBDIR) $(LIBS)

MODULES = error syntax env type basis eval
CMO = $(addsuffix .cmo, $(MODULES))
CMI = $(addsuffix .cmi, $(MODULES))

default: compile

.PHONY: compile test clean

compile: $(CMO)
	$(OCAMLC) $(INCLUDE_FLAGS) $(CMO) -o $(NAME)

test: compile
	python3 run_tests.py

clean:
	rm -f ./$(NAME)
	rm -f *.cmi *.cmo

basis.cmo: basis.mli basis.ml env.cmo error.cmo type.cmo
	$(OCAMLC) $(INCLUDE_FLAGS) -c $^

env.cmo: env.mli env.ml error.cmo syntax.cmo
	$(OCAMLC) $(INCLUDE_FLAGS) -c $^

error.cmo: error.mli error.ml
	$(OCAMLC) $(INCLUDE_FLAGS) -c $^

syntax.cmo: syntax.ml syntax.mli error.cmo
	$(OCAMLC) $(INCLUDE_FLAGS) -c syntax.mli syntax.ml

type.cmo: type.ml type.mli error.cmo syntax.cmo
	$(OCAMLC) $(INCLUDE_FLAGS) -c type.mli type.ml

eval.cmo: eval.ml error.cmo syntax.cmo env.cmo type.cmo basis.cmo
	$(OCAMLC) $(INCLUDE_FLAGS) -c eval.ml
