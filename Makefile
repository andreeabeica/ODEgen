CMO=lexer.cmo parser.cmo  main.cmo
GENERATED = lexer.ml parser.ml parser.mli
BIN=compilateur
FLAGS=str.cma

all:
	ocamlbuild -libs str -use-menhir main.native
	mv main.native ODEgen

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly 

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

.mly.mli:
	ocamlyacc -v $<

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED)
	rm -f parser.output parser.automaton parser.conflicts
	rm -f  .depend
	rm -f ODEgen

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend
