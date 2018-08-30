SOURCES = util.ml matrix.ml function.ml parser.mly lexer.mll graph.ml html.ml main.ml
SOURCESHTML = index.ml

EXEC = project.exe

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
CAMLFIND = ocamlfind
JCAML = js_of_ocaml
JCAMLP4 = js_of_ocaml-camlp4
CAMLP4 = camlp4o

LIBS = $(WITHGRAPHICS) $(WITHSTR) $(WITHUNIX)

WITHGRAPHICS = graphics.cma
WITHSTR = str.cma
WITHUNIX = unix.cma

CUSTOM = -custom

all: depend $(EXEC)

SOURCES1 = $(SOURCES:.mly=.ml)
SOURCES2 = $(SOURCES1:.mll=.ml)
SOURCES1HTML = $(SOURCESHTML:.ml=.bytes)
OBJS = $(SOURCES2:.ml=.cmo)

$(EXEC): $(OBJS)
	$(CAMLFIND) $(CAMLC) -package $(JCAML) -package $(JCAMLP4) -linkpkg $(CUSTOM) $(LIBS) -o $(EXEC) $(OBJS)
	@echo "\033[0;34m[Creation]\033[0m" html interface
	$(CAMLFIND) $(CAMLC) -package $(JCAML) -package $(JCAMLP4) -linkpkg -syntax $(CAMLP4) $(LIBS) -o $(SOURCES1HTML) $(OBJS) $(SOURCESHTML)
	@$(JCAML) $(SOURCES1HTML)
	@echo "\033[0;32m[Project complete]\033[0m" $(EXEC)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLFIND) $(CAMLC) -c -package $(JCAML) -package $(JCAMLP4) -syntax $(CAMLP4) $<

.mli.cmi:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLC) -c $<

.ml.cmx:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLOPT) -c $<

.mll.cmo:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLLEX) $<
	@$(CAMLC) -c $*.ml

.mll.cmx:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLLEX) $<
	@$(CAMLOPT) -c $*.ml

.mly.cmo:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLYACC) -v $<
	@$(CAMLC) -c $*.mli
	@$(CAMLC) -c $*.ml

.mly.cmx:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLYACC) -v $<
	@$(CAMLOPT) -c $*.mli
	@$(CAMLOPT) -c $*.ml

.mly.cmi:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLYACC) $<
	@$(CAMLC) -c $*.mli

.mll.ml:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLLEX) $<

.mly.ml:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLYACC) $<

clean:
	@echo "\033[0;31m[Deleting]\033[0m" *.cm[iox]
	@rm -f *.cm[iox]
	@echo "\033[0;31m[Deleting]\033[0m" $(EXEC)
	@rm -f $(EXEC)
	@echo "\033[0;31m[Deleting]\033[0m" lexer.ml
	@rm -f lexer.ml
	@echo "\033[0;31m[Deleting]\033[0m" *.o
	@rm -f *.o
	@echo "\033[0;31m[Deleting]\033[0m" parser.ml
	@rm -f parser.ml
	@echo "\033[0;31m[Deleting]\033[0m" parser.mli
	@rm -f parser.mli
	@echo "\033[0;31m[Deleting]\033[0m" *.js
	@rm -f *.js
	@echo "\033[0;31m[Deleting]\033[0m" *.bytes
	@rm -f *.bytes
	@echo "\033[0;31m[Deleting]\033[0m" *.html
	@rm -f *.html

depend: $(SOURCES2)
	@echo "\033[0;35m[Dependance]\033[0m" "*.mli *.ml > .depend"
	@$(CAMLFIND) $(CAMLDEP) -package $(JCAMLP4) -syntax $(CAMLP4) *.mli *.ml > .depend

include .depend