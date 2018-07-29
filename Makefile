SOURCES = util.ml matrix.ml function.ml graph.ml parser.mly lexer.mll main.ml

EXEC = project.exe

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

LIBS = $(WITHGRAPHICS) $(WITHSTR) $(WITHUNIX)

WITHGRAPHICS = graphics.cma
WITHSTR = str.cma
WITHUNIX = unix.cma

CUSTOM = -custom

all: depend $(EXEC)

opt : $(EXEC).opt

SOURCES1 = $(SOURCES:.mly=.ml)
SOURCES2 = $(SOURCES1:.mll=.ml)
OBJS = $(SOURCES2:.ml=.cmo)
OPTOBJS = $(SOURCES2:.ml=.cmx)

$(EXEC): $(OBJS)
	@$(CAMLC) $(CUSTOM) graphics.cma str.cma -o $(EXEC) $(OBJS)
	@echo "\033[0;32m[Project Complete]\033[0m" $(EXEC)
	
$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(EXEC) $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	@echo "\033[0;33m[Compiling]\033[0m" $<
	@$(CAMLC) -c $<

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

depend: $(SOURCES2)
	@echo "\033[0;33m[Dependance]\033[0m" *.mli *.ml > .depend
	@$(CAMLDEP) *.mli *.ml > .depend

include .depend