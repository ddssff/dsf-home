LIBS = unix pcre mysql unixutils los log dbweb
OLFLAGS = -g $(patsubst %, %.cma, $(LIBS))
OXFLAGS = $(patsubst %, %.cmxa, $(LIBS))

OCFLAGS = $(patsubst %, -I +%, $(LIBS))

ML = debchangelog.ml debversion.ml get_changelog.ml
CMO = $(patsubst %.ml, %.cmo, $(ML))
CMX = $(patsubst %.ml, %.cmx, $(ML))

all: get_changelog get_changelog.opt

get_changelog: $(CMO) Makefile
	ocamlc $(OCFLAGS) $(OLFLAGS) $(CMO) -o $@

get_changelog.opt: $(CMX) Makefile
	ocamlopt $(OCFLAGS) $(OXFLAGS) $(CMX) -o $@

test:
	./get_changelog.opt los-detector 7.0.53 7.0.57

%.cmo: %.ml Makefile
	ocamlc -c -g $(OCFLAGS) $*.ml

%.cmx: %.ml Makefile
	ocamlopt -c $(OCFLAGS) $*.ml

clean:
	rm -f *.o *.cm[oaxi] *.cmxa get_changelog get_changelog.opt *~