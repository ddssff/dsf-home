all: procstat.opt

OCDIRS := pcre los log unixutils debian netstring debutils detect
OCLIBS := unix $(OCDIRS)
OCFLAGS = $(patsubst %, -I +%, $(OCDIRS)) $(patsubst %, %.cma, $(OCLIBS))
OXFLAGS = $(patsubst %, -I +%, $(OCDIRS)) $(patsubst %, %.cmxa, $(OCLIBS))

OCDIRS := pcre
OCLIBS := unix $(OCDIRS)

procstat.opt: procstat.ml
	ocamlopt $(OXFLAGS) procstat.ml -o $@
