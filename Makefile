#PREP=-pp "camlp4o pa_macro.cmo"
PREP=-pp "camlp4o pa_macro.cmo -DTRIP"
MLC=ocamlopt $(PREP)
INCLUDE=$(shell ocamlfind query -i-format csv ocamlgraph)
LIBS=str.cmxa csv.cmxa graph.cmxa unix.cmxa
SRCMLI=$(wildcard *.mli)
SRCML=heap.ml unionfind.ml utils.ml gtfs.ml label.ml dimacs9.ml quadtree.ml	transportgraph.ml trip.ml ps.ml timeprofile.ml ch.ml dijkstra.ml separator.ml test.ml main.ml
GTFS=.
TIME=/usr/bin/time -v
OPTIONS=-lab -n 100

all: vbb ratp stif

main: depend $(SRCMLI:.mli=.cmi) $(SRCML:.ml=.cmx)
	$(MLC) $(INCLUDE) $(LIBS) $(SRCML:.ml=.cmx) -o $@

%.cmi: %.mli
	$(MLC) $(INCLUDE) -c $<

%.cmx: %.ml
	$(MLC) $(INCLUDE) -c $<

stat:
	kill -s USR1 `pidof main`

clean:
	rm -f *.cmx *.cmi *.o

mrproper: clean
	rm -f main

depend:
	ocamldep $(PREP) -native *.mli *.ml > .depend

vbb: main
	$(TIME) ./main -gtfs $(GTFS)/vbb -d 20160810 -g $(GTFS)/vbb/graph -s $(GTFS)/vbb/graph_separator -cg $(GTFS)/vbb/graph_contracted -l $(GTFS)/vbb/labels $(OPTIONS) 2>&1 | tee $(GTFS)/vbb/stdout

ratp: main
	$(TIME) ./main -gtfs $(GTFS)/ratp -d 20160810 -g $(GTFS)/ratp/graph -s $(GTFS)/ratp/graph_separator -cg $(GTFS)/ratp/graph_contracted -l $(GTFS)/ratp/labels $(OPTIONS) 2>&1 | tee $(GTFS)/ratp/stdout

stif: main
	$(TIME) ./main -gtfs $(GTFS)/stif -d 20160810 -g $(GTFS)/stif/graph -s $(GTFS)/stif/graph_separator -cg $(GTFS)/stif/graph_contracted -l $(GTFS)/stif/labels $(OPTIONS) 2>&1 | tee $(GTFS)/stif/stdout

# require -DTRIP
ratposm: main
	$(TIME) ./main -gtfs $(GTFS)/ratp -d 20160810 -g $(GTFS)/ratp/osm_graph -s $(GTFS)/ratp/osm_graph_separator -cg $(GTFS)/ratp/osm_graph_contracted -dim $(OPTIONS) 2>&1 | tee $(GTFS)/ratp/osm_stdout

include .depend
.PHONY: all clean mrproper stat vbb ratp stif
