all: schema.png

%.png: %.gv
	dot -Tpng -o $@ $<

%.gv: %.gvs
	gvs2gv $<

watch:
	ls -1 schema.gvs | entr -c make
