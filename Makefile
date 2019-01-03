#
# Create a program from the xml output of researchr
#
# To get the xml, enter edit mode, go to General Data / Data Export
# and click on ACM DL Mobile app XML

# XMLFILE=acmdlxml.xml
PDFFILE=out/main.pdf
XMLFILE=xml/popl19-info.xml
TEXFILE=tex/main
SCHEDULE=out/schedule.tex
OVERVIEW=out/overview.tex
LOG=log

program.pdf: $(TEXFILE).tex $(OVERVIEW) $(SCHEDULE)
	pdflatex -8bit $(TEXFILE)
	pdflatex -8bit $(TEXFILE) # for cross-references

$(SCHEDULE) : $(XMLFILE) schedule_parser
	./schedule_parser --schedule $(XMLFILE) > $@


$(OVERVIEW) : $(XMLFILE) schedule_parser
	./schedule_parser --overview $(XMLFILE) > $@

# The location for xml-light below is if you have installed ocaml through opam,
# otherwise you may have to change it
XML_LIGHT_DIR= `opam config var xml-light:lib`
schedule_parser: src/schedule_parser.ml
	ocamlc -I $(XML_LIGHT_DIR) xml-light.cma str.cma $< -o $@

# If you do not have Nicolas Cannasse's xml-light, but are using opam,
# the line below should be sufficient.
.PHONY: xml-light
xml-light:
	opam install xml-light

.PHONY: clean
clean:
	rm -f *~ program.pdf schedule-standalone.tex *.cm* schedule_parser
