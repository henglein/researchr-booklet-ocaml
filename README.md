# researchr-booklet-ocaml
Scripts to generate LaTeX schedule from researchr.org exported XML

## Main Skeleton of Brochure

* tex/main.tex

## WRITE the following files

* tex/preface.tex
* tex/aec-report.tex
* tex/committees.tex
* tex/keynotes.tex 
* tex/social.tex 
* tex/sponsors.tex 

## GENERATE the following files

* out/overview.tex 
* out/schedule.tex

These above two files are "generated" by the script 

* `src/schedule_parser.ml`

which has a bunch of conference-specific information thats 
visible in the script itself, AND takes as input the actual
conference schedule information in the `.xml` file

* `xml/popl18-info.xml` 

that you can "export" from the researchr website.


