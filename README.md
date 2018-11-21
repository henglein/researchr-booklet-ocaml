# researchr-booklet-ocaml
Scripts to generate LaTeX schedule from researchr.org exported XML

## Getting Started

To get started, do

```
$ opam init 
$ make xml-light
$ make
``` 

this will get you to "regenerate" the popl18 brochure as a start :) 

## Customizing 

To customize for your conference, e.g. POPL19:

1. UPDATE the various TEX files in `tex/*.tex` with the POPL19 information

2. DOWNLOAD the POPL19 xml file from researchr.
 
3. EDIT the script in `src/schedule_parser.ml` 
    to have the sessions, chairs, event information for your conference.

    There is some trick to (2) that escapes me now, 
    but the web-chair should be able to get it (there 
    was some clicking around on the website involved).  

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

Pull requests most welcome!

