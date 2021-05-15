EXDIR = ../examples

EXLGC := $(shell find $(EXDIR) -name *.lgc)
EXPDF := $(EXLGC:.lgc=.pdf)
MLVER := $(shell awk 'BEGIN{FS=";"}$$1=="ml"{print $$2}' ../Version)
ILCVER := $(shell awk 'BEGIN{FS=";"}$$1=="ilc"{print $$2}' ../Version)

.FORCE:
.PHONY: .FORCE

all: index.html examples manual

examples: $(EXPDF)

manual: ../doc/multlog.pdf

../doc/multlog.tex: ../doc/multlog.md ../Version
	pandoc --template=$< </dev/null -V ml_version="$(MLVER)" -V ilc_version="$(ILCVER)" --metadata pagetitle="MUltlog $(MLVER)$ & iLC $(ILCVER) Manual" | pandoc --standalone --toc -N --output=$@

../doc/multlog.pdf: ../doc/multlog.tex
	pdflatex -output-directory $(@D) $< 
	pdflatex -output-directory $(@D) $< 

$(EXDIR)/%.pdf: .FORCE
	cd $(EXDIR); \
	lgc2pdf $(notdir $*)

index.html: index.md website.md ../Version ../ml/ml.bib
	pandoc --template=index.md  -V ml_version="$(MLVER)" \
	-V ilc_version="$(ILCVER)" --toc website.md | \
	pandoc -s --bibliography=../ml/ml.bib --metadata link-citations --toc --output=index.html
