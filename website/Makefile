EXDIR = ../examples

EXLGC := $(shell find $(EXDIR) -name *.lgc)
EXPDF := $(EXLGC:.lgc=.pdf)
MLVER := $(shell awk 'BEGIN{FS=";"}$$1=="ml"{print $$2}' ../Version)
ILCVER := $(shell awk 'BEGIN{FS=";"}$$1=="ilc"{print $$2}' ../Version)

.FORCE:
.PHONY: .FORCE

all: index.html examples manual

examples: $(EXPDF)

manual: ../doc/multlog.pdf ../doc/multlog.html

../doc/multlog.tex: ../doc/multlog.md ../Version
	pandoc --template=$< </dev/null -V ml_version="$(MLVER)" -V ilc_version="$(ILCVER)" --metadata pagetitle="MUltlog $(MLVER)$ & iLC $(ILCVER) Manual" | pandoc --standalone --toc -N --toc-depth=2 --output=$@

../doc/multlog.html: ../doc/multlog.md ../Version
	pandoc --template=$< </dev/null -V ml_version="$(MLVER)" -V ilc_version="$(ILCVER)" --metadata pagetitle="MUltlog $(MLVER)$ & iLC $(ILCVER) Manual" | pandoc --standalone --toc -N --toc-depth=1 -c https://www.logic.at/user/themes/logic/assets/css/main.css -B before.html -A after.html -H header.html --output=$@

../doc/multlog.pdf: ../doc/multlog.tex
	pdflatex -output-directory $(@D) $< 
	pdflatex -output-directory $(@D) $< 

$(EXDIR)/%.pdf: .FORCE
	cd $(EXDIR); \
	lgc2pdf $(notdir $*)

index.html: index.md website.md ../Version ../ml/ml.bib
	pandoc --template=index.md  -V ml_version="$(MLVER)" \
	-V ilc_version="$(ILCVER)" --metadata pagetitle="website" website.md | \
	pandoc -s --bibliography=../ml/ml.bib --metadata link-citations -c https://www.logic.at/user/themes/logic/assets/css/main.css -B before.html -A after.html  -H header.html --output=index.html

update:
	scp -o PubkeyAuthentication=false index.html ../examples/*.pdf ../doc/multlog.html ../doc/multlog.pdf rick.logic.at:web/www/multlog/