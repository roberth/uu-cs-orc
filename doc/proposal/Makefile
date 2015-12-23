
default: proposal.pdf

install:
	cp proposal.pdf $$out/

%.tex : %.lhs
	$$lhs2tex/bin/lhs2TeX $< >$@

%.pdf : %.tex
	$$texlive/bin/pdflatex $< </dev/null
	$$texlive/bin/pdflatex $< </dev/null
