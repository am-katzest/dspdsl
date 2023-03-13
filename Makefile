.PHONY: sprawko/sprawozdanie.pdf

sprawko/sprawozdanie.pdf: sprawko/sprawozdanie.tex # snippets
	cd sprawko; pdflatex -shell-escape sprawozdanie
	cd sprawko; bibtex sprawozdanie
	cd sprawko; pdflatex -shell-escape sprawozdanie
	cd sprawko; pdflatex -shell-escape sprawozdanie

#snippets:
#	-rm -r sprawko/snippets
#	mkdir sprawko/snippets
#	./snippets sprawko/snippets/ src/mh1/alg.clj
# end
