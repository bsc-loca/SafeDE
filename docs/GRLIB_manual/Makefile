CC=pdflatex

all: pdfopen 

pdfopen: main.pdf
	evince main.pdf &

main.pdf: main.tex 1-Section.tex 2-Section.tex 3-Section.tex 4-Section.tex 5-Section.tex 6-Section.tex 
	$(CC) main.tex
	#$(CC) main.tex

clean:
	rm *.aux *.log *.blg *.bbl *.out

clear:
	rm *.aux *.log *.blg *.bbl *.out *.pdf 

