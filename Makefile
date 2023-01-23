
EXE=_build/default/main.exe

all: $(EXE) rapport.pdf

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ngoc 

test: $(EXE)
	./tests/exec_tests.sh

rapport.pdf: rapport.tex
	pdflatex rapport.tex > /dev/null

export-%:
	cp test.go ../tests/exec/$*.go
	go run test.go > ../tests/exec/$*.out

.PHONY: clean
clean:
	dune clean
	rm -f ngoc *.aux *.pdf *.log
