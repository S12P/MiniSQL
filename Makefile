all:
	obuild configure
	obuild build

init:
	opam switch 4.06.1
	eval `opam config env`
	opam install obuild csv


clean:
	rm minisql; rm -rf dist/
