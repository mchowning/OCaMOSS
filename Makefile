build:
	dune build bin/main.exe

run:
	dune exec bin/main.exe

test:
	dune test

clean:
	dune clean