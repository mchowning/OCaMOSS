build:
	dune build bin/main.exe

exec:
	dune exec bin/main.exe -- $(arg1)

test:
	dune test

clean:
	dune clean
