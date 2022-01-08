build:
	dune build bin/main.exe

exec:
	dune exec bin/main.exe -- $(arg1)

test:
	dune test && dune exec bin/main.exe -- /tmp/test/test.js

clean:
	dune clean
