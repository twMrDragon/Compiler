
all: miniturtle.exe
	dune exec ./miniturtle.exe test.logo

tests: miniturtle.exe
	for f in tests/*.logo; do dune exec ./miniturtle.exe $$f; done

tests_windows:
	for %%f in (tests\*.logo) do dune exec ./miniturtle.exe %%f

miniturtle.exe:
	dune build miniturtle.exe

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

clean:
	dune clean

.PHONY: all clean explain miniturtle.exe
