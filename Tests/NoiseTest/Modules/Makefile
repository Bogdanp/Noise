mods.zo: *.rkt
	raco ctool \
		--runtime runtime \
		--mods $@ \
		fib.rkt http.rkt loud.rkt bytes.rkt

.PHONY: clean
clean:
	rm -f mods.zo
