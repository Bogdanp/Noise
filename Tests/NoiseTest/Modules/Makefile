mods.zo: *.rkt
	raco ctool \
		--runtime runtime \
		--mods $@ \
		callout.rkt fib.rkt http.rkt loud.rkt bytes.rkt

.PHONY: clean
clean:
	rm -f mods.zo
