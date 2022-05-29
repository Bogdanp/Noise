mods.zo: *.rkt
	raco ctool \
		--runtime runtime \
		--mods $@ \
		fib.rkt http.rkt loud.rkt

.PHONY: clean
clean:
	rm mods.zo
