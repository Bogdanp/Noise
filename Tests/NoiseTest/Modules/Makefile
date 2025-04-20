.PHONY: all
all: deps mods.zo

.PHONY: deps
deps:
	raco pkg install --auto --skip-installed http-easy-lib

mods.zo: *.rkt
	raco ctool \
		--runtime runtime \
		--mods $@ \
		callout.rkt fib.rkt http.rkt loud.rkt bytes.rkt

.PHONY: clean
clean:
	rm -f mods.zo
