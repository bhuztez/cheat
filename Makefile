CHT_SRCS = $(wildcard examples/*.cht)
ELFS = $(CHT_SRCS:examples/%.cht=bin/%)

.PHONY: all clean

all: $(ELFS)

bin/%: examples/%.cht | bin
	./cheatc "$<" | gcc -g -o "$@" -Wall -Wextra -Wno-unused-label -Wno-varargs -x c -

bin:
	mkdir bin

clean:
	rm -f $(ELFS)
