CHT_SRCS = $(wildcard examples/*.cht)
ELFS = $(CHT_SRCS:examples/%.cht=bin/%)

.PHONY: all clean

all: $(ELFS)

bin/%: examples/%.cht | bin
	escript cheatc.erl "$<" | gcc -o "$@" -Wall -Wextra -Werror -Wno-unused-label -Wno-varargs -x c -

bin:
	mkdir bin

clean:
	rm -f $(ELFS)
