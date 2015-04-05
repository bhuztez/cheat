ifndef test
SRCS=$(wildcard code/*.cht)
BINS=$(SRCS:code/%.cht=build/%.elf)
TESTS=$(SRCS:code/%.cht=%)

.PRECIOUS: $(BINS)

all: $(TESTS)

clean:
	rm -rf $(BINS)

build/%.elf: code/%.cht
	@echo "COMPILE $(@:build/%.elf:%)"
	@../cheatc "$<" | gcc -g -o "$@" -Wall -Wextra -Wno-unused-label -Wno-varargs -x c -

%: build/%.elf
	@echo "CHECK $@"
	@make -s 'test=$@'

else
TESTCASES=$(wildcard testcase/$(test)/*.in.*)
TESTS=$(TESTCASES:testcase/$(test)/$(test).in.%=%)

all: $(TESTS)
	echo "  $(test) OK"

%:
	echo "  $(test).in.$@"
	cat "testcase/$(test)/$(test).in.$@" | "build/$(test).elf" | diff "testcase/$(test)/$(test).out.$@" -
endif
