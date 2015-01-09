XRL_SRCS = $(wildcard src/*.xrl)
YRL_SRCS = $(wildcard src/*.yrl)
ERL_SRCS = $(wildcard src/*.erl)
ERLS = $(XRL_SRCS:src/%.xrl=src/%.erl) $(YRL_SRCS:src/%.yrl=src/%.erl)
BEAMS = $(XRL_SRCS:src/%.xrl=ebin/%.beam) $(YRL_SRCS:src/%.yrl=ebin/%.beam) $(ERL_SRCS:src/%.erl=ebin/%.beam)

.PHONY: all clean
.PRECIOUS: $(ERLS)

all: $(BEAMS)

src/%.erl: src/%.xrl
	erlc -o src "$<"

src/%.erl: src/%.yrl
	erlc -o src "$<"

ebin/%.beam: src/%.erl | ebin
	erlc -o ebin "$<"

ebin:
	mkdir ebin

clean:
	rm -f $(BEAMS) $(ERLS)
