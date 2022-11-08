COMPILER_BIN := j4th

IVERILOG_FLAGS += -Wall -D TESTBENCH
GHC_FLAGS      += -icompiler -Wall

TESTS := $(wildcard bin/*.awk) $(wildcard fs/*.awk)

$(COMPILER_BIN): compiler/*.hs
	ghc $(GHC_FLAGS) compiler/j4.hs -o $(COMPILER_BIN)

.PHONY: lint
lint:
	verilator -DTESTBENCH --lint-only -Wall *.v
	hlint compiler/

.PHONY: test
test: $(TESTS:awk=test)

%.test: %.bin %.awk
	# see: https://unix.stackexchange.com/a/589866
	./$< | awk -f $(basename $<).awk | grep . > /dev/null

%_tb: j4.v %_tb.v
	iverilog -o $@ $(IVERILOG_FLAGS) $^

%.bin: %.mem j4_tb.v
	iverilog -o $@ $(IVERILOG_FLAGS) -D FILE=\"$<\" j4_tb.v j4.v

%.run: %.bin ; ./$^

.PRECIOUS: %.mem
%.mem: %.fs j4th
	./$(COMPILER_BIN) < $< > $@

.PHONY: clean
clean:
	rm -f */*.bin fs/*.mem j4_tb $(COMPILER_BIN)
