IVERILOG_FLAGS  += -Wall -D TESTBENCH
GHC_FLAGS       += -icompiler -Wall

.PHONY: j4th
j4th: compiler/*.hs
	ghc $(GHC_FLAGS) compiler/j4.hs -o j4th

.PHONY: test
test:
	@./test.sh

.PHONY: lint
lint:
	verilator -DTESTBENCH --lint-only -Wall *.v

%_tb: j4.v %_tb.v
	iverilog -o $@ $(IVERILOG_FLAGS) $^

%.bin: %.mem j4_tb.v
	iverilog -o $@ $(IVERILOG_FLAGS) -D FILE=\"$<\" j4_tb.v j4.v

%.run: %.bin ; ./$^

.PRECIOUS: %.mem
%.mem: %.fs j4th
	./j4th < $< > $@

clean:
	rm -f */*.bin fs/*.mem
