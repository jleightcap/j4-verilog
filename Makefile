IVERILOG_FLAGS  += -Wall
VERILATOR_FLAGS += --lint-only -Wall
GHC_FLAGS       += -icompiler -Wall

%_tb: j4.v %_tb.v
	iverilog -o $@ $(IVERILOG_FLAGS) -D TESTBENCH $^ 

%.bin: %.mem top_tb.v
	iverilog -o $@ $(IVERILOG_FLAGS) -D FILE=\"$<\" top_tb.v j4.v

%.run: %.bin ; ./$^

.PRECIOUS: %.mem
%.mem: %.fs j4th
	./j4th < $< > $@

j4th: compiler/*.hs
	ghc $(GHC_FLAGS) compiler/j4.hs -o j4th

.PHONY: lint
lint: j4.v top_tb.v
	verilator $(VERILATOR_FLAGS) $^

clean:
	rm */*.bin fs/*.mem
