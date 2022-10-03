IVERILOG_FLAGS  += -Wall
VERILATOR_FLAGS += --lint-only -Wall

%_tb: j1.v %_tb.v
	iverilog -o $@ $(IVERILOG_FLAGS) -D TESTBENCH $^ 

%.bin: %.mem top_tb.v
	iverilog -o $@ $(IVERILOG_FLAGS) -D FILE=\"$<\" top_tb.v j1.v

.PRECIOUS: %.mem
%.mem: %.fs compiler
	runghc compiler/j1.hs < $< > $@

compiler: compiler/*.hs

.PHONY: lint
lint: j1.v top_tb.v
	verilator $(VERILATOR_FLAGS) $^

clean:
	rm */*.bin fs/*.mem
