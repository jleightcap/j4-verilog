IVERILOG_FLAGS  += -Wall
VERILATOR_FLAGS += --lint-only -Wall

%_tb: j1.v %_tb.v
	iverilog $(IVERILOG_FLAGS) -D TESTBENCH $^ -o $@

%.bin: %.mem top_tb.v
	iverilog $(IVERILOG_FLAGS) -D FILE=\"$<\" top_tb.v j1.v -o $@

.PRECIOUS: %.mem
%.mem: %.fs
	runghc assembler/asm.hs < $< > $@

.PHONY: lint
lint: j1.v top_tb.v
	verilator $(VERILATOR_FLAGS) $^

clean:
	rm */*.bin fs/*.mem
