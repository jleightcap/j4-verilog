`include "common.h"

module top_tb;

reg  clk;
reg  [15:0] instr;
wire [12:0] pc;
wire io_we;
wire io_re;
wire [`WIDTH-1:0] io_ptr;
reg  [`WIDTH-1:0] io_out;
wire [`WIDTH-1:0] io_in;

// FIXME(jl): i thought there were only 15 bit pointers?
reg [`WIDTH-1:0] ram [0:(1<<`WIDTH)-1];

always @(posedge clk) begin
    if (io_re)
        io_out <= ram[io_ptr];
    if (io_we)
        ram[io_ptr] <= io_in;
    instr <= ram[{3'b0, pc}];
end

j1 cpu( .pc(pc)
      , .clk(clk)
      , .instr(instr)
      , .io_we(io_we)
      , .io_re(io_re)
      , .io_ptr(io_ptr)
      , .io_in(io_out)
      , .io_out(io_in)
      );

// verilator lint_off STMTDLY
initial begin
    #0 clk=1; { instr , io_out } = 0;
    // verilator lint_off INFINITELOOP
    forever #1 clk=!clk;
    // verilator lint_on INFINITELOOP
end

`ifndef FILE
    `define FILE "bin/test.mem"
`endif

initial begin
    #0 $readmemb(`FILE, ram);
    #0 $monitor("instr=%2h@%2h o?=%1b i?=%1b io_ptr=%2h io_in=%2h", instr, pc, io_we, io_re, io_ptr, io_in);
    #1000 $finish;
end

endmodule
