`include "common.h"

module j4_tb;

reg  clk;
wire  [15:0] instr;
wire [12:0] pc;
wire io_we;
wire io_re;
wire [`WIDTH-1:0] io_ptr;
reg  [`WIDTH-1:0] io_out;
wire [`WIDTH-1:0] io_in;

wire [12:0] pc_dut;
wire [`DEPTH-1:0] dsp_dut;
wire [`WIDTH-1:0] dst0_dut;
wire [`WIDTH-1:0] dst1_dut;
wire ds_we_dut;
wire [`DEPTH-1:0] rsp_dut;
wire [`WIDTH-1:0] rst0_dut;

reg [`WIDTH-1:0] ram [0:(1<<`WIDTH)-1];

assign instr = ram[{3'b0, pc}];

always @(posedge clk) begin
    if (io_re)
        io_out <= ram[io_ptr];
    if (io_we)
        ram[io_ptr] <= io_in;
end

j4 cpu( .pc(pc)
      , .clk(clk)
      , .instr(instr)
      , .io_we(io_we)
      , .io_re(io_re)
      , .io_ptr(io_ptr)
      , .io_in(io_out)
      , .io_out(io_in)
      /* testbench wiring */
      , .pc_dut(pc_dut)
      , .dsp_dut(dsp_dut)
      , .dst0_dut(dst0_dut)
      , .dst1_dut(dst1_dut)
      , .ds_we_dut(ds_we_dut)
      , .rsp_dut(rsp_dut)
      , .rst0_dut(rst0_dut)
      );

// verilator lint_off STMTDLY
initial begin
    #0 clk=0; { io_out } = 0;
    // verilator lint_off INFINITELOOP
    forever #1 clk=!clk;
    // verilator lint_on INFINITELOOP
end

always @(posedge clk) begin
    if (io_we)
        $display("IO_OUT ram[%2h] <- %2h", io_ptr, io_in);
    if (io_re)
        $display("IO_IN ram[%2h] -> %2h", io_ptr, io_out);
end

`ifndef FILE
    `define FILE "bin/test.mem"
`endif

initial begin
    #0 $readmemb(`FILE, ram);
    #0 $monitor(
        "instr=%2h@%2h io_{w,r}={%1b, %1b}\n", instr, pc_dut, io_we, io_re,
        "\tdsp=%02h ds_we=%1b T=%2h N=%2h\n", dsp_dut, ds_we_dut, dst0_dut, dst1_dut,
        "\trsp=%02h R=%2h", rsp_dut, rst0_dut
    );
    #1000 $finish;
end

endmodule
