`include "common.h"

module j4_tb;

reg clk;
reg [15:0] instr;
wire io_we;
wire io_re;
wire [`WIDTH-1:0] io_ptr;
reg  [`WIDTH-1:0] io_out;
wire [`WIDTH-1:0] io_in;

wire [12:0] pc_dut;
wire [`DEPTH-1:0] dsp_dut;
wire [`WIDTH-1:0] dst0_dut;
wire [`WIDTH-1:0] dst1_dut;

j4 j4_dut( .clk(clk)
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
);

// verilator lint_off STMTDLY
initial begin
    #0 clk=1; { instr , io_out }=0;
    // verilator lint_off INFINITELOOP
    forever #1 clk=!clk;
    // verilator lint_on INFINITELOOP
end

initial begin
    #2 instr={1'b1, 15'b111_0000_0000_1111};    // literal 0x700f -- dsp = 1 ; dst0 = 0x700f ; dst1 = 0xXXXX
    #0 $display("DATA STACK TEST");
    #0 $monitor("%4h dsp=%1h T=%4h N=%4h", instr, dsp_dut, dst0_dut, dst1_dut);
    #2 instr={3'b011, 13'b0_0000_00000000};     // alu nop
    #2 instr={1'b1, 15'b000_1111_1111_0000};    // literal 0x0ff0 -- dsp = 2 ; dst0 = 0x0ff0 ; dst1 = 0x700f

    #2 instr={3'b011, 13'b0_0001_00000000};     // N
    #0 $display("ALU TEST");
    #0 $monitor("%4h dsp=%1h T=%4h N=%4h", instr, dsp_dut, dst0_dut, dst1_dut);
    #2 instr={3'b011, 13'b0_0110_00000000};     // not T
    #2 instr={3'b011, 13'b0_0011_00000000};     // T & N

    #2 instr={3'b000, 13'b1111111111111};       // jmp 0x1fff   -- pc = 0x1fff, jump executed
    #0 $display("PC TEST");
    #0 $monitor("%4h pc=%4h T=%4h", instr, pc_dut, dst0_dut);
    #2 instr={3'b011, 13'b0_0000_00000000};     // alu nop      -- pc = 0x0000
    #2 instr={3'b011, 13'b0_0000_00000000};     // alu nop      -- pc = 0x0001
    #2 instr={3'b001, 13'b1111111111111};       // cjump 0x1fff -- pc = 0x1fff, jump executed
    #2 instr={3'b011, 13'b0_1110_00000000};     // depth        -- pc = 0x0000
    #2 instr={3'b001, 13'b1111111111111};       // cjump 0x1fff -- pc = 0x0001, jump not executed

    #2 $finish;
end
// verilator lint_on STMTDLY

endmodule
