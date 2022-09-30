`include "common.h"

module j1( input  wire clk
         , output reg  [12:0] pc
         , input  wire [15:0] instr
         , output wire io_we                    // I/O write enable
         , output wire io_re                    // I/O read enable
         , output wire [`WIDTH-1:0] io_ptr
         , input  wire [`WIDTH-1:0] io_in
         , output wire [`WIDTH-1:0] io_out
`ifdef TESTBENCH
         , output wire [12:0] pc_dut
         , output wire [`DEPTH-1:0] dsp_dut
         , output wire [`WIDTH-1:0] dst0_dut
         , output wire [`WIDTH-1:0] dst1_dut
`endif
    );

//
// DATA STACK
//
reg  ds_we;                         // data stack write enable
reg  [`WIDTH-1:0] dst0, _dst0;      // top of data stack, synchronous and asynchronous
wire [`WIDTH-1:0] dst1;              // data stack read value
reg  [`DEPTH-1:0] dsp, _dsp;        // data stack pointer, synchronous and asynchronous

initial begin
    { ds_we , dsp , _dsp , dst0 , _dst0 } = 0;
end

reg  [`WIDTH-1:0] ds_data [(2**`DEPTH)-1:0];
always @(posedge clk) begin
    if (ds_we) begin
        ds_data[_dsp] <= dst0;
    end
end
assign dst1 = ds_data[dsp];

// instruction effect on data stack
always @(*) begin
    casez ({instr[15:13]})
        3'b1??: /* literal */
            {ds_we, _dsp} = {1'b1, dsp + 4'b0001};    // increment data stack
        3'b000: /* jump */
            {ds_we, _dsp} = {1'b0, dsp + 4'b0000};    // no effect
        3'b001: /* conditional jump */
            {ds_we, _dsp} = {1'b0, dsp + 4'b0000};    // no effect
        3'b010: /* call */
            {ds_we, _dsp} = {1'b0, dsp + 4'b0000};    // no effect
        3'b011: /* alu */
            // given a 4-bit 'signed' dsp, the four possible data stack changes encoded in instr[1:0]:
            //  _dsp + 00_00 -- unchanged
            //  _dsp + 00_01 -- increment by 1
            //  _dsp + 11_10 -- decrement by 2
            //  _dsp + 11_11 -- decrement by 1
            {ds_we, _dsp} = {instr[7], dsp + {instr[1], instr[1], instr[1:0]}};
    endcase
end

//
// RETURN STACK
//
reg  rs_we;                         // return stack write enable
wire [`WIDTH-1:0] rst0;             // top of return stack, synchronous
reg  [`WIDTH-1:0] _rst0;            // top of return stack, asynchronous
reg  [`DEPTH-1:0] rsp, _rsp;        // return stack pointer, synchronous and asynchronous

initial begin
    // FIXME(jl): _rst0 and subroutines.
    { rsp , _rsp , rs_we , _rst0 } = 0;
end

reg  [`WIDTH-1:0] rs_data [(2**`DEPTH)-1:0];
always @(posedge clk) begin
    if (rs_we) begin
        rs_data[_rsp] <= _rst0;
    end
end
assign rst0 = rs_data[rsp];

// instruction effect on return stack
always @(*) begin
    casez ({instr[15:13]})
        3'b1??: /* literal */
            {rs_we, _rsp} = {1'b0, rsp + 4'b0000};    // no effect
        3'b000: /* jump */
            {rs_we, _rsp} = {1'b0, rsp + 4'b0000};    // no effect
        3'b001: /* conditional jump */
            {rs_we, _rsp} = {1'b0, rsp + 4'b0000};    // no effect
        3'b010: /* call */
            {rs_we, _rsp} = {1'b1, rsp + 4'b0001};    // increment return stack
        3'b011: /* alu */
            // given a 4-bit 'signed' rsp, the four possible data stack changes encoded in instr[3:2]:
            //  _rsp + 00_00 -- unchanged
            //  _rsp + 00_01 -- increment by 1
            //  _rsp + 11_10 -- decrement by 2
            //  _rsp + 11_11 -- decrement by 1
            {rs_we, _rsp} = {instr[6], rsp + {instr[3], instr[3], instr[3:2]}};
    endcase
end

//
// ALU
//

always @* begin
    casez ({instr[15:8]})
        8'b1??_?_????: /* zero-padded instruction-encoded literal */
            _dst0 = {{(`WIDTH - 15){1'b0}}, instr[14:0]};
        8'b000_?_????, /* jump */
        8'b001_?_????, /* conditional jump */
        8'b010_?_????: /* call */
            _dst0 = dst0;
        8'b011_?_0000: /* alu: T */
            _dst0 = dst0;
        8'b011_?_0001: /* alu: N */
            _dst0 = dst1;
        8'b011_?_0010: /* alu: T + N */
            _dst0 = dst0 + dst1 ;
        8'b011_?_0011: /* alu: T & N */
            _dst0 = dst0 & dst1;
        8'b011_?_0100: /* alu: T | N */
            _dst0 = dst0 | dst1;
        8'b011_?_0101: /* alu: T ^ N */
            _dst0 = dst0 ^ dst1;
        8'b011_?_0110: /* alu: not T */
            _dst0 = ~dst0;
        8'b011_?_0111: /* alu: N == T */
            _dst0 = {16{(dst0 == dst1)}};
        8'b011_?_1000: /* alu: N < T */
            _dst0 = {16{($signed(dst1) < $signed(dst0))}};
        8'b011_?_1001: /* alu: N >> 1 */
            _dst0 = dst0 >> 1;
        8'b011_?_1010: /* alu: T - 1 */
            _dst0 = dst0 - 1;
        8'b011_?_1011: /* alu: R */
            _dst0 = rst0;
        8'b011_?_1100: /* alu: [T] */
            _dst0 = io_in;
        8'b011_?_1101: /* alu: N << 1 */
            _dst0 = dst0 << 1;
        8'b011_?_1110: /* alu: depth */
            _dst0 = {{(`WIDTH - 8){1'b0}}, rsp, dsp};
        8'b011_?_1111: /* alu: N u< T */
            _dst0 = {16{(dst1 < dst0)}};
    endcase
end

//
// PROCESSOR STATE
//
reg  [12:0] _pc;        // program counter, asynchronous
wire [12:0] pc_inc;

assign pc_inc = pc + 1;

// instruction effect on program counter
always @(*) begin
    casez ({instr[15:13]})
        3'b1??: /* literal */
            _pc = pc_inc;
        3'b000: /* jump */
            _pc = instr[12:0];
        3'b001: /* conditional jump */
            _pc = (|dst0 == 0) ? instr[12:0] : pc_inc;
        3'b010: /* call */
            _pc = instr[12:0];
        3'b011: /* alu */
            _pc = pc_inc;
    endcase
end

assign io_we = (instr[15:13] == 3'b011) & instr[5]; // ALU N -> [T]
assign io_re = (instr[15:13] == 3'b011) & (instr[11:8] == 4'hc) & (|dst0[15:14]);
assign io_ptr = dst0;
assign io_out = dst1;

// synchronous-asynchronous register pairs update
always @(posedge clk) begin
    { pc , dst0 , dsp , rsp } <= { _pc , _dst0 , _dsp , _rsp };
end

`ifdef TESTBENCH
    assign { dsp_dut , dst0_dut , dst1_dut , pc_dut } = { dsp , dst0 , dst1 , pc };
`endif

endmodule
