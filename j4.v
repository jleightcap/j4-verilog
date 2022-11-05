`include "common.h"

module j4( input  wire clk
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
         , output wire ds_we_dut
         , output wire [`DEPTH-1:0] rsp_dut
         , output wire [`WIDTH-1:0] rst0_dut
`endif
    );

//
// DATA STACK
//
reg  ds_we;                         // data stack write enable
reg  [`WIDTH-1:0] dst0, _dst0;      // top of data stack, synchronous and asynchronous
wire [`WIDTH-1:0] dst1;             // data stack read value
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
        8'b1??_?_????:
            {_dst0, _rst0} =
                { {{(`WIDTH - 15){1'b0}}, instr[14:0]},   /* T zero-padded instruction-encoded literal */
                  rst0                                    /* R unchanged */
                };
        8'b000_?_????, /* jump */
        8'b001_?_????: /* conditional jump */
            {_dst0, _rst0} = {dst0, rst0};                /* unchanged */
        8'b010_?_????: /* call */
            {_dst0, _rst0} =
                { dst0,                                   /* T unchanged */
                  {{(`WIDTH - 13){1'b0}}, pc_inc}         /* R <- PC + 1 (return address) */
                };
        8'b011_?_0000:
            {_dst0, _rst0} = {dst0, rst0};                /* T unchanged, R unchanged (NOP) */
        8'b011_?_0001:
            {_dst0, _rst0} = {dst1, rst0};                /* T <- N, R unchanged */
        8'b011_?_0010:
            {_dst0, _rst0} = {dst0 + dst1, rst0};         /* T <- T + N, R unchanged */
        8'b011_?_0011:
            {_dst0, _rst0} = {dst0 & dst1, rst0};         /* T <- T & N, R unchanged */
        8'b011_?_0100:
            {_dst0, _rst0} = {dst0 | dst1, rst0};         /* T <- T | N, R unchanged */
        8'b011_?_0101:
            {_dst0, _rst0} = {dst0 ^ dst1, rst0};         /* T <- T ^ N, R unchanged */
        8'b011_?_0110:
            {_dst0, _rst0} = {~dst0, rst0};               /* T <- ~T, R unchanged */
        8'b011_?_0111:
            {_dst0, _rst0} =
                { {16{(dst0 == dst1)}},                   /* T <- T == N */
                  rst0                                    /* R unchanged */
                };
        8'b011_?_1000:
            {_dst0, _rst0} =
                { {16{($signed(dst1) < $signed(dst0))}},  /* T <- N s< T */
                  rst0                                    /* R unchanged */
                };
        8'b011_?_1001:
            {_dst0, _rst0} = { dst0 >> 1, rst0};          /* T <- T >> 1, R unchanged */
        8'b011_?_1010:
            {_dst0, _rst0} = {dst0 - `WIDTH'h1, rst0};    /* T <- T - 1, R unchanged */
        8'b011_?_1011:
            {_dst0, _rst0} = {rst0, rst0};                /* T <- R, R unchanged */
        8'b011_?_1100:
            {_dst0, _rst0} = {io_in, rst0};               /* T <- [T], R unchanged */
        8'b011_?_1101:
            {_dst0, _rst0} = {dst0 << 1, rst0};           /* T <- T << 1, R unchanged */
        8'b011_?_1110:
            {_dst0, _rst0} =
                { {{(`WIDTH - 8){1'b0}}, rsp, dsp},       /* T <- {rsp, dsp} */
                  rst0                                    /* R unchanged */
                };
        8'b011_?_1111:
            {_dst0, _rst0} =
                { {16{(dst1 < dst0)}},                    /* T <- N u< R */
                  rst0                                    /* R unchanged */
                };
    endcase
end

//
// PROCESSOR STATE
//
reg  [12:0] _pc;        // program counter, asynchronous
wire [12:0] pc_inc;

assign pc_inc = pc + 1;

initial begin
    { _pc } = 0;
end


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
            _pc =  instr[12] ? rst0[12:0] : pc_inc;
    endcase
end

 // ALU & N -> [T]
assign io_we = (instr[15:13] == 3'b011) & instr[5];
// ALU & T' = [T] & ptr & pointer into RAM 'above' PC
assign io_re = (instr[15:13] == 3'b011) & (instr[11:8] == 4'hc) & (|dst0[15:14]);
assign io_ptr = dst0;
assign io_out = dst1;

// synchronous-asynchronous register pairs update
always @(posedge clk) begin
    { pc , dst0 , dsp , rsp } <= { _pc , _dst0 , _dsp , _rsp };
end

`ifdef TESTBENCH
    assign { dsp_dut , dst0_dut , dst1_dut , ds_we_dut , rsp_dut , rst0_dut, pc_dut } =
           { dsp     , dst0     , dst1     , ds_we     , rsp     , rst0    , pc };
`endif

endmodule
