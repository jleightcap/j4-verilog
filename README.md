# j4 cpu

implementation of a stack-backed, forth-oriented cpu à la bowman, koopman.

## memory map

16-bit word size

```
                    v          v
0xffff ++=====++ ---+----------+-
       ||     ||    |          |
       ||     ||  v | readable |
0x1fff +-------+ -+-+-         |
       ||     ||  | ^          |
       ||     ||  |            |
       ||     ||  |            |
       ||     ||  |            | writable
       ||     ||  |            |
       ||     ||  | program    |
       ||     ||  |            |
       ||     ||  |            |
       ||     ||  |            |
       ||     ||  |            |
       ||     ||  |            |
0x0000 ++=====++ -+------------+-
                  ^            ^
```

## instruction encoding

| encoding | meaning |
| -------- | ------- |
| `1nnn_nnnn_nnnn_nnnn` | 15-bit literal `n` |
| `000p_pppp_pppp_pppp` | unconditoinal jump to 13-bit pointer `p` |
| `001p_pppp_pppp_pppp` | conditional jump to 13-bit pointer `p` |
| `010p_pppp_pppp_pppp` | call 13-bit pointer `p` |
| `011x_xxxx_xxxx_xxxx` | [alu encoding](#alu-instruction-encoding) |

## registers
| user register | use |
| ------------- | --- |
| T | top element of data stack |
| N | second element of data stack |
| R | top element of return stack |

| system register | use |
| --------------- | --- |
| dsp | pointer to top of data stack |
| rsp | pointer to top of return stack |
| pc  | program counter |

## ALU instruction encoding

ALU instructions are composed of 7 fields

| bit(s) | field | meaning |
| ------ | ----- | ------- |
| 15-13  | constant `011` | prefix; see [instruction encoding](#instruction-encoding) |
| 12     | R→PC | copy R to PC (i.e., return from subroutine) |
| 11-8   | T' | alu operation; see below |
| 7      | N→T | copy N to T |
| 6      | T→R | copy T to R |
| 5      | N→[T] | copy N to \*T |
| 4      | _unused_ | |
| 3-2    | rstack | stack delta¹ |
| 1-0    | dstack | stack delta¹ |

| alu operation² | meaning |
| -------------- | ------- |
| 0000 | NOP
| 0001 | N → T
| 0010 | T + N → T
| 0011 | T & N → T
| 0100 | T | N → T
| 0101 | T ^ N → T
| 0110 | ~T → T
| 0111 | T == N → T
| 1000 | N s< T → T (signed compare)
| 1001 | T >> 1 → T
| 1010 | T - 1 → T
| 1011 | R → T
| 1100 | \*T → T
| 1101 | T << 1 → T
| 1110 | {rsp, dsp} → T (e.g. stack depths)
| 1111 | N u< R → T (unsigned compare)

> ¹ the stack delta is a 2-bit number treated as a 4-bit 'signed' number; `00` +0, `01` +1, `10` -2, `11` -1.

> ² all arithmetic operations are bit-wise.

# references

 1. [Bowman: _J1: a small Forth CPU Core for FPGAs_](https://www.complang.tuwien.ac.at/anton/euroforth/ef10/papers/bowman.pdf)
 2. [Koopman: _Stack Computers: the new wave_](https://users.ece.cmu.edu/~koopman/stack_computers/index.html)
