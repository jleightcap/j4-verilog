# j4 cpu

implementation of a stack-backed, forth-oriented CPU cpu à la bowman, koopman.

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
| `011x_xxxx_xxxx_xxxx` | ALU (see encoding below) |

## ALU instruction encoding

ALU instructions are composed of 7 fields

| bit(s) | field | meaning |
| ------ | ----- | ------- |
| 15-13  | constant `011` | prefix; see [instruction encoding](#instruction-encoding) |
| 12     | R→PC | |
| 11-8   | T' | |
| 7      | N→T | |
| 6      | T→R | | 
| 5      | N→[T] | |
| 4      | _unused_ | |
| 3-2    | rstack | |
| 1-0    | dstack | |
 
# references

 - [J1: a small Forth CPU Core for FPGAs](https://www.complang.tuwien.ac.at/anton/euroforth/ef10/papers/bowman.pdf)
