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
