signature TEMP = sig
    type temp  (* temporary variables of your program *)
    type label (* temporary labels for your program *)
    val newlabel : unit -> label
    val newtemp  : unit -> temp

end

structure MIPS = struct 

datatype reg = zero | v of int | a of int | t of int | s of int
|k of int | gp | sp | fp | ra
(*zero|v0|v1|a0|a1|a2|a3|t0|t1|t2|t3|t4|t5|t6|t7|t8|t9|s0|s1|s2|s3|s4|s5|s6|s7|k0|k1|gp|sp|fp|ra *)
datatype ('t, 'l) inst = ADD of 't * 't * 't
                | ADDI of 't * 't * int
                | ADDU of 't * 't * 't
                | ADDIU of 't * 't * int
                | SUB of 't * 't * 't
                | SUBU of 't * 't * 't
                | MUL of 't * 't * 't
                | MULT of 't * 't
                | DIV of 't * 't
                | AND of 't * 't * 't
                | ANDI of 't * 't * int
                | OR of 't * 't * 't
                | ORI of 't * 't * int
                | SLL of 't * 't * int
                | SRL of 't * 't * int
                | LW of 't * int * 't
                | LI of 't * int
                | LA of 't * 'l
                | LUI of 't * int
                | SW of 't * int * 't
                | MFHI of 't
                | MFLO of 't
                | MOVE of 't * 't
                | BEQ of 't * 't * 'l
                | BNE of 't * 't * 'l
                | BGT of 't * 't * 'l
                | BGE of 't * 't * 'l
                | BLT of 't * 't * 'l
                | BLE of 't * 't * 'l
                | SLT of 't * 't * 't
                | SLTI of 't * 't * int
                | J of 'l
                | JR of 't
                | JAL of 'l
                | SYSCALL

                        

end
