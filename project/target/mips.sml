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
                | SUB of 't * 't * 't
                | ADDI of 't * 't * int
                | ADDU of 't * 't * 't
                | SUBU of 't * 't * 't
                | ADDIU of 't * 't * int
                | MUL of 't * 't * 't
                | MULT of 't * 't
                | DIV of 't * 't
                | AND of 't * 't * 't
                | OR of 't * 't * 't
                | ANDI of 't * 't * int
                | ORI of 't * 't * int
                | SLL of 't * 't * int
                | SRL of 't * 't * int
                | LW of 't * int * 't
                | SW of 't * int * 't
                | LUI of 't * int
                | LA of 't * 'l
                | LI of 't * int
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

fun pretty (ADD (a, b, c)) = "add $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (SUB (a, b, c)) = "sub $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (ADDI (a, b, c)) = "addi $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (ADDU (a, b, c)) = "addu $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (SUBU (a, b, c)) = "subu $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (ADDIU (a, b, c)) = "addiu $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n"
    |  pretty (MUL (a, b, c)) = "mul $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (MULT (a, b)) = "mult $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ "\n"
    |  pretty (DIV (a, b)) = "div $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ "\n"
    |  pretty (AND (a, b, c)) = "and $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (OR (a, b, c)) = "or $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (ANDI (a, b, c)) = "andi $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (ORI (a, b, c)) = "ori $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^"\n"
    |  pretty (SLL (a, b, c)) = "sll $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (SRL (a, b, c)) = "srl $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (LW (a, b, c)) =  "lw $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (SW (a, b, c)) =  "sw $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (LUI (a, b)) =  "lui $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ "\n"
    |  pretty (LA (a, b)) =  "la $" ^ Int.toString a ^ ", " ^ Int.toString b ^ "\n"
    |  pretty (LI (a, b)) =  "li $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ "\n"
    |  pretty (MFHI a) =  "mhfi $" ^ Int.toString a ^ "\n"
    |  pretty (MFLO a) =  "mflo $" ^ Int.toString a ^ "\n"
    |  pretty (BEQ (a, b, c)) = "beq $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n"
    |  pretty (BNE (a, b, c)) = "bnq $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n"
    |  pretty (BGT (a, b, c)) = "bgt $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n"
    |  pretty (BGE (a, b, c)) = "bge $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n"
    |  pretty (BLT (a, b, c)) = "blt $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n"
    |  pretty (BLE (a, b, c)) = "ble $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n"
    |  pretty (SLT (a, b, c)) = "slt $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n"
    |  pretty (SLTI (a, b, c)) = "slti $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n"
    |  pretty (J a) = "j " ^ Int.toString a ^ "\n"
    |  pretty (JR a) = "jr $" ^ Int.toString a ^ "\n"
    |  pretty (JAL a) = "jal " ^ Int.toString a ^ "\n"
    |  pretty _ =  "syscall\n"
end
