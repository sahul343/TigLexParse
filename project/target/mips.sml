signature TEMP = sig
    type temp  (* temporary variables of your program *)
    type label (* temporary labels for your program *)
    val newlabel : unit -> label
    val newtemp  : unit -> temp

end
structure Temp : TEMP = struct
    type temp = int 
    type label = int
    val tempcnt = ref 0
    val labelcnt = ref 0
    fun newlabel () = (labelcnt := !labelcnt + 1; !labelcnt)
    fun newtemp () = (tempcnt := !tempcnt + 1; !tempcnt) 
    

end;
structure MIPS = struct 

 datatype REG = ZERO | V  of int | A of int | T of int | S of int
|K of int | GP | SP | FP | RA 
(*ZERO|V0|V1|A0|A1|A2|A3|T0|T1|T2|T3|T4|T5|T6|T7|T8|T9|S0|S1|S2|S3|S4|S5|S6|S7|K0|K1|GP|SP|FP|RA *)
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

fun pretty (ADD (a, b, c)) = ("add $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (SUB (a, b, c)) = ("sub $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (ADDI (a, b, c)) = ("addi $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (ADDU (a, b, c)) = ("addu $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (SUBU (a, b, c)) = ("subu $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (ADDIU (a, b, c)) = ("addiu $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n")
    |  pretty (MUL (a, b, c)) = ("mul $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (MULT (a, b)) = ("mult $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ "\n")
    |  pretty (DIV (a, b)) = ("div $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ "\n")
    |  pretty (AND (a, b, c)) = ("and $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (OR (a, b, c)) = ("or $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (ANDI (a, b, c)) = ("andi $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (ORI (a, b, c)) = ("ori $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^"\n")
    |  pretty (SLL (a, b, c)) = ("sll $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (SRL (a, b, c)) = ("srl $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (LW (a, b, c)) = ( "lw $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (SW (a, b, c)) = ( "sw $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (LUI (a, b)) = ( "lui $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ "\n")
    |  pretty (LA (a, b)) = ( "la $" ^ Int.toString a ^ ", " ^ Int.toString b ^ "\n")
    |  pretty (LI (a, b)) = ( "li $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ "\n")
    |  pretty (MFHI a) = ( "mhfi $" ^ Int.toString a ^ "\n")
    |  pretty (MFLO a) = ( "mflo $" ^ Int.toString a ^ "\n")
    |  pretty (BEQ (a, b, c)) = ("beq $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n")
    |  pretty (BNE (a, b, c)) = ("bnq $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n")
    |  pretty (BGT (a, b, c)) = ("bgt $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n")
    |  pretty (BGE (a, b, c)) = ("bge $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n")
    |  pretty (BLT (a, b, c)) = ("blt $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n")
    |  pretty (BLE (a, b, c)) = ("ble $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n")
    |  pretty (SLT (a, b, c)) = ("slt $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", $" ^ Int.toString c ^ "\n")
    |  pretty (SLTI (a, b, c)) = ("slti $" ^ Int.toString a ^ ", $" ^ Int.toString b ^ ", " ^ Int.toString c ^ "\n")
    |  pretty (J a) = ("j " ^ Int.toString a ^ "\n")
    |  pretty (JR a) = ("jr $" ^ Int.toString a ^ "\n")
    |  pretty (JAL a) = ("jal " ^ Int.toString a ^ "\n")
    |  pretty _ = ( "syscall\n")
end;
val r1 = Temp.newtemp()
val r2 = Temp.newtemp()
val r3 = Temp.newtemp()
val test = MIPS.ADD(r1, r2, r3)
val res = MIPS.pretty(test)
