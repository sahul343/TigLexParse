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
datatype binop = plus | minus | mul 
datatype binop 
datatype ('l,'t) inst = alu of binop*t*t* | aluimm of binop*t*t*int

end
