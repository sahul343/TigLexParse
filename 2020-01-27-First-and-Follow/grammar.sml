use "type.sml";
(* symbols in the grammar *)
val sym = ref AtomSet.empty;
sym := AtomSet.addList(!sym,[Atom.atom "S",Atom.atom "E", Atom.atom "T", Atom.atom "F"] );


(* Tokens in the grammar *)
val tok = ref AtomSet.empty;
tok := AtomSet.addList(!tok,[Atom.atom "$", Atom.atom "*", Atom.atom "+", Atom.atom "a"] );


(*Adding productions for S *)
val S_ = ref RHSSet.empty;
S_:= RHSSet.addList(!S_,[ [Atom.atom "E", Atom.atom "$"] ]);

(* Adding productions for E *)
val E_ = ref RHSSet.empty;
E_:= RHSSet.addList(!E_,[ [Atom.atom "E", Atom.atom "+",Atom.atom "T"], [Atom.atom "T"] ]);


(* Adding productions for T *)
val T_ = ref RHSSet.empty;
T_:= RHSSet.addList(!T_,[ [Atom.atom "T", Atom.atom "*",Atom.atom "F"], [Atom.atom "F"] ]);

(* Adding productions for F *)
val F_ = ref RHSSet.empty;
F_:= RHSSet.addList(!F_,[ [Atom.atom "a"] ]);



val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
rule := AtomMap.insert (!rule, Atom.atom "F", !F_);
rule := AtomMap.insert (!rule, Atom.atom "E", !E_);
rule := AtomMap.insert (!rule, Atom.atom "T", !T_);
val grammar : Grammar = {symbols = !sym, tokens = !tok, rules = !rule };
