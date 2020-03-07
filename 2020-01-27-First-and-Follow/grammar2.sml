(* symbols in the grammar *)
val sym = ref AtomSet.empty;
sym := AtomSet.addList(!sym,[Atom.atom "S",Atom.atom "E"] );


(* Tokens in the grammar *)
val tok = ref AtomSet.empty;
tok := AtomSet.addList(!tok,[Atom.atom "$", Atom.atom "(", Atom.atom ")"] );


(*Adding productions for S *)
val S_ = ref RHSSet.empty;
S_:= RHSSet.addList(!S_,[ [Atom.atom "E", Atom.atom "$"] ]);

(* Adding productions for E *)
val E_ = ref RHSSet.empty;
E_:= RHSSet.addList(!E_,[ [Atom.atom "(", Atom.atom "E",Atom.atom ")"], [Atom.atom "EPS"] ]);



val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
rule := AtomMap.insert (!rule, Atom.atom "E", !E_);
val grammar : Grammar = {symbols = !sym, tokens = !tok, rules = !rule };

