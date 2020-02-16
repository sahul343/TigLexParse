type Symbol = Atom.atom
type Token  = Atom.atom

type Symbols = AtomSet.set   (* set of symbols *)
type Tokens  = AtomSet.set   (* set of tokens  *)

type RHS    = Atom.atom list  (* The RHS γ of a rule A -> γ *)

(*

We have the structures AtomSet and AtomMap to represent sets and maps
of Atoms. For any type t if we want sets and maps (dictionaries) we
need an ordering structure on the elements.  We would like to create
the set structure on RHS's. For this you first need to define a
structure of signature ORD_KEY for RHS.

*)

structure RHS_KEY : ORD_KEY = struct
    (* complete this *)
type ord_key = RHS
fun compare(a,b) = List.collate Atom.compare (a, b)
end


(*

Use the above structure to create a set of rhs's

*)

structure RHSSet = RedBlackSetFn (RHS_KEY)


structure First_set : ORD_KEY = struct
type ord_key = AtomSet.set
fun compare = RHSSet.compare
end


structure FirstMap = RedBlackMapFn(First_set)



type Productions = RHSSet.set

(* The rules of the grammar are a dictionary whose keys are the symbol
   and the values are the Productions associated with the grammar.
*)

type Rules = Productions AtomMap.map

(* find next approximate first set and return *)



(*fun ApproxFirst g curr = 


fun first_helper g  currfirst= let 
			nextfirst = ApproxFirst g currfirst
	      	in 
			if currfirst = nextfirst then currfirst else (first_helper g nextfirst)	
	     	end


fun first g = let 
		val intialise = FirstMap.empty()
		val insert =map  (fn x => FirstMap.insert(x,))   (#first g)
	(* intialise termianl first set as itself and 
				non-terminals as empty set *)
	      in 
		 	first_helper g intialise
	      end
*)


type Grammar    = { symbols : Symbols, tokens : Tokens, rules : Rules }

(*  update Grammar* (AtomSet.set AtomMap.map) ->( AtomSet.set AtomMap.map) *)
