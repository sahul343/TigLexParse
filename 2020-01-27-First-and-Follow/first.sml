(* we need grammar type and an example grammar *)
use "type.sml";
use "grammar.sml";

(* The flow is like we find first all nullable symbols then find first sets and follow sets of symbols *)

(* set of atoms to store all nullable items *)

val nullable : Atom.atom list ref = ref nil;


(* Map for storing first and follow of symbols of grammar*)
val first : (AtomSet.set ref) AtomMap.map ref = ref AtomMap.empty;
val follow : (AtomSet.set ref) AtomMap.map ref = ref AtomMap.empty;


(* we have to intialise first and follow sets to be empty for each symbol of the grammar *)

fun initalise mp = let  
			fun insert_sym (x::xs) = (mp:= AtomMap.insert(!mp,x,AtomSet.empty); insert_sym xs  )
			    |insert_sym  _	   = () 
		   in 
			insert_sym (AtomSet.listItems (#symbols grammar) )
		   end;

initalise first;
intialise follow;


fun member_atom_list lst x = let 
					fun cmp y = Atom.compare (x, y) = EQUAL 
				in
                            		List.exists cmp lst
                        	end







