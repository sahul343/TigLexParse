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

fun init mp = let fun insert_map_symbol (x::xs) = (mp := AtomMap.insert (!mp, x, ref AtomSet.empty); insert_map_symbol xs)
                            |   insert_map_symbol _ = () in
                        insert_map_symbol (AtomSet.listItems (#symbols grammar))
                    end;
init first;
init follow;



fun member_atom_list lst x = List.exists (fn y => (Atom.compare(x,y) = EQUAL)) lst;





(* whether NULLABLE has reached it's fixed point*)

val cont = ref false;
fun check_nullable_single x = if Atom.compare (x, Atom.atom "EPS") = EQUAL then true
							  else if AtomSet.member (#tokens grammar, x) then false
							       else member_atom_list (!nullable) x;

fun check_nullable_prod (x::xs) = if not (check_nullable_single x) then false else check_nullable_prod xs
|	check_nullable_prod _ = true;

fun check_nullable_rule (x::xs) = if check_nullable_prod x then true else check_nullable_rule xs
| 	check_nullable_rule _ = false;

fun check_nullable_symbol x = let val rl = RHSSet.listItems (AtomMap.lookup (#rules grammar, x)) in
									check_nullable_rule rl
								end;

fun check_nullable_symbols (x::xs) = (if member_atom_list (!nullable) x then () 
                                      else if check_nullable_symbol x then (nullable := x :: !nullable;
																            cont := true)
									       else (); 
                                      check_nullable_symbols xs)
|	check_nullable_symbols _ = ();

fun find_nullable () = (cont := false; check_nullable_symbols (AtomSet.listItems (#symbols grammar));
						if (!cont) then find_nullable () else ());


