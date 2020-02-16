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

fun init mp = let
fun insert_map_symbol (x::xs) = (mp := AtomMap.insert (!mp, x, ref AtomSet.empty); insert_map_symbol xs)
|   insert_map_symbol _ = ()
in
insert_map_symbol (AtomSet.listItems (#symbols grammar))
end;
      
init first;
init follow;



fun member_atom_list lst x = List.exists (fn y => (Atom.compare(x,y) = EQUAL)) lst;





(* Whether fixed point is calculated or not*)

val cont = ref false;

(* finding nullable elements*)
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
					else if check_nullable_symbol x then (nullable := x :: !nullable; cont := true)
				 	else ();check_nullable_symbols xs)
|	check_nullable_symbols _ = ();


fun find_nullable () = (cont := false; check_nullable_symbols (AtomSet.listItems (#symbols grammar));
						if (!cont) then find_nullable () else ());



(* finding first sets *)
fun add_first_symbol a b = let val fst_b = if AtomSet.member (#tokens grammar, b) then AtomSet.add (AtomSet.empty, b)
                                           else if Atom.same (b, Atom.atom "EPS") then AtomSet.empty
					   else !(AtomMap.lookup (!first, b))
				val fst_a = !(AtomMap.lookup (!first, a)) 
			in
			   if AtomSet.isSubset (fst_b, fst_a) then () 
			   else (cont := true; AtomMap.lookup (!first, a) := AtomSet.union(fst_a, fst_b))
			end;
fun find_first_prod a (x::xs) = (add_first_symbol a x;if member_atom_list (!nullable) x then find_first_prod a xs else () )
    | find_first_prod _ _ = ();

fun find_first_rule a (x::xs) = (find_first_prod a x;find_first_rule a xs)
    | find_first_rule _ _     = ();
fun find_first_symbol x = let val rl = RHSSet.listItems (AtomMap.lookup(#rules grammar, x) ) in 
						find_first_rule x rl
			  end;

fun find_first_symbols (x::xs) = (find_first_symbol x;find_first_symbols xs)
    |find_first_symbols _	= ();

fun find_first () = (cont:= false; find_first_symbols (AtomSet.listItems (#symbols grammar) );
			if (!cont) then find_first () else () );


(* FINDING FOLLOW SETS *)
fun add_follow_symbol y x (xs::xss) = (let val fst_xs = if AtomSet.member (#tokens grammar, xs) then AtomSet.add (AtomSet.empty, xs)
									   					else !(AtomMap.lookup (!first, xs))
												val foll_x = !(AtomMap.lookup (!follow, x)) in
											if AtomSet.isSubset (fst_xs, foll_x) then ()
											else (cont := true; AtomMap.lookup (!follow, x) := AtomSet.union (fst_xs, foll_x))
										end;
										if member_atom_list (!nullable) xs then add_follow_symbol y x xss else ())
| 	add_follow_symbol y x _ = let val foll_y = !(AtomMap.lookup (!follow, y))
										val foll_x = !(AtomMap.lookup (!follow, x)) in
									if AtomSet.isSubset (foll_y, foll_x) then ()
									else (cont := true; AtomMap.lookup (!follow, x) := AtomSet.union (foll_x, foll_y))
								end;

fun find_follow_prod y (x::xs) = (if AtomSet.member (#symbols grammar, x) then add_follow_symbol y x xs else ();
								  find_follow_prod y xs)
|	find_follow_prod _ _ = ();

fun find_follow_rule x (r::rules) = (find_follow_prod x r; find_follow_rule x rules)
|   find_follow_rule _ _ = ();
fun find_follow_symbol x = let val xrules = RHSSet.listItems (AtomMap.lookup(#rules grammar, x)) in
				find_follow_rule x xrules
			   end;
fun find_follow_symbols (x::xs) = (find_follow_symbol x; find_follow_symbols xs)
|   find_follow_symbols _ 	= ();
fun find_follow () = (cont:= false; find_follow_symbols (AtomSet.listItems (#symbols grammar) ) ;
			if(!cont) then find_follow () else () );

find_nullable ();
find_first ();
find_follow ();

fun print_atom_list (x::xs) = (Atom.toString x)^", " ^ (print_atom_list xs)
    |print_atom_list _ = "\n";

(*printing nullable *)

print("The list of nullable symbols :-\t"^print_atom_list (!nullable));

