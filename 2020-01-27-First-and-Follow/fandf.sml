(* to find first and follow of the grammer first we have to find nullable sysbols *)

(* we will use grammer *)

use "grammer.sml";

(*  we will use set of atoms to store nullable symbols *)

val nullable : Atom.atom list ref = ref nil;

(* we will use Map function of sets of Atom to store first and follow *)

val first : (AtomSet.set ref) AtomMap.map ref  = ref AtomMap.empty;
val follow : (AtomSet.set ref) AtomMap.map ref = ref AtomMap.empty;



