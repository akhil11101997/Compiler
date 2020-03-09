(* here first we have to represent the symbols in grammer by using set of atoms *)
use "overview_of_grammer.sml";
val symb  = ref AtomSet.empty;
symb := AtomSet.addList(!symb, [Atom.atom "S", Atom.atom "E", Atom.atom "F", Atom.atom "X"]);


(* here we have to represent the tokens by using set of atoms *)

val toke = ref AtomSet.empty;
toke := AtomSet.addList(!toke, [Atom.atom "a", Atom.atom "b", Atom.atom "(", Atom.atom ")", Atom.atom "$"]);


(* here we will add productions corresponding to each symbol in the grammer     *)

val S_productions = ref RHSSet.empty;
S_productions := RHSSet.add (!S_productions, [Atom.atom "E", Atom.atom "$"]);

val E_productions = ref RHSSet.empty;
E_productions := RHSSet.add (!E_productions, [Atom.atom "X", Atom.atom "F"]);

val F_productions = ref RHSSet.empty;
F_productions := RHSSet.add (!F_productions, [Atom.atom "EPS"]);
F_productions := RHSSet.add (!F_productions, [Atom.atom "a", Atom.atom "E"]);

val X_productions = ref RHSSet.empty;
X_productions := RHSSet.add (!X_productions, [Atom.atom "(", Atom.atom "E", Atom.atom ")"]);
X_productions := RHSSet.add (!X_productions, [Atom.atom "b"]);



(* here we will add all this productions into rules *)

val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "S", !S_productions);
rule := AtomMap.insert (!rule, Atom.atom "E", !E_productions);
rule := AtomMap.insert (!rule, Atom.atom "F", !F_productions);
rule := AtomMap.insert (!rule, Atom.atom "X", !X_productions);


(* now we will join all components of grammer tokens,symbols,rules *)

val grammer :  Grammar = { symbols = !symb, tokens = !toke, rules = !rule };



