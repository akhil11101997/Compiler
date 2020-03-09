type Symbol = Atom.atom;
type Token  = Atom.atom;

type Symbols = AtomSet.set;   (* set of symbols *)
type Tokens  = AtomSet.set;   (* set of tokens  *)

type RHS    = Atom.atom list;  (* The RHS γ of a rule A -> γ *)

(*

We have the structures AtomSet and AtomMap to represent sets and maps
of Atoms. For any type t if we want sets and maps (dictionaries) we
need an ordering structure on the elements.  We would like to create
the set structure on RHS's. For this you first need to define a
structure of signature ORD_KEY for RHS.

*)

structure RHS_KEY = 
struct
	type ord_key = RHS
	val compare = List.collate Atom.lexCompare
end;


(*

Use the above structure to create a set of rhs's

*)

structure RHSSet = RedBlackSetFn (RHS_KEY);

type Productions = RHSSet.set;

(* The rules of the grammar are a dictionary whose keys are the symbol
   and the values are the Productions associated with the grammar.
*)

type Rules = Productions AtomMap.map;


type Grammar = { symbols : Symbols, tokens : Tokens, rules : Rules };
