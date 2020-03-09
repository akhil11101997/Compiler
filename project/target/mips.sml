signature TEMP = sig
	type temp  (* temporary variables of your program *)
	type label (* temporary labels for your program *)
        val newlabel : unit -> label
        val newtemp  : unit -> temp
	val valtemp  : temp -> int
	val vallabel : label -> int
	val assigntemp : temp * int -> unit
	val assignlabel : label * int -> unit
end;

structure Temp :> TEMP = struct
	structure Map = IntRedBlackMap


	type temp = Map.Key.ord_key
	type label = Map.Key.ord_key


	val tempcount = ref 0
	val lablecount = ref 0
	

	val local_temp : int ref Map.map ref = ref Map.empty;
	val local_lable : int ref Map.map ref = ref Map.empty;

	
	fun newtemp () = ( tempcount := !tempcount +1; local_temp := Map.insert (!local_temp, !tempcount, ref 0 ); !tempcount )
	fun newlabel () = ( lablecount := !lablecount +1; local_lable := Map.insert(!local_lable, !lablecount, ref 0); !lablecount)

	fun valtemp x = !(Map.lookup (!local_temp, x))
	fun vallabel x = !(Map.lookup (!local_lable, x))

	
	fun assigntemp (x, y) = (Map.lookup (!local_temp, x)) := y
	fun assignlabel (x, y) = (Map.lookup (!local_lable, x)) := y
end;


structure MIPS_module = struct
    datatype ('t, 'l) inst = ADD of 't * 't * 't 
                 | ADDI of 't * 't * int
                 | ADDU of 't * 't * 't 
                 | ADDIU of 't * 't * int
                 | SUB of 't * 't * 't 
                 | SUBU of 't * 't * 't 
                 | MUL of 't * 't * 't 
                 | MULT of 't * 't 
                 | DIV of 't * 't 
                 | AND of 't * 't * 't 
                 | ANDI of 't * 't * int
                 | OR of 't * 't * 't 
                 | ORI of 't * 't * int
                 | SLL of 't * 't * int
                 | SRL of 't * 't * int
                 | LW of 't * int * 't 
                 | LI of 't * int
                 | LA of 't * 'l
                 | LUI of 't * int
                 | SW of 't * int * 't 
                 | MFHI of 't 
                 | MFLO of 't 
                 | MOVE of 't * 't 
                 | BEQ of 't * 't * 'l
                 | BNE of 't * 't * 'l
                 | BGT of 't * 't * 'l
                 | BGE of 't * 't * 'l
                 | BLT of 't * 't * 'l
                 | BLE of 't * 't * 'l
                 | SLT of 't * 't * 't 
                 | SLTI of 't * 't * int
                 | J of 'l
                 | JR of 't 
                 | JAL of 'l
                 | SYSCALL;

    fun prettyPrintExp (ADD (x, y, z)) = TextIO.print ("add $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (ADDI (x, y, z)) = TextIO.print ("addi $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (ADDU (x, y, z)) = TextIO.print ("addu $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (ADDIU (x, y, z)) = TextIO.print ("addiu $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (SUB (x, y, z)) = TextIO.print ("sub $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (SUBU (x, y, z)) = TextIO.print ("subu $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (MUL (x, y, z)) = TextIO.print ("mul $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (MULT (x, y)) = TextIO.print ("mult $" ^ Int.toString x ^ " $" ^ Int.toString y ^ "\n")
    |   prettyPrintExp (DIV (x, y)) = TextIO.print ("div $" ^ Int.toString x ^ " $" ^ Int.toString y ^ "\n")
    |   prettyPrintExp (AND (x, y, z)) = TextIO.print ("and $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (ANDI (x, y, z)) = TextIO.print ("andi $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (OR (x, y, z)) = TextIO.print ("or $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (ORI (x, y, z)) = TextIO.print ("ori $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^"\n")
    |   prettyPrintExp (SLL (x, y, z)) = TextIO.print ("sll $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (SRL (x, y, z)) = TextIO.print ("srl $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (LW (x, y, z)) = TextIO.print ("lw $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (LI (x, y)) = TextIO.print ("li $" ^ Int.toString x ^ " $" ^ Int.toString y ^ "\n")
    |   prettyPrintExp (LA (x, y)) = TextIO.print ("la $" ^ Int.toString x ^ " " ^ Int.toString y ^ "\n")
    |   prettyPrintExp (LUI (x, y)) = TextIO.print ("lui $" ^ Int.toString x ^ " $" ^ Int.toString y ^ "\n")
    |   prettyPrintExp (SW (x, y, z)) = TextIO.print ("sw $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (MFHI x) = TextIO.print ("mhfi $" ^ Int.toString x ^ "\n")
    |   prettyPrintExp (MFLO x) = TextIO.print ("mflo $" ^ Int.toString x ^ "\n")
    |   prettyPrintExp (MOVE (x, y)) = TextIO.print ("move $" ^ Int.toString x ^ " $" ^ Int.toString y ^ "\n")
    |   prettyPrintExp (BEQ (x, y, z)) = TextIO.print ("beq $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (BNE (x, y, z)) = TextIO.print ("bnq $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (BGT (x, y, z)) = TextIO.print ("bgt $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (BGE (x, y, z)) = TextIO.print ("bge $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (BLT (x, y, z)) = TextIO.print ("blt $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (BLE (x, y, z)) = TextIO.print ("ble $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (SLT (x, y, z)) = TextIO.print ("slt $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (SLTI (x, y, z)) = TextIO.print ("slti $" ^ Int.toString x ^ " $" ^ Int.toString y ^ " $" ^ Int.toString z ^ "\n")
    |   prettyPrintExp (J x) = TextIO.print ("j " ^ Int.toString x ^ "\n")
    |   prettyPrintExp (JR x) = TextIO.print ("jr $" ^ Int.toString x ^ "\n")
    |   prettyPrintExp (JAL x) = TextIO.print ("jal " ^ Int.toString x ^ "\n")
    |   prettyPrintExp _ = TextIO.print "syscall\n";
    
end;










							
						









				    
