open Big_int
module E = Errormsg

type vastunop = 
	| UPLUS
	| UMINUS
	| ULNOT
	| UBNOT
	| UAND
	| UOR
	| UXOR
	| UNAND
	| UNOR
	| UXNOR
and vastbinop = 
	| PLUS
	| MINUS
	| MULT
	| DIV
	| MOD
	| EQ
	| NEQ
	| EQ4S
	| NEQ4S
	| LAND
	| LOR
	| LT
	| LE
	| GT
	| GE
	| AND
	| OR
	| XOR
	| LSHIFT
	| RSHIFT
and vastmodule = {
	mutable modname: string;
		(* module name *)
	mutable inputs: vastvariable list;
		(* inputs to the module (clk and rst are automatically added) *)
	mutable outputs: vastvariable list;
		(* inputs to the module *)
	mutable locals: vastvariable list;
		(* local variables for the module *)
	mutable always: vastassignment list;
		(* the always block*)
	mutable clockedge: vastassignment list;
		(* the @(posedge clk) block (reset behaviour is added automatically) *)
}
and vastinitialiser = 
	| SINGLE of big_int
	| ARRAY of vastinitialiser list
and vastvariable = {
	mutable name: string;
		(* variable name *)
	mutable resetto: vastinitialiser;
		(* value to set in the reset block *)
	mutable typ: vasttype;
		(* the variable type *)
}
and vastconstant = {
	mutable value: big_int;
		(* constant value *)
	mutable cwidth: int;
		(* width of the constant *)
}
and vasttype = {
	mutable width: int;
		(** The width of the type, in bits *)
	mutable isSigned: bool;
		(** Whether this is a signed variable *)
	mutable logictype: vastlogic;
		(** The kind of logic this is implemented with *)
	mutable arraytype: int list;
		(** [] represents basic type, [5] represents e.g. a reg [3:0] v [5] *)
}
and vastlogic = 
	| WIRE (* indicates a wire *)
	| REG  (* indicates a register *)
	| NA   (* we don't know (e.g. inputs) *)
and vastassignment = {
	mutable var: vastlval;
		(* variable to assign to *)
	mutable assign: vastexpression;
		(* the expression assigned *)
	mutable blocking: bool;
		(* whether to use blocking assignment =
		 * or non blocking assignment <= *)
}
and vastlval = {
	mutable variable: vastvariable;
		(* the variable *)
	mutable range: (int * int) option;
		(* options bit range *)
	mutable indexing: vastexpression list;
		(* the indexes into the array type *)
}
and vastexpression = 
	| CONST of vastconstant (* represents a constant *)
	| VARIABLE of vastlval (* represents variable data *)
	| UNARY of vastunop * vastexpression (* unary operation & b etc. *)
	| BINARY of vastbinop * vastexpression * vastexpression (* binary operation a + b etc. *)
	| TERNARY of vastexpression * vastexpression * vastexpression (* ternary c ? t : f *)
	| CONCAT of vastexpression list (* concatenation {a,b,c} *)

let isReg l = match l with
| REG -> true
| _ -> false

(** functions for getting a verilog string out of the structure *)
let rec vastunop_to_verilog u = match u with
	| UPLUS  -> "+"
	| UMINUS -> "-"
	| ULNOT  -> "!"
	| UBNOT  -> "~"
	| UAND   -> "&"
	| UOR    -> "|"
	| UXOR   -> "^"
	| UNAND  -> "~&"
	| UNOR   -> "~|"
	| UXNOR  -> "~^"
and vastbinop_to_verilog b = match b with
	| PLUS   -> "+"
	| MINUS  -> "-"
	| MULT   -> "*"
	| DIV    -> "/"
	| MOD    -> "%"
	| EQ     -> "=="
	| NEQ    -> "!="
	| EQ4S   -> "==="
	| NEQ4S  -> "!=="
	| LAND   -> "&&"
	| LOR    -> "||"
	| LT     -> "<"
	| LE     -> "<="
	| GT     -> ">"
	| GE     -> ">="
	| AND    -> "&"
	| OR     -> "|"
	| XOR    -> "^"
	| LSHIFT -> "<<"
	| RSHIFT  -> ">>"
and vastmodule_to_verilog m = 
	"module " ^ m.modname ^ "( input clk, input rst" 
		^ (String.concat "" (List.map 
			(fun v -> ", input " ^ (vastvariable_to_verilog v)) m.inputs))
		^ (String.concat "" (List.map 
			(fun v -> ", output " ^ (vastvariable_to_verilog v)) m.outputs))
		^ ");\n" 
	^ (String.concat "" (List.map 
			(fun v -> (vastvariable_to_verilog v) ^ ";\n") m.locals))
	^ "initial begin\n    "
		^ (String.concat) "\n    " (Listutil.mapflatten vastvariable_to_initial_assignment m.outputs)
		^ "\n    "
		^ (String.concat) "\n    " (Listutil.mapflatten vastvariable_to_initial_assignment m.locals)
	^ "\nend\n    "
		^ (String.concat "" (List.map 
			(fun a -> "assign " ^ (vastassignment_to_verilog a) ^ ";\n    ") m.always))
	^ "\n    always_ff @(posedge clk)\n    if(rst)\n         begin\n            "
			^ (String.concat "    " (Listutil.mapfilter
				(fun v -> vastvariable_to_reset_assignment v) m.outputs))
			^ "    "
			^ (String.concat "    " (Listutil.mapfilter
				(fun v -> vastvariable_to_reset_assignment v) m.locals))
		^ "end\n    else\n        begin\n            "
			^ (String.concat "        " (List.map 
			(fun a -> (vastassignment_to_verilog a) ^ ";\n    ") m.clockedge))
		^ "    end\n"
	^ "endmodule // end of module " ^ m.modname
and vastvariable_to_reset_assignment v = 
	if(isReg v.typ.logictype)
	then match v.resetto with
		| SINGLE c -> Some (v.name ^ " <= " ^ (string_of_int v.typ.width) ^ "'d" 
			^ string_of_big_int c ^ ";\n        ")
		| _ -> None
	else None
and vastvariable_to_initial_assignment v = 
	if(isReg v.typ.logictype)
	then let rec driver s t i = match (t, i) with
			| ([],SINGLE c) -> [s ^ " = " ^ (string_of_int v.typ.width) ^ "'d" 
				^ string_of_big_int c ^ ";" ]
			| (hd::tl,ARRAY il) when List.length il = hd -> List.flatten 
				(List.mapi 
					(fun ix ni -> driver (s^"["^string_of_int ix^"]") tl ni) 
				il)
			| _ -> E.s(E.error "Incorrect initialiser for %s\n" v.name)
		in driver v.name v.typ.arraytype v.resetto 
	else []
and vastinitialiser_to_verilog i = match i with
	| SINGLE b -> [string_of_big_int b]
	| ARRAY il -> List.flatten (List.map vastinitialiser_to_verilog il)
and vastvariable_to_verilog v = let (b,a) = vasttype_to_verilog v.typ
	in b ^ v.name ^ a
and vastconstant_to_verilog c = (string_of_int c.cwidth) ^ "'d" ^ (string_of_big_int c.value)
and vasttype_to_verilog t = (
		(vastlogic_to_verilog t.logictype)
			^ (if t.isSigned then "signed " else "")
			^ (if (t.width < 2) then "" else "[" ^ (string_of_int (t.width - 1)) ^ ":0] "),
		String.concat "" (List.map (fun i -> "[" ^ string_of_int i ^ "]") t.arraytype)
	)
and vastlogic_to_verilog l = match l with
	| WIRE -> "wire "
	| REG  -> "reg "
	| NA   -> ""
and vastassignment_to_verilog a = 
	(vastlval_to_verilog a.var)
	^ (if a.blocking then " = " else " <= ")
	^ (vastexpression_to_verilog a.assign)
and vastlval_to_verilog l = 
	l.variable.name ^ 
	String.concat "" (List.map (fun e -> "[" ^ vastexpression_to_verilog e ^ "]") l.indexing)
	^ (match l.range with
		| None -> ""
		| Some (i,j) -> "[" ^ (string_of_int i) ^ ":" ^ (string_of_int j) ^ "]")
and vastexpression_to_verilog e = "(" ^ (match e with
		| CONST c -> vastconstant_to_verilog c
		| VARIABLE l -> vastlval_to_verilog l
		| UNARY (u,e1) -> (vastunop_to_verilog u) ^ (vastexpression_to_verilog e1)
		| BINARY(b,e1,e2) -> (vastexpression_to_verilog e1) ^ 
			(vastbinop_to_verilog b) ^ (vastexpression_to_verilog e2)
		| TERNARY (e1,e2,e3) -> (vastexpression_to_verilog e1) ^ "?" ^
			(vastexpression_to_verilog e2) ^ ":" ^ (vastexpression_to_verilog e3)
		| CONCAT el -> "{" ^ (String.concat "," (List.map 
			(fun e -> vastexpression_to_verilog e) el)) ^ "}"
	) ^ ")"

(** useful helper functions *)

(* gets a variable by name *)
let getvar (m:vastmodule) (s:string) = 
	try (List.find (fun v -> v.name = s) m.inputs)
	with | Not_found -> 
		try (List.find (fun v -> v.name = s) m.outputs)
		with | Not_found -> 
			try (List.find (fun v -> v.name = s) m.locals)
			with | Not_found -> E.s (E.error "Can't find %s\n" s)
	