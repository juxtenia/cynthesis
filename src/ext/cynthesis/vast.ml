open Big_int

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
	| RSHIT
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
and vastvariable = {
	mutable name: string;
		(* variable name *)
	mutable resetto: big_int;
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
	mutable logictype: vastlogic
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
}
and vastexpression = 
	| CONST of vastconstant (* represents a constant *)
	| VARIABLE of vastlval (* represents variable data *)
	| UNARY of vastunop * vastexpression (* unary operation & b etc. *)
	| BINARY of vastbinop * vastexpression * vastexpression (* binary operation a + b etc. *)
	| TERNARY of vastexpression * vastexpression * vastexpression (* ternary c ? t : f *)
	| CONCAT of vastexpression list (* concatenation {a,b,c} *)

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
	| RSHIT  -> ">>"
and vastmodule_to_verilog m = 
	"module " ^ m.modname ^ "( input clk, input rst" 
		^ (String.concat "" (List.map 
			(fun v -> ", input " ^ (vastvariable_to_verilog v)) m.inputs))
		^ (String.concat "" (List.map 
			(fun v -> ", output " ^ (vastvariable_to_verilog v)) m.outputs))
		^ ")\n" 
	^ (String.concat "" (List.map 
			(fun v -> (vastvariable_to_verilog v) ^ ";\n") m.locals))
	^ "always_comb\n    begin\n        "
		^ (String.concat "    " (List.map 
			(fun a -> (vastassignment_to_verilog a) ^ ";\n    ") m.always))
		^ "end\n"
	^ "always_ff @(posedge clock)\n    if(rst)\n         begin\n            "
			^ (String.concat "    " (List.map 
				(fun v -> vastvariable_to_reset_assignment v) m.outputs))
			^ "    "
			^ (String.concat "    " (List.map 
				(fun v -> vastvariable_to_reset_assignment v) m.locals))
		^ "end\n    else\n        begin\n            "
			^ (String.concat "        " (List.map 
			(fun a -> (vastassignment_to_verilog a) ^ ";\n    ") m.clockedge))
		^ "    end\n"
	^ "endmodule // end of module " ^ m.modname
and vastvariable_to_reset_assignment v = 
	v.name ^ " = " ^ (string_of_int v.typ.width) ^ "'d" 
				^ (string_of_big_int v.resetto) ^ ";\n        "
and vastvariable_to_verilog v = 
	(vasttype_to_verilog v.typ) ^ v.name
and vastconstant_to_verilog c = (string_of_int c.cwidth) ^ "'d" ^ (string_of_big_int c.value)
and vasttype_to_verilog t = 
	(if t.isSigned then "signed " else "")
	^ (vastlogic_to_verilog t.logictype)
	^ (if (t.width < 2) then "" else "[" ^ (string_of_int (t.width - 1)) ^ ":0] ")
and vastlogic_to_verilog l = match l with
	| WIRE -> "wire "
	| REG  -> "reg "
	| NA   -> ""
and vastassignment_to_verilog a = 
	(vastlval_to_verilog a.var)
	^ (if a.blocking then " = " else " <= ")
	^ (vastexpression_to_verilog a.assign)
and vastlval_to_verilog l = 
	l.variable.name ^ (match l.range with
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