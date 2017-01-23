open Big_int
module E = Errormsg

(** Unary Operations*)
type unop =
  | Cast                                (** Cast expression*)
  | Neg                                 (** Unary minus *)
  | BNot                                (** Bitwise complement (~) *)
  | LNot                                (** Logical Not (!) *)
(** Binary operations *)
and binop =
  | PlusA                               (** arithmetic + *)
  | MinusA                              (** arithmetic - *)
  | Mult                                (** * *)
  | Div                                 (** / *)
  | Mod                                 (** % *)
  | Shiftlt                             (** shift left *)
  | Shiftrt                             (** shift right *)
  | Lt                                  (** <  (arithmetic comparison) *)
  | Gt                                  (** >  (arithmetic comparison) *)  
  | Le                                  (** <= (arithmetic comparison) *)
  | Ge                                  (** >  (arithmetic comparison) *)
  | Eq                                  (** == (arithmetic comparison) *)
  | Ne                                  (** != (arithmetic comparison) *)            
  | BAnd                                (** bitwise and *)
  | BXor                                (** bitwise exclusive-or *)
  | BOr                                 (** bitwise inclusive-or *)
and funmodule = {
	mutable vdesc: vvarinfo;
		(** The name and output type of the function *) 
	mutable vinputs: vvarinfo list;
		(** The parameters to the function *)
	mutable vlocals: vvarinfo list;
		(** The local variables in the function *)
	mutable vmodules: vmodule list;
		(** The modules that provide internal functionality *)
}
and vvarinfo = {
	mutable varname: string;
		(** The name of the variable *)
	mutable vtype: vtype;
		(** the type of this constant *)
}
and vconstinfo = {
	mutable value: big_int;
		(** the value of this constant *)
	mutable ctype: vtype;
		(** the type of this constant *)
}
and vconnection = {
	mutable connectfrom: int option;
		(** The id of the exporting module, none means from the start *)
	mutable connectto: int option;
		(** The id of the importing module, none means computation ends *)
	mutable requires: (voperation * bool) option;
		(** Optional requirement for exporting, The value of the operation 
			with the given id must have the same c truth value as the bool 
			provided *)
}
and vmodule = {
	mutable mid: int;
		(** The id of this module in the function *)
	mutable mvars: vvarinfo list;
		(** The variables inputed to this module *)
	mutable mvarexports: vvarinfo list;
		(** The variables outputed from this module *)
	mutable minputs: vconnection list;
		(** The incoming connections *)
	mutable moutputs: vconnection list;
		(** The posible modules to hand control flow to *)
	mutable mdataFlowGraph: voperation list;
}
and voperation = {
	mutable oid: int;
		(** id used to remove duplicates inside a module *)
	mutable operation: voperationtype;
		(** the type of this operation*)
	mutable ousecount: int;
		(** the number of users of this operation *)
	mutable oschedule: vscheduleinfo;
		(** the scheduling data of this operation *)
}
and vscheduleinfo = {
	earliest: int;
		(** asap schedule *)
	latest: int;
		(** alap schedule*)
	set: int;
		(** actual schedule *)
}
and voperationtype = 
	| Variable of vvarinfo
		(** The value of a variable as it enters the module *)
	| Constant of vconstinfo
		(** The value of a constant *)
	| Result of vvarinfo * voperation
		(** marks the value of a variable that should be passed to the next 
			stage *)
	| ReturnValue of voperation
		(** marks the value to be returned *)
	| Unary of unop * voperation * vtype
		(** applies a unary operation to the previous item *)
	| Binary of binop * voperation * voperation * vtype
		(** applies a binary operation to the previous items *)  
and vtype = {
	mutable width: int;
		(** The width of the type, in bits *)
	mutable isSigned: bool;
		(** Whether this is a signed variable *)
}

let emptyschedule = {earliest = 0; latest = 0; set = 0};;

(** The following functions produce a JSON like dump of all the 
 *  information inside any of the above data types. Since there are
 *  typically multiple references to certain items within modules,
 *  some items are represented only by an id.
 *)
let rec string_of_unop u = match u with
  | Neg -> "Neg"
  | BNot -> "BNot"
  | LNot -> "LNot"
  | Cast -> "Cast"
and string_of_binop b = match b with
  | PlusA  -> "PlusA "
  | MinusA -> "MinusA"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Shiftlt -> "Shiftlt"
  | Shiftrt -> "Shiftrt"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Le -> "Le"
  | Ge -> "Ge"
  | Eq -> "Eq"
  | Ne -> "Ne"
  | BAnd -> "BAnd"
  | BXor -> "BXor"
  | BOr -> "BOr"
and string_of_funmodule f = 
	"{ vdesc:" ^ (string_of_vvarinfo f.vdesc)
	^ ", vinputs:[" ^ (String.concat ", " (List.map string_of_vvarinfo f.vinputs))
	^ "], vlocals:[" ^ (String.concat ", " (List.map string_of_vvarinfo f.vlocals))
	^ "], vmodules:[" ^ (String.concat ", " (List.map string_of_vmodule f.vmodules))
	^ "]}"
and string_of_vvarinfo v = 
	"{ varname:\"" ^ v.varname
	^ "\", vtype:" ^ (string_of_vtype v.vtype)
	^ "}"
and string_of_vconstinfo c = 
	"{ value:" ^ (string_of_big_int c.value)
	^ ", vtype:" ^ (string_of_vtype c.ctype)
	^ "}"
and string_of_vconnection c = 
	"{ connectfrom:" ^ (match c.connectfrom with 
		| Some i -> (string_of_int i)
		| None -> "None"
	)
	^ ", connectto:" ^ (match c.connectto with 
		| Some i -> (string_of_int i)
		| None -> "None"
	)
	^ ", requires:" ^ (match c.requires with 
		| Some (o,b) -> "Some:(" ^ (string_of_int o.oid) ^ ", " ^ (string_of_bool b) ^ ")"
		| None -> "None"
	)
	^ "}"
and string_of_vmodule m = 
	"{ mid:" ^ (string_of_int m.mid)
	^ ", minputs:[" ^ (String.concat ", " (List.map string_of_vconnection m.minputs))
	^ "], moutputs:[" ^ (String.concat ", " (List.map string_of_vconnection m.moutputs))
	^ "], mdataFlowGraph:[" ^ (String.concat ", " (List.map string_of_voperation m.mdataFlowGraph))
	^ "]}"
and string_of_voperation o = 
	"{ oid:" ^ (string_of_int o.oid)
	^ ", ousecount:" ^ (string_of_int o.ousecount) 
	^ ", oschedule:" ^ (string_of_vscheduleinfo o.oschedule)
	^ ", operation:" ^ (string_of_voperationtype o.operation)
	^ "}"
and string_of_vscheduleinfo si = 
	"{ earliest:" ^ (string_of_int si.earliest)
	^ ", latest:" ^ (string_of_int si.latest) 
	^ ", set:" ^ (string_of_int si.set)
	^ "}"
and string_of_voperationtype vt = "{" ^ (match vt with
	| Variable v -> "Variable:" ^ (string_of_vvarinfo v)
	| Constant c -> "Constant:" ^ (string_of_vconstinfo c)
	| Result (v,o) -> "Result:(" ^ (string_of_vvarinfo v) ^ ", " ^ (string_of_int o.oid) ^ ")"
	| ReturnValue o -> "ReturnValue:(" ^ (string_of_int o.oid) ^ ")"
	| Unary (u,o,t) -> "Unary:(" ^ (string_of_unop u) ^ ", " ^ (string_of_int o.oid) ^ ", " 
		^ (string_of_vtype t) ^ ")" 
	| Binary (u,o1,o2,t) -> "Binary:(" ^ (string_of_binop u) ^ ", " ^ (string_of_int o1.oid) 
		^ ", " ^ (string_of_int o2.oid) ^ ", " ^ (string_of_vtype t) ^ ")" 
	) ^ "}"
and string_of_vtype t = 
	"{ width:" ^ (string_of_int t.width)
	^ ", isSigned:" ^ (string_of_bool t.isSigned)
	^ "}"

(*  The following functions attempt to give a quick readable
 *  output from the various types, intented for console output
 *)
let rec print_unop u = match u with
  | Neg -> "-"
  | BNot -> "~"
  | LNot -> "!"
  | Cast -> "<cast>"
and print_binop b = match b with
  | PlusA  -> "+"
  | MinusA -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Shiftlt -> "<<"
  | Shiftrt -> ">>"
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Eq -> "=="
  | Ne -> "!="
  | BAnd -> "&"
  | BXor -> "^"
  | BOr -> "|"
and print_funmodule f = 
	(print_vvarinfo f.vdesc)
	^ "\n\tinputs:[" ^ (String.concat ", " (List.map print_vvarinfo f.vinputs))
	^ "]\n\tlocals:[" ^ (String.concat ", " (List.map print_vvarinfo f.vlocals))
	^ "]\n\tmodules:\n\t" ^ (String.concat "\n\t" (List.map print_vmodule f.vmodules))
and print_vvarinfo v = 
	v.varname ^ ": " ^ (print_vtype v.vtype)
and print_vconstinfo c = 
	(string_of_big_int c.value) ^ ": " ^ (print_vtype c.ctype)
and print_vconnections (c:vconnection list) = match c with
	| [{connectfrom=f;connectto=t;requires=None}] -> "-> " ^ (match t with 
		| Some i -> (string_of_int i)
		| None -> "return"
	)
	| [{connectfrom=f1;connectto=tt;requires=Some(o1,true)};{connectfrom=f2;connectto=tf;requires=Some(o2,false)}] when f1 = f2 || o1 = o2
		-> "-> " ^ (string_of_int o1.oid) ^ " ? " ^ (match tt with 
		| Some i -> (string_of_int i)
		| None -> "return"
	) ^ " : " ^ (match tf with 
		| Some i -> (string_of_int i)
		| None -> "return"
	)
	| _ -> "[" ^ (String.concat ", " (List.map string_of_vconnection c)) ^ "]"
and print_vmodule m = 
	(string_of_int m.mid) ^ " " ^ (print_vconnections m.moutputs) 
	^ "    ( " ^ (String.concat ", " (List.map (fun v -> v.varname) m.mvars))
	^ " ) -> ( " ^ (String.concat ", " (List.map (fun v -> v.varname) m.mvarexports))
	^ " )\n\t\t" ^ (String.concat "\n\t\t" (List.map print_voperation m.mdataFlowGraph))
and print_voperation o = 
	(string_of_int o.oid) ^ " ==== " ^ (print_voperationtype o.operation)
and print_voperationtype vt = match vt with
	| Variable v -> print_vvarinfo v
	| Constant c -> print_vconstinfo c
	| Result (v,o) -> v.varname ^ " = <" ^ (string_of_int o.oid) ^">"
	| ReturnValue o -> "return <" ^ (string_of_int o.oid) ^ ">"
	| Unary (u,o,t) -> (print_unop u) ^ "<" ^ (string_of_int o.oid) ^ ">: " 
		^ (print_vtype t) 
	| Binary (u,o1,o2,t) -> "<" ^ (string_of_int o1.oid) ^ "> " ^ (print_binop u) ^ " <" ^ 
		(string_of_int o2.oid) ^ ">: " ^ (print_vtype t)
and print_vtype t = 
	(string_of_int t.width) ^ "'" ^ if t.isSigned then "s" else "u"



(* the following functions check for equality between various types
 * there is several pieces of information left out, if it's not deemed
 * necessary to the equality test *)
let eq_type (t1:vtype) (t2:vtype) = 
	t1.width = t2.width && t1.isSigned = t2.isSigned
let eq_unop (u1:unop) (u2:unop) = match (u1,u2) with
	| (Neg,Neg) -> true
	| (BNot,BNot) -> true
	| (LNot,LNot) -> true
	| _ -> false
let eq_binop (b1:binop) (b2:binop) = match (b1,b2) with
	| (PlusA,PlusA) -> true
	| (MinusA,MinusA) -> true
	| (Mult,Mult) -> true
	| (Div,Div) -> true
	| (Mod,Mod) -> true
	| (Shiftlt,Shiftlt) -> true
	| (Shiftrt,Shiftrt) -> true
	| (Lt,Lt) -> true
	| (Gt,Gt) -> true
	| (Le,Le) -> true
	| (Ge,Ge) -> true
	| (Eq,Eq) -> true
	| (Ne,Ne) -> true
	| (BAnd,BAnd) -> true
	| (BXor,BXor) -> true
	| (BOr,BOr) -> true
	| _ -> false
let eq_operation_type (ot1:voperationtype) (ot2:voperationtype) = 
	match (ot1,ot2) with
	| (Variable v1, Variable v2) when v1.varname = v2.varname -> true
	| (Constant c1, Constant c2) when (eq_big_int c1.value c2.value && eq_type c1.ctype c2.ctype) -> true
	| (Result (v1, o1), Result (v2, o2)) when v1.varname = v2.varname && o1.oid = o2.oid -> true
	| (Unary (u1,o1,t1), Unary (u2,o2,t2)) when eq_unop u1 u2 && o1.oid = o2.oid && eq_type t1 t2 -> true
	| (Binary (b1,o11,o21,t1), Binary(b2,o12,o22,t2)) when eq_binop b1 b2 && o11.oid = o12.oid && o21.oid = o22.oid && eq_type t1 t2 -> true
	| _ -> false
let eq_operation (o1:voperation) (o2:voperation) = 
	o1.oid = o2.oid && eq_operation_type o1.operation o2.operation

(* gets the children of an operation *)
let getchildren (o:voperation) = 
	match o.operation with
	| Result (_,o1) -> [o1]
	| ReturnValue o1 -> [o1]
	| Unary (_,o1,_) -> [o1]
	| Binary (_,o1,o2,_) -> [o1;o2]
	| _ -> []

(* checks whether the children are in a given list, or returns default
 * if o has no children *)
let childreninlist (default:bool) (o:voperation) (l:voperation list) = 
	match o.operation with
	| Result (_,o1) -> List.memq o1 l
	| ReturnValue o1 -> List.memq o1 l
	| Unary (_,o1,_) -> List.memq o1 l
	| Binary (_,o1,o2,_) -> List.memq o1 l && List.memq o2 l
	| _ -> default

(* gets scheduling offset for o *)
let operationoffset (o:voperation) = match o.operation with
	| Unary (Cast,_,_) -> 0 (* cast is instant *)
	| Unary (_,_,_)  
	| Binary(_,_,_,_) -> 1 (* other operators take 1 step *)
	| _ -> 0 (* results, consts returnvalues and variables are instant *)

(* helper functions for manipulating vil objects *)

let getentrypoint (f:funmodule) = List.find (fun m -> match m.minputs with
		| [{connectfrom=None;connectto=Some _;requires=None}] -> true
		| _ -> false
	) f.vmodules

(* extracts module from an id *)
let modulefromintoption (f:funmodule) (io:int option) = match io with
	| Some i -> Some (List.find (fun m -> m.mid = i) f.vmodules)
	| None -> None

(* gives a sucessor list *)
let getmodulesucessors (f:funmodule) (m:vmodule) :vmodule list = 
	Listutil.mapfilter (fun c -> modulefromintoption f c.connectto) m.moutputs

(* gives a predecessor list *)
let getmodulepredecessors (f:funmodule) (m:vmodule) :vmodule list = 
	Listutil.mapfilter (fun c -> modulefromintoption f c.connectfrom) m.minputs

(* is variable v in list l ? (name equality)*)
let variableinlist (v:vvarinfo) (l:vvarinfo list) = 
	List.exists (fun v1 -> v1.varname = v.varname) l

(* does m have a result operation for v?*)
let hasvariableresult (v:vvarinfo) (m:vmodule) = 
	List.exists (fun op -> match op.operation with
			| Result (v1,_) when v1.varname = v.varname -> true
			| _ -> false
		) m.mdataFlowGraph

(* doesn m have a variable operation for v? *)
let hasvariableuse (v:vvarinfo) (m:vmodule) = 
	List.exists (fun op -> match op.operation with
			| Variable v1 when v1.varname = v.varname -> true
			| _ -> false
		) m.mdataFlowGraph

(* increment operation use count *)
let incoperationcount (op:voperation) = 
	op.ousecount <- op.ousecount + 1

(* decrement operation use count *)
let decoperationcount (op:voperation) = 
	op.ousecount <- op.ousecount - 1

(* applies f to the immediate children of op *)
let dotoimmediatechildren (f:voperation -> unit) (op:voperation) = 
	match op.operation with
	| Unary(_,o,_) -> f o
	| Binary(_,o1,o2,_) -> f o1; f o2
	| Result(_,o) -> f o
	| ReturnValue o -> f o
	| _ -> ()

(* simple uses of dotoimmediatechildren for tracking use counts *)
let incchildren = dotoimmediatechildren incoperationcount;;
let decchildren = dotoimmediatechildren decoperationcount;;

(* get switching points *)
let getswitches (m:vmodule) = 
	let foundids = ref []
	in (Listutil.mapfilter (fun c -> match c.requires with
		| Some(o,_) when not (List.mem o.oid !foundids) -> 
			foundids := o.oid :: ! foundids;
			Some(o)
		| _ -> None) m.moutputs)

(* get type of an operation *)
let rec gettype (o:voperation) = match o.operation with
	| Variable v -> v.vtype
	| Constant c -> c.ctype
	| Result (v,_) -> v.vtype
	| ReturnValue o1 -> gettype o1
	| Unary (_,_,t) -> t
	| Binary (_,_,_,t) -> t

(* get the name of a function *)
let functionname (f:funmodule) = f.vdesc.varname

(* gets the latest scheduled item in a vmodule *)
let maxtime (m:vmodule) = 
	List.fold_left (fun a o -> max o.oschedule.set a) 0 m.mdataFlowGraph

(* code for filling in the schedule *)

(* sets the asap schedule for o *)
let asap (o:voperation) = o.oschedule <- 
	{
		earliest = (List.fold_left max 0 (List.map (fun o1 -> o1.oschedule.earliest + (operationoffset o1)) (getchildren o)));
		latest = o.oschedule.latest; 
		set = o.oschedule.set;
	}

(* sets the alap schedule for o*)
let alap (latest:int) (ops:voperation list) (o:voperation) = o.oschedule <- 
	{
		earliest = o.oschedule.earliest; 
		latest = (let users = List.filter (fun o1 -> List.memq o (getchildren o1)) ops 
			in match users with
				| [] -> latest
				| h::t -> (List.fold_left min h.oschedule.latest (List.map (fun o1 -> o1.oschedule.latest - (operationoffset o1)) t))
		);
		set = o.oschedule.set;
	}

(* builds asap schedule for ops (start with acc=[]) *)
let rec generateasap (acc:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> ()
	| _ -> let (ready,notready) = List.partition (fun o -> childreninlist true o acc) ops
		in List.iter asap ready;
			generateasap (List.rev_append ready acc) notready
(* builds alap schedule for ops (start with acc=[] and latest as the maximum asap value) *)
let rec generatealap (latest:int) (acc:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> ()
	| _ -> let (notready,ready) = List.partition (fun o -> List.exists (fun o1 -> List.memq o (getchildren o1)) ops) ops
		in List.iter (alap latest acc) ready;
			generatealap latest (List.rev_append ready acc) notready

(* generates an overall schedule for the module *)
let rec generateschedule (m:vmodule) =
	List.iter (fun o -> o.oschedule <- (* TODO add non trivial scheduler *)
	{earliest=o.oschedule.earliest;latest=o.oschedule.latest;set=o.oschedule.earliest}) m.mdataFlowGraph

(* generates schedules for all modules *)
let generatescheduleinfo (f:funmodule) = 
	List.iter (fun m -> 
		generateasap [] m.mdataFlowGraph; 
		generatealap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
			0 m.mdataFlowGraph) [] m.mdataFlowGraph;
		generateschedule m;
	) f.vmodules

