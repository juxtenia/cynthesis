open Vil
open Big_int
module E = Errormsg

(** the amount of iterations to assume loops perform if there's no other information *)
let averageloopcount = ref 40

type tristate = 
	| Const of big_int
	| Notconst
	| Unknown

let tristatemerge t1 t2 = match (t1,t2) with
	| (Unknown,_) -> t2
	| (_,Unknown) -> t1
	| (Notconst,_) 
	| (_,Notconst) -> Notconst
	| (Const c1, Const c2) -> if eq_big_int c1 c2 then Const c1 else Notconst

let tristateadd t1 t2 = match (t1,t2) with
	| (Unknown,_) -> t2
	| (_,Unknown) -> t1
	| (Notconst,_) 
	| (_,Notconst) -> Notconst
	| (Const c1, Const c2) -> Const (add_big_int c1 c2)

let additiontristate (v:vvarinfo) (b:vblock) = 
	if hasvariableresult v b.bdataFlowGraph 
	then match Listutil.mapfilter (fun op -> match op.operation with
			| Result (v1,b,w,o) when v1.varname = v.varname -> Some o
			| _ -> None
		) b.bdataFlowGraph with
	| [Simple {operation=Binary(PlusA,Simple{operation=Variable v1},Simple{operation=Constant c},_)}] 
		when v1.varname = v.varname
	-> Const c.value
	| [Simple {operation=Binary(MinusA,Simple{operation=Variable v1},Simple{operation=Constant c},_)}] 
		when v1.varname = v.varname
	-> Const (minus_big_int c.value)
	| [Simple {operation=Binary(PlusA,Simple{operation=Constant c},Simple{operation=Variable v1},_)}] 
		when v1.varname = v.varname
	-> Const c.value
	| _ -> Notconst
	else Const zero_big_int

let rec allroadsleadtorome (rome:int) (seen:int list) (f:funmodule) (b:vblock) = 
	b.bid = rome || (not (List.mem b.bid seen) && match getblocksucessors f b with
	| [] -> false
	| x -> List.for_all (allroadsleadtorome rome (b.bid::seen) f) x)

let rec allwiththesamedenarii (v:vvarinfo) (rome:int) (seen:int list) (f:funmodule) (t:tristate) (b:vblock) =
	if b.bid = rome then t
	else if List.mem b.bid seen then Unknown
	else match getblocksucessors f b with
	| [] -> Unknown
	| x -> let nextt = tristateadd t (additiontristate v b)
		in List.fold_left (fun t1 b1 -> tristatemerge t1 
			(allwiththesamedenarii v rome (b.bid::seen) f nextt b1)) Unknown x

let onlyfollowedconections (f:funmodule) (endv:int) (b:vblock) = 
	let net = getallsucessors f endv b
	in let netplus = endv :: net
	in List.for_all (fun i -> 
		List.for_all (fun c -> match c.connectfrom with
			| Some i -> List.mem i netplus
			| _ -> false
		) (blockfromint f i).binputs) net

let isloopbody (f:funmodule) (endv:int) (body:int) = 
	let b = blockfromint f body
	in allroadsleadtorome endv [] f b && onlyfollowedconections f endv b

let loopdesc (f:funmodule) (b:vblock) = match b.boutputs with
	| [{connectto=Some bt;requires=Some(o1,true);};
		{connectto=Some bf;requires=Some(o2,false);}] 
		when eq_operation_link o1 o2
	->  (match (isloopbody f b.bid bt, isloopbody f b.bid bf) with
			| (true,true) -> [Some true; Some false]
			| (true,false) -> [Some true]
			| (false,true) -> [Some false]
			| (false,false) -> [])
	| [{connectto=Some t;requires=None;}]
	->  if isloopbody f b.bid t then [None] else []
	| _ -> []

let isloop (f:funmodule) (b:vblock) = Listutil.empty (loopdesc f b)

let isunsafeloop (loopdesc:(bool option) list) = match loopdesc with
	| [None] 
	| [Some true; Some false] -> true
	| _ -> false

let allconst (v:vvarinfo) (bs:vblock list) = match bs with
	| [] -> None
	| h::t -> match getconstvalue v h with
		| Some c -> if List.for_all (fun b -> match getconstvalue v h with 
				| Some c1 -> eq_big_int c c1
				| _ -> false) t
			then Some c
			else None
		| None -> None

let canterminatebasicloop (b:binop) = match b with
	| Lt
	| Gt
	| Le
	| Ge
	| Eq
	| Ne -> true
	| _ -> false

let opswap (b:binop) = match b with
	| Lt -> Gt
	| Gt -> Lt
	| Le -> Ge
	| Ge -> Le
	| _ -> b (** N.B. this won't work for divide, minus, etc! Only for logic! *)

let rec numberofiterations (b:binop) (start:big_int) (increment:big_int) (endv:big_int) = match b with
	| Lt -> if lt_big_int start endv 
			then if le_big_int increment zero_big_int
				then None
				else try Some (int_of_big_int (div_big_int (sub_big_int endv start) increment))
					with | Failure _ -> None
			else Some 0
	| Le -> numberofiterations Lt start increment (succ_big_int endv)
	| Gt -> if gt_big_int start endv 
			then if ge_big_int increment zero_big_int
				then None
				else try Some (int_of_big_int (div_big_int (sub_big_int start endv) (minus_big_int increment)))
					with | Failure _ -> None
			else Some 0
	| Ge -> numberofiterations Gt start increment (pred_big_int endv)
	| _ -> None

let rec unwindcasts (op:voperation) = match op.operation with
	| Unary(Cast,Simple o1,_) -> unwindcasts o1
	| _ -> op

let basicloop (f:funmodule) (b:vblock) (loopdesc:(bool option) list) = match loopdesc with
	| []
	| [None] 
	| _::_::_ -> None
	| [Some b1] -> let (cto,switch,_) = List.find (fun (_,_,b2) -> b2 = b1) (Listutil.mapfilter (fun c ->
			match (c.connectto,c.requires) with
			| (Some i,Some(o,b3)) -> Some(i,o,b3)
			| _ -> None) b.boutputs)
		in match switch with
			| Simple {operation=Binary(bopin,Simple o1,Simple o2,_)} 
				when canterminatebasicloop bopin
			->  let trigger = match (unwindcasts o1,unwindcasts o2) with
					| ({operation=Variable var},{operation=Constant c}) -> Some (bopin,var,c.value)
					| ({operation=Constant c},{operation=Variable var}) -> Some (opswap bopin,var,c.value)
					| _ -> None
				in (match trigger with
					| None -> None
					| Some (bop,v,endv) -> 
						let loopsucc = blockfromint f cto
						in let loopids = getallsucessors f b.bid loopsucc
						in let incrementtristate = allwiththesamedenarii v b.bid [] f Unknown loopsucc
						in let startvalueoption = allconst v (Listutil.mapfilter (fun c -> match c.connectfrom with
								| Some i when not (List.mem i loopids) -> Some (blockfromint f i)
								| _ -> None
							) b.binputs)
						in (match (incrementtristate, startvalueoption) with
							| (Const increment, Some base) -> numberofiterations bop base increment endv
							| _ -> None
						)
					)
			| _ -> None

let getloopcount (f:funmodule) (b:vblock) =
	match loopdesc f b with
	| []
	| [None] 
	| _::_::_ -> None
	| [Some b1] -> match basicloop f b [Some b1] with
		| None -> Some (!averageloopcount,b1)
		| Some i -> Some (i,b1)

let loopinfo (f:funmodule) (b:vblock) = 
	match loopdesc f b with
	| [] -> None
	| x -> Some (
		if isunsafeloop x then "infinite" 
		else (string_of_int b.bid ^ match basicloop f b x with
			| Some i -> " basic(" ^ string_of_int i ^ ")"
			| None -> "")
		)

let inloopbody (f:funmodule) (idin:int) (idof:int) =
	let b = blockfromint f idof
	in List.mem idin (getallsucessors f idof b) && List.mem idin (getallsucessors f idof b)

let constchildren (o:voperation) = match o.operation with
	| Variable _
	| Constant _
	| Result (_,_,_,_) 
	| ReturnValue _ -> false
	| _ -> List.for_all (fun o1 -> match o1.operation with
			| Constant _ -> true
			| _ -> false) (getchildren o)

let rec positivise c t = if lt_big_int c zero_big_int 
		then positivise (add_big_int c (shift_left_big_int unit_big_int t.width)) t
		else c

let normalisewidth (c:big_int) (t:vtypeelement) = 
	let widthshift = shift_left_big_int unit_big_int t.width
	in and_big_int (sub_big_int widthshift unit_big_int) (positivise c t)

let normalisesignwidth (c:big_int) (t:vtypeelement) = 
	let nwidth = normalisewidth c t
	in  if t.isSigned && ge_big_int nwidth (shift_left_big_int unit_big_int (t.width - 1))
		then sub_big_int nwidth (shift_left_big_int unit_big_int t.width)
		else nwidth

let constfromoperationlink (ol:voperationlink) = match ol with
	| Simple {operation=Constant c} -> c.value
	| Simple _ -> raise (Invalid_argument "Not constant children")
	| Compound cll -> let consts = List.map (fun cl -> match cl.loperation.operation with
			| Constant c -> (cl.lbase,cl.lwidth,normalisewidth c.value (gettypeelement c.ctype))
			| _ -> raise (Invalid_argument "Not constant children")) cll
		in let constsranged = List.map (fun (b,w,c) -> 
			(w, and_big_int (sub_big_int (shift_left_big_int unit_big_int w) unit_big_int) 
				(shift_right_big_int c b))) consts
		in let (_,v) = List.fold_left (fun (tw,tc) (w,c) -> 
			(w+tw, add_big_int tc (shift_left_big_int c tw))) (0,zero_big_int) constsranged
		in v

let big_int_of_bool b = if b then unit_big_int else zero_big_int

let evaluate (o:voperation) = match o.operation with
	| Variable _  
	| Constant _ 
	| Result (_,_,_,_) 
	| ReturnValue _ -> raise (Invalid_argument "Not appropriate to evaluate")
	| Unary (u,o1,t) -> 
		let c1 = constfromoperationlink o1
		in let c2 = match u with
			| Cast -> c1
			| Neg -> minus_big_int c1
				(* since - is flip bits and add one in two's complement, we do - then subtract 1 *)
			| BNot -> sub_big_int (minus_big_int c1) unit_big_int
			| LNot -> if eq_big_int c1 zero_big_int then unit_big_int else zero_big_int
		in normalisesignwidth c2 (gettypeelement t)
	| Binary (b,o1,o2,t) -> 
		let te = gettypeelement t
		in let c1 = constfromoperationlink o1
		in let c2 = constfromoperationlink o2
		in let c3 = match b with
			| PlusA -> add_big_int c1 c2
			| MinusA -> sub_big_int c1 c2
			| Mult -> mult_big_int c1 c2
			| Div -> div_big_int c1 c2
			| Mod -> mod_big_int c1 c2
			| Shiftlt -> shift_left_big_int c1 (int_of_big_int c2)
			| Shiftrt -> shift_right_big_int c1 (int_of_big_int c2)
			| Lt -> big_int_of_bool (lt_big_int c1 c2)
			| Gt -> big_int_of_bool (gt_big_int c1 c2)
			| Le -> big_int_of_bool (le_big_int c1 c2)
			| Ge -> big_int_of_bool (ge_big_int c1 c2)
			| Eq -> big_int_of_bool (eq_big_int c1 c2)
			| Ne -> big_int_of_bool (not(eq_big_int c1 c2))
			| BAnd -> and_big_int (positivise c1 te) (positivise c2 te)
			| BXor -> xor_big_int (positivise c1 te) (positivise c2 te)
			| BOr -> or_big_int (positivise c1 te) (positivise c2 te)
		in normalisesignwidth c3 te
	| Ternary (o1,o2,o3,t) -> 
		let c1 = if (eq_big_int (constfromoperationlink o1) zero_big_int)
			then constfromoperationlink o3
			else constfromoperationlink o2
		in normalisesignwidth c1 (gettypeelement t)
				