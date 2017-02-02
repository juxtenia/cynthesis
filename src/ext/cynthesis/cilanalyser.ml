open Cil
module E = Errormsg

(** the amount of iterations to assume loops perform if there's no other information *)
let averageloopcount = ref 40

let isbasicloop (vars:(string * constant) list) (s:stmt) = match s.skind with
	| Loop(b,_,Some cl,Some bl) -> None
	| Loop(_,l,_,_) -> E.s (E.error "CFG hasn't run on stmt at %a" d_loc l)
	| _ -> None

let getifweights (sl:stmt list) = Listutil.mapfilter (fun st -> 
	match st.skind with
	| If(_,_,_,_) -> Some (st.sid,0.5)
	| _ -> None
)