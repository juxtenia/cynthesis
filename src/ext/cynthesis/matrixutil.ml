module H = Hashtbl

let initialsize = ref 40

(** matrix type*)
type sparsematrix = {
	(** the height of the matrix *)
	h:int;
	(** the width of the matrix *)
	w:int;
	(** hashtable that stores nonzero elements *)
	d:(int*int, float) H.t;
}
(** vector type *)
type sparsevector = {
	(** the length of the vector *)
	l:int;
	(** hashtable that stores nonzero elements *)
	v:(int,float) H.t;
}

(** creates a vector *)
let create_v l = {l=l;v=H.create !initialsize}

(** clears a vector to all zeros *)
let clear_v v = H.reset v.v

(** checks index to a vector *)
let checkbound_v v i = i>=0 && i<v.l

(** sets a value in a vector *)
let set_v v i a = 
	if checkbound_v v i
	then if a = 0.0
		then H.remove v.v i
		else H.replace v.v i a
	else raise (Invalid_argument "Vector index out of range")

(** gets a value from a vector *)
let get_v v i = 
	if checkbound_v v i
	then try H.find v.v i
		with | Not_found -> 0.0
	else raise (Invalid_argument "Vector index out of range")

(** gets a list from the current vector values *)
let tolist_v v = 
	let rec driver acc i =
		if i < 0 then acc
		else driver ((get_v v i) :: acc) (i - 1)
	in driver [] (v.l - 1)

let sum_v v = List.fold_left (+.) 0. (tolist_v v)

(** creates a matrix *)
let create_m h w = {h=h;w=w;d=H.create !initialsize}

(** clears a matrix to all zeros *)
let clear_m m = H.reset m.d

(** checks index to a matrix *)
let checkbound_m m i j = i>=0 && i<m.w && j>=0 && j<m.h

(** sets a value in a matrix *)
let set_m m i j v = 
	if checkbound_m m i j
	then if v = 0.0
		then H.remove m.d (i,j)
		else H.replace m.d (i,j) v
	else raise (Invalid_argument "Matrix index out of range")

(** gets a value from a matrix *)
let get_m m i j = 
	if checkbound_m m i j 
	then try H.find m.d (i,j) 
		with | Not_found -> 0.0
	else raise (Invalid_argument "Matrix index out of range")

(** gets a list of lists from the current matrix values *)
let tolist_m m = 
	let rec driver acc j =
		let rec row acc2 i =
	   		if i < 0 then acc2
			else row ((get_m m i j) :: acc2) (i - 1)
		in
		if j < 0 then acc
		else driver ((row [] (m.w - 1)) :: acc) (j - 1)
	in driver [] (m.h - 1)

(** does m[rd][i]=m[rd][i]+f*m[rs][i] for each item in row rd *)
let rowadd m f rs rd = 
	let rec driver i = 
		if i >= m.w 
		then ()
		else (set_m m i rd ((get_m m i rd) +. f *. (get_m m i rs));
			driver (i+1)  )
	in if f = 0. then () else driver 0

(** subroutine to zero a column c whilst keeping equations consistent.
 *  this is done by adding appropriate multiples of row mx
 *  used by diagonalisation.
 *)
let zerocol m v mx c = 
	let f = (get_m m c mx)
	in let rec driver j = 
		if j>= mx then ()
		else (rowadd m (-.(get_m m c j)/.f) mx j;
			set_v v j ((get_v v j) +. f *. (get_v v mx));
			driver (j+1) )
	in driver 0 

(** put matrix into lower diagonal form (most control flow matrices 
 *  should be quite close to this already) 
 *)
let diagonalise m v = 
	if m.h = m.w && m.h = v.l
	then 
		let rec driver i = 
			if i < 0 then ()
			else (zerocol m v i i; driver (i-1))
		in driver (m.h-1)
	else raise (Invalid_argument "matrix dimensions must be equal and agree with vector")

let solvediagonalised m v = 
	if m.h = m.w && m.h = v.l
	then let ret = create_v v.l
		in let rec driver j = 
			let rec summer t i = 
				if i >= j then t
				else summer (t +. (get_m m i j) *. (get_v ret i)) (i + 1)
			in  if j >= ret.l then ret
				else (set_v ret j (if get_m m j j = 0.
						then 0. 
						else ((summer (-.(get_v v j)) 0)/. -.(get_m m j j))
					); driver (j+1) )
		in driver 0
	else raise (Invalid_argument "matrix dimensions must be equal and agree with vector")

let solveaxb a b = diagonalise a b; solvediagonalised a b