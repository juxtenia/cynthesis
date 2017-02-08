open Cil
open Feature
open Vil
module VA = Vast
module E = Errormsg

(** set by a command line flag, sets which outputs to do *)
let printflags = ref 48;;
(** set by a command line flag, sets where to output verilog *)
let outputdir = ref ".";;

(** gets the name of the file in !outputdir with the name [funname].sv *)
let getfilename (funname:string) = 
	let dirname = !outputdir
	(* get system directory separator *)
	in let dirsep = Filename.dir_sep
	in let dirlen = String.length dirname
	in let seplen = String.length dirsep
	in let suff = 
		(* if dirname ends with dirsep then "" else dirsep *)
		if dirlen >= seplen && (String.compare dirsep 
			(String.sub dirname (dirlen - seplen) seplen)) = 0
		then "" else dirsep
		(* return *)
    in dirname ^ suff ^ funname ^ ".sv"

(** writes a string to a file *)
let writestringtofile (file:string) (value:string) = 
	let oc = open_out file in    (* create or truncate file, return channel *)
  	Printf.fprintf oc "%s\n" value;   (* write string *)   
  	close_out oc

(** converts a cil fundec into a vil funmodule, and does appropriate outputs,
 *  before converting it into a vast module, turning that into a string, and 
 *  outputing it to the file with the same name as the function name *)
let funtomodule (f:fundec) = 
	let ret = Viloptimisationsteps.hillclimibingoptimiser (Ciltovil.generatefunmodule f)
	in  
		(* dump module info *)
		if(!printflags land 8 <> 0) then E.log("%s\n") (string_of_funmodule ret) else ();
		(* print more readable module printout *)
  		if(!printflags land 16 <> 0) then E.log("%s\n") (print_funmodule ret) else ();
  		(* print optimiser metrics *)
  		if(!printflags land 64 <> 0) then E.log("Opcost: %d, Timecost: %f, Loops: [%s]\n") 
  			(Vilevaluator.totaloperationcost ret)
  			(Vilevaluator.weightedtimecost ret)
  			(String.concat ", " (Listutil.mapfilter (Vilanalyser.loopinfo ret) ret.vblocks))
  			 else ();
	let vret = Viltovast.vil_to_vast ret
	in let vstring = VA.vastmodule_to_verilog vret
	in  (* print verilog result *)
  		if(!printflags land 32 <> 0) then E.log("%s\n") vstring else ();
  		(* output verilog to file *)
  		writestringtofile (getfilename (functionname ret)) vstring
  		
	
(** entry point for Cynthesis, as used by cil*)
let cynthesise f = 
	(* run features that we need first 
	 * can't do this via command line since the
	 * Simplify feature runs after the makeCFG feature
	 * meaing CFG is incomplete
	 *)
	Simplify.feature.fd_doit f;
	Partial.makeCFGFeature.fd_doit f;
	Partial.makeCFGFeature.fd_enabled <- true; (* Stops the partial feature failing *)
	Partial.feature.fd_doit f;
	(* must do this again, since partial feature can screw up the CFG soemtimes *)
	Partial.makeCFGFeature.fd_doit f;
	if(!printflags land 2 <> 0) then Printers.cfgfeature.fd_doit f else ();      (** DEBUG PRINT *)
	if(!printflags land 4 <> 0) then Printers.cfglistfeature.fd_doit f else ();  (** DEBUG PRINT *)
	if(!printflags land 1 <> 0) then Printers.transfeature.fd_doit f else ();    (** DEBUG PRINT *)
	(* check that we can synthesise the marked functions
	 * Validitycheck prints out problems as it finds them *)
	if Validitycheck.check f 
	then List.iter (fun glob -> match glob with
    	| GFun(fd,_) when fd.svar.vinline -> funtomodule fd;
      			fd.svar.vinline <- false (* Temporary hack to stop gcc having a hissy fit*)
    	| _ -> ()
  	) f.globals
    else E.log("There were errors \n")

(** useful padding for the feature item *)
let padd = "                                   "

(** feature that CIL registers *)
let feature = 
  { fd_name = "cynthesis";
    fd_enabled = false;
    fd_extraopt = [("--cynthesis_print_flags",
    	Arg.Int (fun i -> 
    		Viloptimisationsteps.verbose := (i land 64 <>0); 
    		Viloptimisationsteps.domoduleprint := (i land 128 <>0); 
    		printflags := i),
    	" Print flags for the cynthesis plugin\n" ^ padd ^
		"   1 - Print code before cynthesis\n" ^ padd ^
		"   2 - Print CFG info\n" ^ padd ^
		"   4 - Print a detailed CFG list\n" ^ padd ^
		"   8 - Dump all info about resulting module\n" ^ padd ^
		"  16 - Print resulting module\n" ^ padd ^
    	"  32 - Print resulting verilog\n" ^ padd ^ 
    	"  64 - Print what the optimiser is doing\n" ^ padd ^ 
    	" 128 - Print module after each optimisation step");
    	("--cynthesis_output_dir",
    	Arg.Set_string outputdir,
    	" Set the output directory for verilog files. " ^
    	" Filenames will be the name of the synthesised functions");
    	("--cynthesis_average_loop_count",
    	Arg.Set_int Vilanalyser.averageloopcount,
    	" Set the number of iterations assumed a loop executes, " ^
    	" (if analysis can't determine this)")
    ];
    fd_description = "verilog HLS of functions marked with 'inline'";
    fd_doit = cynthesise;
    fd_post_check = false;
}

(** register feature with CIL *)
let () = Feature.register feature