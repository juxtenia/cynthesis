open Cil
open Feature
module VI = Vil
module VA = Vast
module E = Errormsg

let printflags = ref 48;;
let outputdir = ref ".";;

let getfilename (funname:string) = 
	let dirname = !outputdir
	in let dirsep = Filename.dir_sep
	in let dirlen = String.length dirname
	in let seplen = String.length dirsep
	in let suff = 
		if dirlen >= seplen && (String.compare dirsep 
			(String.sub dirname ((String.length dirname) - seplen) seplen)) = 0
		then "" else dirsep
    in dirname ^ suff ^ funname ^ ".sv"

let writestringtofile (file:string) (value:string) = 
	let oc = open_out file in    (* create or truncate file, return channel *)
  	Printf.fprintf oc "%s\n" value;   (* write something *)   
  	close_out oc

let funtomodule (f:fundec) = 
	let ret = Ciltovil.generatefunmodule f 
	in  (* schedule *)
		Vil.generatescheduleinfo ret;
		(* dump module info *)
		if(!printflags land 8 <> 0) then E.log("%s\n") (VI.string_of_funmodule ret) else ();
		(* print more readable module printout *)
  		if(!printflags land 16 <> 0) then E.log("%s\n") (VI.print_funmodule ret) else ();
	let vret = Viltovast.vil_to_vast ret
	in let vstring = VA.vastmodule_to_verilog vret
	in  (* print verilog result *)
  		if(!printflags land 32 <> 0) then E.log("%s\n") vstring else ();
  		(* output verilog to file *)
  		writestringtofile (getfilename (VI.functionname ret)) vstring
  		
	

let cynthesise f = 
	Simplify.feature.fd_doit f;
	Partial.makeCFGFeature.fd_doit f;
	Partial.makeCFGFeature.fd_enabled <- true; (* Stops the partial feature failing *)
	Partial.feature.fd_doit f;
	Partial.makeCFGFeature.fd_doit f;
	if(!printflags land 2 <> 0) then Printers.cfgfeature.fd_doit f else ();      (** DEBUG PRINT *)
	if(!printflags land 4 <> 0) then Printers.cfglistfeature.fd_doit f else ();  (** DEBUG PRINT *)
	if(!printflags land 1 <> 0) then Printers.transfeature.fd_doit f else ();    (** DEBUG PRINT *)
	if Validitycheck.check f 
	then List.iter (fun glob -> match glob with
    	| GFun(fd,_) when fd.svar.vinline -> funtomodule fd;
      			fd.svar.vinline <- false (* Temporary hack to stop gcc having a hissy fit*)
    	| _ -> ()
  	) f.globals
    else E.log("There were errors \n")

let feature = 
  { fd_name = "cynthesis";
    fd_enabled = false;
    fd_extraopt = [("--cynthesis_print_flags",
    	Arg.Set_int printflags,
    	" print flags for the cynthesis plugin\n" ^
		"   1 - Print code before cynthesis\n" ^ 
		"   2 - Print CFG info\n" ^
		"   4 - Print a detailed CFG list\n" ^ 
		"   8 - Dump all info about resulting module\n" ^
		"  16 - Print resulting module\n" ^
    	"  32 - Print resulting verilog\n");
    	("--cynthesis_output_dir",
    	Arg.Set_string outputdir,
    	" the output directory for verilog files\n" ^
    	" filenames will be the name of the cynthesised functions")
    ];
    fd_description = "verilog HLS of functions marked with 'inline'";
    fd_doit = cynthesise;
    fd_post_check = false;
}

let () = Feature.register feature