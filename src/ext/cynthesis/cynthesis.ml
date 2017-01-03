open Cil
open Feature
module V = Vil
module E = Errormsg

let cynthesize f = 
	if not (Feature.enabled "makeCFG") then
    Errormsg.s (Errormsg.error
                  "--docynthesis: you must also specify --domakeCFG\n");
    if not (Feature.enabled "simplify") then
    Errormsg.s (Errormsg.error
                  "--docynthesis: you must also specify --dosimplify\n");
    if not (Feature.enabled "partial") then
    Errormsg.s (Errormsg.error
                  "--docynthesis: you must also specify --dopartial\n");
	if Validitycheck.check f 
	then List.iter (fun glob -> match glob with
      | GFun(fd,_) when fd.svar.vinline ->  E.log("%s\n") (V.string_of_funmodule (V.funtofunmodule fd));
      	fd.svar.vinline <- false; (* Temporary hack to stop gcc having a hissy fit*)
      | _ -> ()
  	) f.globals
	else E.log("There were errors \n")

let feature = 
  { fd_name = "cynthesis";
    fd_enabled = false;
    fd_extraopt = [];
    fd_description = "verilog HLS of functions marked with 'inline'";
    fd_doit = cynthesize;
    fd_post_check = false;
}

let () = Feature.register feature