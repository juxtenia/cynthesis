open Cil
open Feature
module E = Errormsg

let print (f : file) : unit =
	lineDirectiveStyle := None ;
	printCilAsIs := true ;
	List.iter (fun g -> E.log "%a\n" d_global g)  f.globals
;;

let feature = 
  { fd_name = "transprint";
    fd_enabled = false;
    fd_extraopt = [];
    fd_description = "Prints the transformed code to stdout";
    fd_doit = print;
    fd_post_check = false;
}

let () = Feature.register feature