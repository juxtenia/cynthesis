open Cil
open Feature
open Cfg 
module E = Errormsg

let print (f : file) : unit =
	List.iter (fun glob -> match glob with
      | GFun(fd,_) ->  printCfgChannel !E.logChannel fd
      | _ -> ()
  ) f.globals
;;

let feature = 
  { fd_name = "cfgprint";
    fd_enabled = false;
    fd_extraopt = [];
    fd_description = "Printing the cfg";
    fd_doit = print;
    fd_post_check = false;
}

let () = Feature.register feature