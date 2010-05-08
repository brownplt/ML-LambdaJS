open JavaScript
open Exprjs
open Prelude
open Printf
open Format
open Lambdajs_cps
open Exprjs_syntax
open Format
open FormatExt
open Lambdajs_lattice
open Lambdajs_env
open Lambdajs_syntax
open Lambdajs_eval
open Lexing
module ES5 = Es5
module ES5s = Es5_syntax
module ES5ds = Es5_desugar
module ES5e = Es5_env
module ES5pp = Es5_pretty
module ES5p = Es5_parser
module ES5eval = Es5_eval

module H = Hashtbl

let p = (Lexing.dummy_pos, Lexing.dummy_pos)

let srcLJS = ref (EConst (p, JavaScript_syntax.CUndefined))
let srcES5 = ref (ES5s.EConst (p, JavaScript_syntax.CUndefined))
let lang = ref "es5"

let action_set_lang (lang_in : string) : unit = 
  match lang_in with
    | "ljs" -> lang := "ljs"
    | "es5" -> lang := "es5"
    | _ -> failwith ("unknown language: " ^ lang_in)
      

let load_js (path : string) : unit = 
  let js = parse_javascript_from_channel (open_in path) path in
    match !lang with
      |	"ljs" ->
	  srcLJS := 
	    ESeq (p, !srcLJS,
		  Lambdajs_syntax.desugar (Exprjs_syntax.from_javascript js))
      | "es5" ->
	  srcES5 := ES5ds.ds_top (Exprjs_syntax.from_javascript js)
      | _ -> failwith ("Unknown language: " ^ !lang)

let load_lambdajs (path : string) : unit =
  srcLJS := ESeq (p, !srcLJS,
		  Lambdajs.parse_lambdajs (open_in path) path)
    
let load_es5 (path : string) : unit =
  srcES5 := ES5s.ESeq (p, !srcES5,
		       ES5.parse_es5 (open_in path) path)

let load_file (path : string) : unit =
  if Filename.check_suffix path ".js" then
    load_js path
  else if Filename.check_suffix path ".jsl" then
    load_lambdajs path
  else if Filename.check_suffix path ".es5" then
    load_es5 path
  else 
    failwith ("unknown file extention; try -js or -jsl")

let desugar () : unit =
  match !lang with
    | "ljs" -> srcLJS := Lambdajs_desugar.desugar_op !srcLJS
    | "es5" -> srcES5 := ES5ds.ds_op !srcES5
    | _ -> failwith ("Unknown language: " ^ !lang)

let set_env (s : string) =
  match !lang with
    | "ljs" -> srcLJS := enclose_in_env (parse_env (open_in s) s) !srcLJS
    | "es5" -> srcES5 := ES5e.enclose_in_env (ES5e.parse_env (open_in s) s) !srcES5
    | _ -> failwith ("Unknown language: " ^ !lang)

let action_pretty () : unit =
  match !lang with
    | "ljs" -> failwith ("Pretty not implemented for " ^ !lang)
    | "es5" -> Es5_pretty.exp !srcES5 std_formatter;
	print_newline ()
    | _ -> failwith ("Unknown language: " ^ !lang)

let action_cps () : unit =
  let cpslambdajs = Lambdajs_cps.cps !srcLJS in
    Lambdajs_cps.p_cpsexp cpslambdajs std_formatter

let action_eval () : unit =
  match !lang with
    | "es5" -> ES5eval.eval_expr !srcES5; print_newline ()
    | _ -> failwith ("Not implemented for language: " ^ !lang)

let action_operators () : unit =
  let ops = operators !srcLJS in
    IdSetExt.p_set text ops std_formatter;
    print_newline ()
      
let action = ref (fun () -> ())

let set_action (thunk : unit -> unit) (() : unit) : unit =
  let prev = !action in
  action :=  fun () -> prev (); thunk ()

let main () : unit =
  Arg.parse
    [ 
      ("-lang", Arg.String action_set_lang,
       "(ljs|es5) select the language to use.  Not all operations work with
all languages");

      ("-js", Arg.String load_js,
       "<file> Load <file> as JavaScript");

      ("-jsl", Arg.String load_lambdajs,
       "Load <file> as LambdaJS");

      ("-env", Arg.String set_env,
      "<file> load <file> as environment");

      ("-full-desugar", Arg.Unit (set_action desugar), "like it says");

      ("-operators", Arg.Unit (set_action action_operators),
       "list operators used");

       ("-cps", Arg.Unit (set_action action_cps),
       "convert program to CPS");

       ("-eval", Arg.Unit (set_action action_eval),
	"run the program");

       ("-pretty", Arg.Unit (set_action action_pretty),
	"Pretty print the current source")
    ]
    load_file
    "Usage: jsc <action> [path] ...";;

main ();
Printexc.print !action ();

pp_print_flush std_formatter ();
pp_print_flush err_formatter ()
