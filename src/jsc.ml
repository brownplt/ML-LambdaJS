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
open Lexing


module H = Hashtbl

let p = (Lexing.dummy_pos, Lexing.dummy_pos)

let src =  ref (EConst (p, JavaScript_syntax.CUndefined))

let load_js (path : string) : unit = 
  let js = parse_javascript_from_channel (open_in path) path in
    src := 
      ESeq (p, !src,
            Lambdajs_syntax.desugar (Exprjs_syntax.from_javascript js))

let load_lambdajs (path : string) : unit =
  src := ESeq (p, !src,
               Lambdajs.parse_lambdajs (open_in path) path)

let load_file (path : string) : unit =
  if Filename.check_suffix path ".js" then
    load_js path
  else if Filename.check_suffix path ".jsl" then
    load_lambdajs path
  else 
    failwith ("unknown file extention; try -js or -jsl")

let desugar () : unit =
  src := Lambdajs_desugar.desugar_op !src
  
let action_cps () : unit =
  let cpslambdajs = Lambdajs_cps.cps !src in
    Lambdajs_cps.p_cpsexp cpslambdajs std_formatter


let action_operators () : unit =
  let ops = operators !src in
    IdSetExt.p_set text ops std_formatter;
    print_newline ()
      

let action = ref (fun () -> ())

let set_action (thunk : unit -> unit) (() : unit) : unit =
  let prev = !action in
  action :=  fun () -> prev (); thunk ()

let main () : unit =
  Arg.parse
    [ ("-js", Arg.String load_js,
       "<file> Load <file> as JavaScript");
      ("-jsl", Arg.String load_lambdajs,
       "Load <file> as LambdaJS");
      ("-env", Arg.String 
         (fun s -> src := enclose_in_env (parse_env (open_in s) s) !src),
      "<file> load <file> as environment");
      ("-full-desugar", Arg.Unit (set_action desugar), "like it says");

      ("-operators", Arg.Unit (set_action action_operators),
       "list operators used");

       ("-cps", Arg.Unit (set_action action_cps),

       "convert program to CPS");
    ]
    load_file
    "Usage: jsc <action> [path] ...";;

main ();
Printexc.print !action ();

pp_print_flush std_formatter ();
pp_print_flush err_formatter ()
