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
(*
let print_env_at cxt = 
  let env = H.find envs cxt in
    p_env env std_formatter

let print_heap_at cxt =
  let sto = H.find heaps cxt in
    p_heap sto std_formatter
*)

let cin = ref stdin

let cin_name = ref "stdin"

let action_load_file path =
  cin := open_in path;
  cin_name := path

let src = 
  ref (EConst ((Lexing.dummy_pos, Lexing.dummy_pos), JavaScript_syntax.CUndefined))

let load_js () : unit = 
  let js = parse_javascript !cin !cin_name in
    src := Lambdajs_syntax.desugar (Exprjs_syntax.from_javascript js)

let load_lambdajs () : unit =
  let _ = (Lexing.dummy_pos, Lexing.dummy_pos) in
    src :=  Lambdajs.parse_lambdajs !cin !cin_name

let desugar () : unit =
  src := Lambdajs_desugar.desugar_op !src
  
(*
let verify_app node exp = match exp with
  | App (_, Id x, _) ->
      let v = lookup x (Hashtbl.find envs node) in
      let set = Type.up (Hashtbl.find heaps node) v in
        if AVSet.is_empty set then
          eprintf "Unapplied application at %d.\n" node
        else
          ()
  | If (_, Id x, _, _) ->
      let v = lookup x (Hashtbl.find envs node) in
      let set = Type.up (Hashtbl.find heaps node) v in
        if AVSet.is_empty set then
          eprintf "Branch skipped at %d.\n" node
        else
          ()
  | Bind ((n, _), x, e, cont) ->
      let bound_node = cpsexp_idx cont in
      let v = lookup x (Hashtbl.find envs bound_node) in
      let set = Type.up (Hashtbl.find heaps node) v in
        if AVSet.is_empty set then
          begin
            eprintf "let/%d %s = %s is empty\n" node x
              (FormatExt.to_string Lambdajs_cps.Pretty.p_bindexp e)
          end 
        else
          ()            
  | _ -> ()

let action_cfa () : unit =
  let lambdajs = !src in
  let cpsexp = Lambdajs_cps.cps lambdajs in
    Lambdajs_cfa.cfa cpsexp;
    cmd_loop 
      { curr_exp = cpsexp; parent = None }
*)

let action_cps () : unit =
  let cpslambdajs = Lambdajs_cps.cps !src in
    Lambdajs_cps.p_cpsexp cpslambdajs std_formatter


let action_operators () : unit =
  let ops = operators !src in
    IdSetExt.p_set text ops std_formatter;
    print_newline ()
      

let action = ref (fun () -> ())

let is_action_set = ref false

let set_action (thunk : unit -> unit) (() : unit) : unit =
  if !is_action_set then
    (eprintf "invalid arguments (-help for help)\n"; exit 1)
  else 
    (is_action_set := true; action := thunk)

let main () : unit =
  Arg.parse
    [ ("-file", Arg.String action_load_file,
       "load from a file");
      ("-js", Arg.Unit load_js,
       "Load JavaScript");
      ("-lambdajs", Arg.Unit load_lambdajs,
       "Load LambdaJS");
      ("-env", Arg.String 
         (fun s -> src := enclose_in_env (parse_env (open_in s) s) !src),
      "load environment");
      ("-full-desugar", Arg.Unit desugar, "like it says");

      ("-operators", Arg.Unit (set_action action_operators),
       "list operators used");

       ("-cps", Arg.Unit (set_action action_cps),

       "convert program to CPS");
(*      ("-cfa", Arg.Unit (set_action action_cfa),
       "(undocumented)"); *)
    ]
    (fun s -> action_load_file s)
    "Usage: jsc [action] [path]";;

main ();
Printexc.print !action ();

pp_print_flush std_formatter ();
pp_print_flush err_formatter ()
