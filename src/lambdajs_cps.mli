(** The datatypes and algorithms in this module are based on Chapters 2 and 5
    of "Compiling with Continuations" by Andrew W. Appel. We depart from CWC
    in the following ways:

    - We do not use a global variable for the current exception handler.
    Instead, all functions receive an additional exception continuation.
    CWC discusses a pitfall with this technique. Note that JavaScript's 
    operators almost never signal exceptions. If a LambdaJS operator signals
    an exception, it indicates a bug in desugaring.

    - We transform all of LambdaJS's control operators to CPS. This requires a
    little more work than CPSing MiniML.

*)

open Prelude
open Lambdajs_syntax

type cpsval =
  | Const of JavaScript_syntax.const
  | Id of id

type node = int * pos

type bindexp =
  | Let of cpsval
  | Op1 of op1 * cpsval
  | Op2 of op2 * cpsval * cpsval
  | Object of (string * cpsval) list
  | UpdateField of cpsval * cpsval * cpsval

type cpsexp =
  | Fix of node * lambda list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Bind of node * id * bindexp * cpsexp

and lambda = id * id list * cpsexp

val cps : exp -> cpsexp

val p_cpsexp : cpsexp -> FormatExt.printer

val cpsexp_idx : cpsexp -> int

val lambda_name : lambda -> id

module Cps : sig

  val mk_node : pos -> node
  val num_nodes : unit -> int
end

val fv : cpsexp -> IdSet.t

val fv_immediate : cpsexp -> IdSet.t

module Pretty : sig
  open FormatExt

  val p_cpsval : cpsval -> printer
  val p_bindexp : bindexp -> printer
  val p_cpsexp : cpsexp -> printer
end
