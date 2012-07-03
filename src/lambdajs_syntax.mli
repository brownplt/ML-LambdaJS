open Prelude
open FormatExt

type op1 = 
  | Op1Prefix of id
  | Deref
  | Ref
  | Prim1 of string

(* TODO: unchecked operations should always use differnet syntax. add an
   uncheckedGetField, uncheckedSetField, updateField, App, and if, ? *)
type op2 =
  | Op2Infix of id
  | Prim2 of string
  | GetField
  | UnsafeGetField
  | DeleteField
  | SetRef

(** NOTE: reference and object manipulation are defined using [EOp1] and 
    [EOp2]. This design shrinks the size of many cases. *)
type exp =
  | EConst of Pos.t * JavaScript_syntax.const
  | EId of Pos.t * id
  | EObject of Pos.t * (Pos.t * string * exp) list
  | EUpdateField of Pos.t * exp * exp * exp
  | EOp1 of Pos.t * op1 * exp
  | EOp2 of Pos.t * op2 * exp * exp
  | EIf of Pos.t * exp * exp * exp
  | EApp of Pos.t * exp * exp list
  | ESeq of Pos.t * exp * exp
  | ELet of Pos.t * id * exp * exp
  | EFix of Pos.t * (id * exp) list * exp 
      (** All bindings must be [ELambda]s. *)
  | ELabel of Pos.t * id * exp
  | EBreak of Pos.t * id * exp
  | ETryCatch of Pos.t * exp * exp
      (** Catch block must be an [ELambda] *)
  | ETryFinally of Pos.t * exp * exp
  | EThrow of Pos.t * exp
  | ELambda of Pos.t * id list * exp

val desugar : Exprjs_syntax.expr -> exp

module Pretty : sig
  val p_op1 : op1 -> printer
  val p_op2 : op2 -> printer
  val p_exp : exp -> printer
end 

val fv : exp -> IdSet.t
val rename : id -> id -> exp -> exp
val operators : exp -> IdSet.t
