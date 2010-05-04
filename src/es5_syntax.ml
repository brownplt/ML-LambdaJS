open Prelude

type op1 = 
  | Op1Prefix of id
  | Prim1 of string

type op2 = 
  | Op2Infix of id
  | Prim2 of string

type exp =
  | EConst of pos * JavaScript_syntax.const
  | EId of pos * id
  | EObject of pos * (string * exp) list *
	       (pos * string * (string * exp) list) list
  | EUpdateField of pos * exp * exp * exp * exp
  | EGetField of pos * exp * exp * exp
  | EDeleteField of pos * exp * exp
  | ESetRef of pos * id * exp
  | EOp1 of pos * op1 * exp
  | EOp2 of pos * op2 * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
  | ESeq of pos * exp * exp
  | ELet of pos * id * exp * exp
  | ELetAlloc of pos * id * exp * exp
  | EFix of pos * id * exp
  | ELabel of pos * id * exp
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * exp
      (** Catch block must be an [ELambda] *)
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ELambda of pos * id list * exp

(******************************************************************************)
