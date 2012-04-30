(** A simplified syntax for JavaScript. The primary simplification is
    that statements are eliminated. Expr{_ JS} is an expression-based syntax.

    We map JavaScript's statements to Expr{_ JS}'s control operators. Some
    statements map trivially to Expr{_ JS}. Others, such as switch and return,
    require less-obvious transformations. See the implementation for details.

    Expr{_ JS} has let-bindings [LetExpr]. We use let-bindings for some
    transformations. However, we do not transform [VarDeclStmt]s into
    let-bindings at this stage. Therefore, Expr{_ JS} uses both lexical scope
    and scope objects. *)
open Prelude

type expr
  = ConstExpr of Pos.t * JavaScript_syntax.const
  | ArrayExpr of Pos.t * expr list
  | ObjectExpr of Pos.t * (Pos.t * string * expr) list
      (** Object properties are transformed into string literals *)
  | ThisExpr of Pos.t
  | VarExpr of Pos.t * id (** identifiers bound in scope objects *)
  | IdExpr of Pos.t * id (** let-bound identifiers *)
  | BracketExpr of Pos.t * expr * expr
  | NewExpr of Pos.t * expr * expr list
  | PrefixExpr of Pos.t * id * expr
  | InfixExpr of Pos.t * id * expr * expr
  | IfExpr of Pos.t * expr * expr * expr
  | AssignExpr of Pos.t * lvalue * expr
  | AppExpr of Pos.t * expr * expr list
  | FuncExpr of Pos.t * id list * expr
  | LetExpr of Pos.t * id * expr * expr 
      (** We need let-expressions to simplify statements. *)
  | SeqExpr of Pos.t * expr * expr
  | WhileExpr of Pos.t * id list * expr * expr (* break label(s), cond, body *)
  | DoWhileExpr of Pos.t * id list * expr * expr (* break label(s), cond, body *)
  | LabelledExpr of Pos.t * id * expr
  | BreakExpr of Pos.t * id * expr
  | ForInExpr of Pos.t * id list * id * expr * expr (* break label(s), var, obj, body *)
  | VarDeclExpr of Pos.t * id * expr
      (** We do not transform VarDeclStmts to let-bindings at this stage *)
  | TryCatchExpr of Pos.t * expr * id * expr
  | TryFinallyExpr of Pos.t * expr * expr
  | ThrowExpr of Pos.t * expr
  | FuncStmtExpr of Pos.t * id * id list * expr
      (** We leave function statements in place, so that they can be lifted
          for JavaScript to turned into letrecs for Typed JavaScript. *)
  | ParenExpr of Pos.t * expr
  | BotExpr of Pos.t

and lvalue =
    VarLValue of Pos.t * id
  | PropLValue of Pos.t * expr * expr

val pos : expr -> Pos.t

val from_javascript : JavaScript_syntax.prog -> expr

val from_javascript_expr : JavaScript_syntax.expr -> expr

(** locally defined functions. *)
val locals : expr -> IdSet.t
