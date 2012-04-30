open Prelude

type prefixOp =
  | PrefixLNot 
  | PrefixBNot 
  | PrefixPlus
  | PrefixMinus 
  | PrefixTypeof 
  | PrefixVoid 
  | PrefixDelete

type unaryAssignOp =
  | PrefixInc 
  | PrefixDec 
  | PostfixInc 
  | PostfixDec

type infixOp =
  | OpLT 
  | OpLEq 
  | OpGT 
  | OpGEq  
  | OpIn
  | OpInstanceof
  | OpEq
  | OpNEq
  | OpStrictEq
  | OpStrictNEq
  | OpLAnd
  | OpLOr 
  | OpMul
  | OpDiv
  | OpMod
  | OpSub
  | OpLShift
  | OpSpRShift
  | OpZfRShift
  | OpBAnd
  | OpBXor
  | OpBOr
  | OpAdd

type assignOp =
  | OpAssign
  | OpAssignAdd
  | OpAssignSub
  | OpAssignMul
  | OpAssignDiv
  | OpAssignMod
  | OpAssignLShift
  | OpAssignSpRShift
  | OpAssignZfRShift
  | OpAssignBAnd
  | OpAssignBXor
  | OpAssignBOr

type const =
  | CString of string
  | CRegexp of string * bool * bool
  | CNum of float
  | CInt of int
  | CBool of bool
  | CNull 
  | CUndefined

type prop =
  | PropId of id
  | PropString of string
  | PropNum of int

type varDecl =
  | VarDeclNoInit of Pos.t * id
  | VarDecl of Pos.t * id * expr

and forInit =
  | NoForInit
  | VarForInit of varDecl list
  | ExprForInit of expr

and catch =
  | CatchClause of Pos.t * id * stmt

and forInInit =
  | VarForInInit of Pos.t * id
  | NoVarForInInit of Pos.t * id

and caseClause =
  | CaseClause of Pos.t * expr * stmt
  | CaseDefault of Pos.t * stmt

and lvalue =
  | VarLValue of Pos.t * id
  | DotLValue of Pos.t * expr * id
  | BracketLValue of Pos.t * expr * expr

and expr =
  | ConstExpr of Pos.t * const
  | ArrayExpr of Pos.t * expr list
  | ObjectExpr of Pos.t * (Pos.t * prop * expr) list
  | ThisExpr of Pos.t
  | VarExpr of Pos.t * id
  | DotExpr of Pos.t * expr * id
  | BracketExpr of Pos.t * expr * expr
  | NewExpr of Pos.t * expr * expr list
  | PrefixExpr of Pos.t * prefixOp * expr
  | UnaryAssignExpr of Pos.t * unaryAssignOp * lvalue
  | InfixExpr of Pos.t * infixOp * expr * expr
  | IfExpr of Pos.t * expr * expr * expr
  | AssignExpr of Pos.t * assignOp * lvalue * expr
  | ParenExpr of Pos.t * expr
  | ListExpr of Pos.t * expr * expr
  | CallExpr of Pos.t * expr * expr list
  | FuncExpr of Pos.t * id list * stmt
  | NamedFuncExpr of Pos.t * id * id list * stmt

and stmt =
  | BlockStmt of Pos.t * stmt list
  | EmptyStmt of Pos.t  
  | ExprStmt of expr
  | IfStmt of Pos.t * expr * stmt * stmt
  | IfSingleStmt of Pos.t * expr * stmt
  | SwitchStmt of Pos.t * expr * caseClause list
  | WhileStmt of Pos.t * expr * stmt
  | DoWhileStmt of Pos.t * stmt * expr
  | BreakStmt of Pos.t
  | BreakToStmt of Pos.t * id
  | ContinueStmt of Pos.t
  | ContinueToStmt of Pos.t * id
  | LabelledStmt of Pos.t * id * stmt
  | ForInStmt of Pos.t * forInInit * expr * stmt
  | ForStmt of Pos.t * forInit * expr * expr * stmt
  | TryStmt of Pos.t * stmt * catch list * stmt
  | ThrowStmt of Pos.t * expr
  | ReturnStmt of Pos.t * expr
  | WithStmt of Pos.t * expr * stmt
  | VarDeclStmt of Pos.t * varDecl list
  | FuncStmt of Pos.t * id * id list * stmt

type prog =
  | Prog of Pos.t * stmt list


let pos_expr e = match e with
  | ConstExpr (p, _)
  | ArrayExpr (p, _)
  | ObjectExpr (p, _)
  | ThisExpr p
  | VarExpr (p, _)
  | DotExpr (p, _, _)
  | BracketExpr (p, _, _)
  | NewExpr (p, _, _)
  | PrefixExpr (p, _, _)
  | UnaryAssignExpr (p, _, _)
  | InfixExpr (p, _, _, _)
  | IfExpr (p, _, _, _)
  | AssignExpr (p, _, _, _)
  | ParenExpr (p, _)
  | ListExpr (p, _, _)
  | CallExpr (p, _, _)
  | FuncExpr (p, _, _)
  | NamedFuncExpr (p, _, _, _) -> p

let pos_stmt s = match s with
  | ExprStmt e -> pos_expr e
  | BlockStmt (p, _)
  | EmptyStmt p  
  | IfStmt (p, _, _, _)
  | IfSingleStmt (p, _, _)
  | SwitchStmt (p, _, _)
  | WhileStmt (p, _, _)
  | DoWhileStmt (p, _, _)
  | BreakStmt p
  | BreakToStmt (p, _)
  | ContinueStmt p
  | ContinueToStmt (p, _)
  | LabelledStmt (p, _, _)
  | ForInStmt (p, _, _, _)
  | ForStmt (p, _, _, _, _)
  | TryStmt (p, _, _, _)
  | ThrowStmt (p, _)
  | ReturnStmt (p, _)
  | WithStmt (p, _, _)
  | VarDeclStmt (p, _)
  | FuncStmt (p, _, _, _) -> p
