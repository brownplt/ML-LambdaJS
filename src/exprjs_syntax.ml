open Prelude

type expr
  = ConstExpr of pos * JavaScript_syntax.const
  | ArrayExpr of pos * expr list
  | ObjectExpr of pos * (pos * string * expr) list
  | ThisExpr of pos
  | VarExpr of pos * id
  | IdExpr of pos * id
  | BracketExpr of pos * expr * expr
  | NewExpr of pos * expr * expr list
  | PrefixExpr of pos * id * expr
  | InfixExpr of pos * id * expr * expr
  | IfExpr of pos * expr * expr * expr
  | AssignExpr of pos * lvalue * expr
  | AppExpr of pos * expr * expr list
  | FuncExpr of pos * id list * expr
  | LetExpr of pos * id * expr * expr
  | SeqExpr of pos * expr * expr
  | WhileExpr of pos * expr * expr
  | DoWhileExpr of pos * expr * expr
  | LabelledExpr of pos * id * expr
  | BreakExpr of pos * id * expr
  | ForInExpr of pos * id * expr * expr
  | VarDeclExpr of pos * id * expr
  | TryCatchExpr of pos * expr * id * expr
  | TryFinallyExpr of pos * expr * expr
  | ThrowExpr of pos * expr
  | FuncStmtExpr of pos * id * id list * expr
  | HintExpr of pos * string * expr

and lvalue =
    VarLValue of pos * id
  | PropLValue of pos * expr * expr

(******************************************************************************)

module S = JavaScript_syntax

let infix_of_assignOp op = match op with
  | S.OpAssignAdd -> S.OpAdd
  | S.OpAssignSub -> S.OpSub
  | S.OpAssignMul -> S.OpMul
  | S.OpAssignDiv -> S.OpDiv
  | S.OpAssignMod -> S.OpMod
  | S.OpAssignLShift -> S.OpLShift
  | S.OpAssignSpRShift -> S.OpSpRShift
  | S.OpAssignZfRShift -> S.OpZfRShift
  | S.OpAssignBAnd -> S.OpBAnd
  | S.OpAssignBXor -> S.OpBXor
  | S.OpAssignBOr -> S.OpBOr
  | S.OpAssign -> failwith "infix_of_assignOp applied to OpAssign"

let string_of_infixOp = FormatExt.to_string JavaScript.Pretty.p_infixOp

let rec seq a e1 e2 = match e1 with
  | SeqExpr (a', e11, e12) -> SeqExpr (a, e11, seq a' e12 e2)
  | _ -> SeqExpr (a, e1, e2)

let rec expr (e : S.expr) = match e with
  | S.ConstExpr (p, c) -> ConstExpr (p, c)
  | S.ArrayExpr (a,es) -> ArrayExpr (a,map expr es)
  | S.ObjectExpr (a,ps) -> ObjectExpr (a,map prop ps)
  | S.ThisExpr a -> ThisExpr a
  | S.VarExpr (a,x) -> VarExpr (a,x)
  | S.DotExpr (a,e,x) -> BracketExpr (a, expr e, ConstExpr (a, S.CString x))
  | S.BracketExpr (a,e1,e2) -> BracketExpr (a,expr e1,expr e2)
  | S.NewExpr (a,e,es) -> NewExpr (a,expr e,map expr es)
  | S.PrefixExpr (a,op,e) -> 
      PrefixExpr (a, "prefix:" ^ FormatExt.to_string JavaScript.Pretty.p_prefixOp op,expr e)
  | S.UnaryAssignExpr (a, op, lv) ->
      let func (lv, e) = 
        match op with
             S.PrefixInc ->
               seq a
                 (AssignExpr 
                    (a, lv, InfixExpr (a, "+", e, ConstExpr (a, S.CInt 1))))
                 e
           | S.PrefixDec ->
               seq
                 a
                 (AssignExpr 
                    (a, lv, InfixExpr (a, "-", e, ConstExpr (a, S.CInt 1))))
                  e
           | S.PostfixInc ->
               seq a
                 (AssignExpr 
                    (a, lv, InfixExpr (a, "+", e, ConstExpr (a, S.CInt 1))))
                 (InfixExpr (a, "-", e, ConstExpr (a, S.CInt 1)))
           | S.PostfixDec ->
               seq a
                 (AssignExpr 
                    (a, lv, InfixExpr (a, "-", e, ConstExpr (a, S.CInt 1))))
                 (InfixExpr (a, "+", e, ConstExpr (a, S.CInt 1)))
      in eval_lvalue lv func
  | S.InfixExpr (a,op,e1,e2) -> 
      InfixExpr (a, string_of_infixOp op,expr e1,expr e2)
  | S.IfExpr (a,e1,e2,e3) -> IfExpr (a,expr e1,expr e2,expr e3)
  | S.AssignExpr (a,S.OpAssign,lv,e) -> AssignExpr (a,lvalue lv,expr e)
  | S.AssignExpr (a,op,lv,e) -> 
      let body_fn (lv,lv_e) = 
        AssignExpr 
          (a, lv,
           InfixExpr (a,string_of_infixOp (infix_of_assignOp op),lv_e,expr e))
      in eval_lvalue lv body_fn
  | S.ParenExpr (_, e) -> expr e
  | S.ListExpr (a,e1,e2) -> seq a (expr e1) (expr e2)
  | S.CallExpr (a,func,args) -> AppExpr (a,expr func,map expr args)
  | S.FuncExpr (a, args, body) ->
      FuncExpr (a, args, LabelledExpr (a, "%return", stmt body))
  | S.HintExpr (p, text, e) -> HintExpr (p, text, expr e)      
  | S.NamedFuncExpr (a, name, args, body) ->
      (* INFO: This translation is absurd and makes typing impossible.
         Introduce FIX and eliminate loops in the process. Note that the
         parser does not produce NamedFuncExprs, so for the moment, this is
         inconsequential. *)
      let anonymous_func = 
        FuncExpr (a,args,LabelledExpr (a,"%return",stmt body)) in
      LetExpr (a,name, ConstExpr (a, S.CUndefined),
               seq a
                 (AssignExpr (a,
                              VarLValue (a,name),anonymous_func))
                 (IdExpr (a,name)))
                        
and lvalue (lv : S.lvalue) = match lv with
    S.VarLValue (a,x) -> VarLValue (a,x)
  | S.DotLValue (a,e,x) -> PropLValue (a, expr e, ConstExpr (a, S.CString x))
  | S.BracketLValue (a,e1,e2) -> PropLValue (a,expr e1,expr e2)

and stmt (s : S.stmt) = match s with 
    S.BlockStmt (a,[]) -> ConstExpr (a, S.CUndefined)
  | S.BlockStmt (a,s1::ss) -> seq a (stmt s1) (stmt (S.BlockStmt (a, ss)))
  | S.EmptyStmt a -> ConstExpr (a, S.CUndefined)
  | S.IfStmt (a,e,s1,s2) -> IfExpr (a,expr e,stmt s1,stmt s2)
  | S.IfSingleStmt (a,e,s) -> 
      IfExpr (a,expr e,stmt s, ConstExpr (a, S.CUndefined))
  | S.SwitchStmt (p,e,clauses) ->
      LabelledExpr
        (p, "%break",
         LetExpr (p, "%v", expr e,
                  LetExpr (p, "%t",
                           ConstExpr (p, S.CBool false),
                           caseClauses p clauses)))
  | S.LabelledStmt (p1, lbl ,S.WhileStmt (p2, test, body)) -> LabelledExpr 
        (p1, "%break", LabelledExpr
           (p1,lbl,WhileExpr
              (p2,expr test,LabelledExpr 
                 (p2,"%continue",LabelledExpr
                    (p1,"%continue-"^lbl,stmt body)))))
                             
                                             
  | S.WhileStmt (p,test,body) -> LabelledExpr
        (p,"%break",WhileExpr 
           (p,expr test,LabelledExpr 
              (p,"%continue",stmt body)))
  | S.LabelledStmt (p1, lbl ,S.DoWhileStmt (p2, body, test)) -> LabelledExpr 
        (p1, "%break", LabelledExpr
           (p1,lbl,DoWhileExpr
              (p2, LabelledExpr 
                 (p1,"%continue",LabelledExpr
                    (p2,"%continue-"^lbl,stmt body)),
              expr test)))
  | S.DoWhileStmt (p, body, test) -> LabelledExpr
      (p, "%break", DoWhileExpr 
           (p, LabelledExpr (p, "%continue", stmt body),
            expr test))
  | S.BreakStmt a -> BreakExpr (a,"%break", ConstExpr (a, S.CUndefined))
  | S.BreakToStmt (a,lbl) -> BreakExpr (a,lbl, ConstExpr (a, S.CUndefined))
  | S.ContinueStmt a -> BreakExpr (a,"%continue", ConstExpr (a, S.CUndefined))
  | S.ContinueToStmt (a,lbl) -> 
      BreakExpr (a,"%continue-"^lbl, ConstExpr (a, S.CUndefined))
  | S.FuncStmt (a, f, args, s) -> 
      FuncStmtExpr 
        (a, f, args, LabelledExpr 
           (a, "%return", stmt s))
  | S.ExprStmt e -> expr e
  | S.ThrowStmt (a, e) -> ThrowExpr (a, expr e)
  | S.ReturnStmt (a, e) -> BreakExpr (a, "%return", expr e)
  | S.WithStmt _ -> failwith "we do not account for with statements"
  | S.TryStmt (a, body, catches, finally) ->
      let f body (S.CatchClause (a, x, s)) = TryCatchExpr (a, body, x, stmt s)
      in TryFinallyExpr (a, fold_left f (stmt body) catches, stmt finally)
  | S.ForStmt (a, init, stop, incr, body) ->
      seq a
        (forInit a init)
        (LabelledExpr 
           (a, "%break",
            WhileExpr 
              (a, expr stop, 
               seq a
                 (LabelledExpr (a, "%continue", stmt body))
                 (expr incr))))
  | S.ForInStmt (p, init, e, body) ->
      let (x, init_e) = forInInit init in
        SeqExpr 
          (p, init_e,
           LabelledExpr
             (p, "%break",
              ForInExpr 
                (p, x, expr e,
                 LabelledExpr
                   (p, "%continue", stmt body))))
  | S.VarDeclStmt (a, decls) -> varDeclList a decls
  | S.LabelledStmt (p, lbl, s) ->
      LabelledExpr (p, lbl, stmt s)
  | S.HintStmt (p, txt, s) -> HintExpr (p, txt, stmt s)

and forInit p (fi : S.forInit) = match fi with
    S.NoForInit -> ConstExpr (p, S.CUndefined)
  | S.ExprForInit e -> expr e
  | S.VarForInit decls -> varDeclList p decls

and forInInit fii = match fii with
  | S.VarForInInit (p, x) ->
      (x, VarDeclExpr (p, x, ConstExpr (p, S.CUndefined)))
  | S.NoVarForInInit (p, x) -> (x, ConstExpr (p, S.CUndefined))

and varDeclList p decls = match decls with
  | [] -> ConstExpr (p, S.CUndefined)
  | [d] -> varDecl p d
  | d :: ds -> seq p (varDecl p d) (varDeclList p ds)

and varDecl p (decl : S.varDecl) = match decl with
    S.VarDeclNoInit (a, x) -> VarDeclExpr (a, x, ConstExpr (p, S.CUndefined))
  | S.VarDecl (a, x, e) -> VarDeclExpr (a, x, expr e)

and collectClauseExprs exprs clauses = match clauses with
  | S.CaseClause (p, e, s) :: rest -> begin match stmt s with
      | ConstExpr (_, S.CUndefined) ->
        collectClauseExprs (expr e :: exprs) rest
      | s' -> (p, expr e :: exprs, s', rest)
  end
  | _ -> failwith "collectClauseExprs expected non-empty list"

and caseClauses p (clauses : S.caseClause list) = match clauses with
    [] -> ConstExpr (p, S.CUndefined)
  | (S.CaseDefault (a,s)::clauses) -> seq a (stmt s) (caseClauses p clauses)
  | clauses ->
      let (p, es, body, rest) = collectClauseExprs [] clauses in
      let f e acc =
        LetExpr (p, "%t", IfExpr (p, IdExpr (p, "%t"),
                                  IdExpr (p, "%t"),
                                  InfixExpr (p, "===", IdExpr (p, "%v"), e)),
                 acc) in
      fold_right f (List.rev es) 
        (SeqExpr 
           (p,
            IfExpr (p, IdExpr (p,"%t"),
                    body,
                    ConstExpr (p, S.CUndefined)),
            caseClauses p rest))

and prop pr =  match pr with
    (p, S.PropId x,e) -> (p, x, expr e)
  | (p, S.PropString s,e) -> (p, s, expr e)
  | (p, S.PropNum n,e) -> (p, string_of_int n, expr e)

(** Generates an expression that evaluates and binds lv, then produces the
    the value of body_fn.  body_fn is applied with references to lv as an
    lvalue and lv as an expression. *)
and eval_lvalue (lv :  S.lvalue) (body_fn : lvalue * expr -> expr) =
  match lv with
    | S.VarLValue (p, x) -> body_fn (VarLValue (p, x), VarExpr (p, x))
  | S.DotLValue (a, e, x) -> 
      LetExpr (a,"%lhs",expr e,
        body_fn
          (PropLValue (a, IdExpr (a,"%lhs"), ConstExpr (a, S.CString x)),
           BracketExpr (a, IdExpr (a,"%lhs"), ConstExpr (a, S.CString x))))
  | S.BracketLValue (a,e1,e2) -> 
      LetExpr (a,"%lhs",expr e1,
      LetExpr (a,"%field",expr e2,
      body_fn (PropLValue (a, IdExpr (a,"%lhs"), IdExpr (a,"%field")),
               BracketExpr (a, IdExpr (a,"%lhs"), IdExpr (a,"%field")))))

let from_javascript (S.Prog (p, stmts)) = 
  let f s e = seq p (stmt s) e
  in fold_right f stmts (ConstExpr (p, S.CUndefined))

let from_javascript_expr = expr

(******************************************************************************)

let rec locals expr = match expr with
  | ConstExpr _ -> IdSet.empty
  | ArrayExpr (_, es) -> IdSetExt.unions (map locals es)
  | ObjectExpr (_, ps) -> IdSetExt.unions (map (fun (_, _, e) -> locals e) ps)
  | ThisExpr _ -> IdSet.empty
  | VarExpr _ -> IdSet.empty
  | IdExpr _ -> IdSet.empty
  | BracketExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | NewExpr (_, c, args) -> IdSetExt.unions (map locals (c :: args))
  | PrefixExpr (_, _, e) -> locals e
  | InfixExpr (_, _, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | IfExpr (_, e1, e2, e3) -> IdSetExt.unions (map locals [e1; e2; e3])
  | AssignExpr (_, l, e) -> IdSet.union (lv_locals l) (locals e)
  | AppExpr (_, f, args) -> IdSetExt.unions (map locals (f :: args))
  | FuncExpr _ -> IdSet.empty
  | LetExpr (_, _, e1, e2) -> 
      (* We are computing properties of the local scope object, not identifers
         introduced by the expression transformation. *)
      IdSet.union (locals e1) (locals e2)
  | SeqExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | WhileExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | DoWhileExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | LabelledExpr (_, _, e) -> locals e
  | BreakExpr (_, _, e) -> locals e
  | ForInExpr (_, _, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | VarDeclExpr (_, x, e) -> IdSet.add x (locals e)
  | TryCatchExpr (_, e1, _, e2) ->
      (* TODO: figure out how to handle catch-bound identifiers *)
      IdSet.union (locals e1) (locals e2)
  | TryFinallyExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | ThrowExpr (_, e) -> locals e
  | FuncStmtExpr (_, f, _, _) -> IdSet.singleton f
  | HintExpr (_, _, e) -> locals e

and lv_locals lvalue = match lvalue with
    VarLValue _ -> IdSet.empty
  | PropLValue (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
