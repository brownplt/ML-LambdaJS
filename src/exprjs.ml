open Prelude
open Exprjs_syntax

let from_javascript = Exprjs_syntax.from_javascript

module Pretty = struct

  open Format
  open FormatExt

  let rec expr e = match e with
    | ConstExpr (_, c) -> JavaScript.Pretty.p_const c
    | ArrayExpr (_, es) -> parens (horz (map expr es))
    | ObjectExpr (_, ps) -> brackets (vert (map prop ps))
    | ThisExpr _ -> text "#this"
    | VarExpr (_, x) -> text x
    | BracketExpr (_, e1, e2) -> squish [ expr e1; brackets (expr e2) ]
    | NewExpr (_, c, args) -> 
        parens (horz (text "new" :: expr c :: map expr args))
    | IfExpr (_, e1, e2, e3) ->
        parens (vert [ horz [ text "if"; expr e1 ]; expr e2; expr e3 ])
    | AppExpr (_, f, args) -> parens (horz (expr f :: map expr args))
    | FuncExpr (_, args, body) ->
        parens (vert [ text "fun"; parens (horz (map text args)); expr body ])
    | LetExpr (_, x, e1, e2) ->
        parens (vert [ horz [ text "let"; text x; text "="; expr e1; 
                              text "in" ];
                       expr e2 ])
    | SeqExpr (_, e1, e2) -> parens (vert [ text "seq"; expr e1; expr e2 ])
    | VarDeclExpr (_, x, e) ->  horz [ text "var"; text x; text "="; expr e ]
    | WhileExpr (_, e1, e2) -> parens (vert [ text "while"; expr e1; expr e2 ])
    | DoWhileExpr (_, e1, e2) ->
        parens (vert [ text "do-while"; expr e1; expr e2 ])
    | LabelledExpr (_, x, e) ->
        parens (vert [ text "label"; text x; expr e ])
    | BreakExpr (_, x, e) ->
        parens (vert [ text "break"; text x; expr e ])
    | TryCatchExpr (_, body, x, catch) ->
        parens (vert [ text "try"; expr body; 
                       parens (vert [text "catch"; text x; expr body ]) ])
    | TryFinallyExpr (_, body, finally) ->
        parens (vert [ text "try"; expr body; 
                       parens (vert [ text "finally"; expr finally ])])
    | ForInExpr (_, x, obj, body) -> 
        parens (horz [ text "for"; text x; text "in"; expr obj; expr body ])
    | ThrowExpr (_, e) ->  parens (horz [ text "throw"; expr e ])
    | FuncStmtExpr (_, f, args, body) ->
        parens (horz [ text "function"; text f; 
                       parens (horz (map text args)); expr body ])
    | PrefixExpr (_, op, e) -> parens (horz [ JavaScript.Pretty.p_prefixOp op;
                                              expr e ])
    | InfixExpr (_, op, e1, e2) ->
        parens (horz [ JavaScript.Pretty.p_infixOp op; expr e1; expr e2 ])
    | AssignExpr (_, lv, e) -> parens (horz [ text "set"; lvalue lv; expr e ])
    | HintExpr (_, txt, e) -> 
        parens (horz [ text ("/**" ^ txt ^ "*/"); expr e ])

  and lvalue lv = match lv with
      VarLValue (_, x) -> text x
    | PropLValue (_, e1, e2) -> squish [ expr e1; brackets (expr e2) ]

  and prop (_, s, e) =  parens (horz [ text s; text ":"; expr e ])

  let p_expr = expr

end
