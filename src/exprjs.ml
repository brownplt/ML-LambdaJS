open Prelude
open Exprjs_syntax

let from_javascript = Exprjs_syntax.from_javascript

let dummy_p = (Lexing.dummy_pos, Lexing.dummy_pos)
let rec lift_decls e = 
  let apply_decls (exp, decls) =
    let wrapE e = List.fold_right (fun id body -> 
      SeqExpr(dummy_p, 
              VarDeclExpr(dummy_p, id, BotExpr dummy_p),
              body)) decls e in
    match exp with
    | LabelledExpr(p, id, e) -> LabelledExpr(p, id, wrapE e) 
    | _ -> wrapE exp in
  let rec lift_list es decls =
    let (es', decls') =
      List.fold_left (fun (es, decls) e -> let (e', decls') = lift e decls in (e'::es, decls')) ([], decls) es in
    (List.rev es', decls') 
  and lift_field (p, id, e) decls =
    let (e', decls') = lift e decls in ((p, id, e'), decls')
  and lift_fields (es : 'a list) (decls : 'c list) : ('b list * 'c list) =
    let (es', decls') =
      List.fold_left (fun (es, decls) e -> 
        let (e', decls') = lift_field e decls in (e'::es, decls')) ([], decls) es in
    (List.rev es', decls') 
  and lift e decls = match e with
    | FuncStmtExpr (p, name, args, body) ->
      let func' = lift_decls (FuncExpr(p, args, body)) in
      (AssignExpr(dummy_p, VarLValue(dummy_p, name), func'), name::decls)
    | VarDeclExpr (p, v, e) ->
      let (e', decls') = lift e decls in
      (AssignExpr(dummy_p, VarLValue(dummy_p, v), e'), v::decls')

    | FuncExpr(p, args, body) ->
      let (body', lifted_body_decls) = lift body [] in
      let body' = apply_decls (body', lifted_body_decls) in
      (FuncExpr(p, args, body'), decls)

    | BotExpr _ -> (e, decls)
    | ConstExpr _ -> (e, decls)
    | ArrayExpr(p, es) ->
      let (es', decls') = lift_list es decls in
      (ArrayExpr(p, es'), decls')
    | ObjectExpr (p, fields) ->
      let (fs', decls') = lift_fields fields decls in
      (ObjectExpr(p, fs'), decls')
    | ThisExpr _ -> (e, decls)
    | VarExpr(p, id) -> (e, decls)
    | IdExpr _ -> (e, decls)
    | BracketExpr(p, e1, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (BracketExpr(p, e1', e2'), decls')
    | NewExpr(p, f, args) ->
      let (f', decls') = lift f decls in
      let (args', decls') = lift_list args decls' in
      (NewExpr(p, f', args'), decls')
    | PrefixExpr(p, id, e) ->
      let (e', decls') = lift e decls in
      (PrefixExpr(p, id, e'), decls')
    | InfixExpr(p, op, e1, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (InfixExpr(p, op, e1', e2'), decls')
    | IfExpr(p, e1, e2, e3) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      let (e3', decls') = lift e3 decls' in
      (IfExpr(p, e1', e2', e3'), decls')
    | AssignExpr(p, lval, e) ->
      let (e', decls') = lift e decls in
      (AssignExpr(p, lval, e'), decls')
    | AppExpr(p, f, args) ->
      let (f', decls') = lift f decls in
      let (args', decls') = lift_list args decls' in
      (AppExpr(p, f', args'), decls')
    | LetExpr(p, id, e1, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (LetExpr(p, id, e1', e2'), decls')
    | SeqExpr(p, e1, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (SeqExpr(p, e1', e2'), decls')
    | WhileExpr(p, lbl, e1, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (WhileExpr(p, lbl, e1', e2'), decls')
    | DoWhileExpr(p, lbl, e1, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (DoWhileExpr(p, lbl, e1', e2'), decls')
    | LabelledExpr(p, id, e) ->
      let (e', decls') = lift e decls in
      (LabelledExpr(p, id, e'), decls') 
    | BreakExpr(p, id, e) ->
      let (e', decls') = lift e decls in
      (BreakExpr(p, id, e'), decls')
    | ForInExpr(p, lbl, id, e1, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (ForInExpr(p, lbl, id, e1', e2'), decls')
    | TryCatchExpr(p, e1, id, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (TryCatchExpr(p, e1', id, e2'), decls')
    | TryFinallyExpr(p, e1, e2) ->
      let (e1', decls') = lift e1 decls in
      let (e2', decls') = lift e2 decls' in
      (TryFinallyExpr(p, e1', e2'), decls')
    | ThrowExpr(p, e) ->
      let (e', decls') = lift e decls in
      (ThrowExpr(p, e'), decls')
    | ParenExpr(p, e) ->
      let (e', decls') = lift e decls in
      (ParenExpr(p, e'), decls')
  in apply_decls (lift e [])
    

module Pretty = struct

  open Format
  open FormatExt

  let rec expr e = match e with
    | ConstExpr (_, c) -> JavaScript.Pretty.p_const c
    | ArrayExpr (_, es) -> parens (horz (map expr es))
    | ObjectExpr (_, ps) -> brackets (vert (map prop ps))
    | ThisExpr _ -> text "#this"
    | IdExpr (_, x) -> text x
    | VarExpr (_, x) -> text ("scope." ^ x)
    | BracketExpr (_, e1, e2) -> squish [ expr e1; brackets (expr e2) ]
    | NewExpr (_, c, args) -> 
        parens (horz (text "new" :: expr c :: map expr args))
    | IfExpr (_, e1, e2, e3) ->
        parens (vert [ horz [ text "if"; expr e1 ]; expr e2; expr e3 ])
    | AppExpr (_, f, args) -> parens (hov 1 2 (expr f :: map expr args))
    | FuncExpr (_, args, body) ->
        parens (vert [ horz [text "fun"; parens (horz (map text args))]; expr body ])
    | LetExpr (_, x, e1, e2) ->
        parens (vert [ horz [ text "let"; text x; text "="; expr e1; 
                              text "in" ];
                       expr e2 ])
    | SeqExpr (_, e1, e2) -> parens (vert [ text "seq"; expr e1; expr e2 ])
    | VarDeclExpr (_, x, e) ->  horz [ text "var"; text x; text "="; expr e ]
    | WhileExpr (_, lbls, e1, e2) -> 
      List.fold_right (fun lbl body -> 
        parens (vert [horz[text "label"; text lbl]; body]))
        lbls
        (parens (vert [ text "while"; expr e1; expr e2 ]))
    | DoWhileExpr (_, lbls, e1, e2) ->
      List.fold_right (fun lbl body -> 
        parens (vert [horz[text "label"; text lbl]; body]))
        lbls
        (parens (vert [ text "do-while"; expr e1; expr e2 ]))
    | LabelledExpr (_, x, e) ->
        parens (vert [ horz [text "label"; text x]; expr e ])
    | BreakExpr (_, x, e) ->
        parens (hov 1 0 [ horz [text "break"; text x]; expr e ])
    | TryCatchExpr (_, body, x, catch) ->
        parens (vert [ text "try"; expr body; 
                       parens (vert [horz [text "catch"; text x]; expr body ]) ])
    | TryFinallyExpr (_, body, finally) ->
        parens (vert [ text "try"; expr body; 
                       parens (vert [ text "finally"; expr finally ])])
    | ForInExpr (_, lbls, x, obj, body) -> 
      List.fold_right (fun lbl body -> 
        parens (vert [horz[text "label"; text lbl]; body]))
        lbls
        (parens (horz [ text "for"; text x; text "in"; expr obj; expr body ]))
    | ThrowExpr (_, e) ->  parens (horz [ text "throw"; expr e ])
    | FuncStmtExpr (_, f, args, body) ->
        parens (hov 1 2 [horz [ text "function"; text f; 
                             parens (horz (map text args))]; expr body ])
    | PrefixExpr (_, op, e) -> parens (horz [ text op; expr e ])
    | InfixExpr (_, op, e1, e2) -> parens (horz [ text op; hov 1 0 [expr e1; expr e2] ])
    | AssignExpr (_, lv, e) -> parens (hov 1 2 [horz [ text "set"; lvalue lv]; expr e ])
    | ParenExpr (_, e) -> parens (horz [ text "parens"; expr e ])
    | BotExpr _ -> text "_|_"

  and lvalue lv = match lv with
      VarLValue (_, x) -> text x
    | PropLValue (_, e1, e2) -> squish [ expr e1; brackets (expr e2) ]

  and prop (_, s, e) =  parens (hov 1 2 [horz [ text s; text ":"]; expr e ])

  let p_expr = expr

end
