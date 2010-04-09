open Prelude
open Lambdajs_syntax

type cpsval =
    Const of JavaScript_syntax.const
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

(*****************************************************************************)

module Cps = struct

  type cont = cpsval -> cpsexp

  let next_node_int = ref 0

  let mk_node (p : pos) : node = 
    let n = !next_node_int in
      incr next_node_int;
      n, p

  let num_nodes () = !next_node_int

  let next_name_int = ref 0

  let mk_name () : id = 
    let n = !next_name_int in
      incr next_name_int;
      "%cps" ^ string_of_int n
  
  let rec cps (exp : exp) (throw : id) (k : cont) : cpsexp = match exp with
      EConst (p, c) -> k (Const c)
    | EId (p, x) -> k (Id x)
    | EObject (p, ps) ->
        cps_list (map thd3 ps) throw 
          (fun vs ->
             let x = mk_name () in
               Bind (mk_node p, x, Object (List.combine (map snd3 ps) vs),
                     k (Id x)))
    | ESeq (p, e1, e2) -> cps e1 throw (fun _ -> cps e2 throw k)
    | EUpdateField (p, e1, e2, e3) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  cps e3 throw
                    (fun v3 ->
                       let x = mk_name () in
                        Bind (mk_node p, x, UpdateField (v1, v2, v3),
                              k (Id x)))))
    | EOp1 (p, op, e) ->
        cps e throw
          (fun v ->
             let r = mk_name () in
               Bind (mk_node p, r, Op1 (op, v), k (Id r)))
    | EOp2 (p, op, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  let r = mk_name () in
                    Bind (mk_node p, r, Op2 (op, v1, v2), k (Id r))))
    | ELet (p, x, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             Bind (mk_node p, x, Let v1,
                   cps e2 throw k))
    | EIf (p, e1, e2, e3) ->
        let cont = mk_name () in
          Fix (mk_node p, 
               [ let r = mk_name () in
                   (cont, [r], k (Id r) ) ],
               cps e1 throw
                 (fun v1 ->
                    If (mk_node p, v1, 
                        tailcps e2 throw cont,
                        tailcps e3 throw cont)))
    | EFix (p, binds, body) ->
        Fix (mk_node p,
             map cps_bind binds,
             cps body throw k)
    | ELambda (p, args, body) -> 
      let f = mk_name ()
      and cont = mk_name ()
      and throw = mk_name () in
        Fix (mk_node p,
             [ (f, cont :: throw :: args, tailcps body throw cont) ],
             k (Id f))
    | EApp (p, f, args) ->
        let cont = mk_name () in
          Fix (mk_node p, 
             [ let r = mk_name () in (cont, [r], k (Id r)) ],
               cps f throw
                 (fun fv ->
                    cps_list args throw
                      (fun argsv ->
                       App (mk_node p, fv, Id cont :: Id throw :: argsv))))
    | ETryCatch (p, body, ELambda (p', [exn], catch_body)) -> 
        let cont = mk_name ()  in
          Fix (mk_node p,
               [ let r = mk_name () in (cont, [r], k (Id r)) ],
             let throw' = mk_name () in
               Fix (mk_node p, 
                    [ throw', [exn], tailcps catch_body throw cont ],
                    tailcps body throw' cont))
    | ETryCatch (p, _, _) -> 
        failwith ("cps : ill-formed catch block at " ^ string_of_position p)
    | EThrow (p, e) -> 
        tailcps e throw throw (* that's right *)
    | ETryFinally (p, body, finally) ->
        let cont = mk_name () in
          Fix (mk_node p,
               [ let r = mk_name () in (cont, [r], k (Id r)) ],
               let normal_exit = mk_name () 
               and throw_exit = mk_name () in
                 (* TODO: account for breaks *)
                 Fix (mk_node p,
                      [ (normal_exit, [mk_name ()], tailcps finally throw cont);
                        let exn = mk_name () in
                          (throw_exit, [exn],
                           cps finally throw 
                             (fun _ -> App (mk_node p, Id throw, [Id exn]))) ],
                      tailcps body throw_exit normal_exit))
    | ELabel (p, lbl, e) ->
        Fix (mk_node p,
             [ let r = mk_name () in
                 (lbl, [r], k (Id r)) ],
             tailcps e throw lbl)
    | EBreak (p, lbl, e) ->
        tailcps e throw lbl

  and cps_list (exps : exp list) (throw : id) (k : cpsval list -> cpsexp) =
    match exps with
        [] -> k []
      | e :: exps' ->
          cps e throw
            (fun v ->
               cps_list exps' throw
                 (fun vs ->
                    k (v :: vs)))

  and tailcps (exp : exp) (throw : id) (return : id) =  match exp with
      EConst (p, c) -> App (mk_node p, Id return, [ Const c ])
    | EId (p, x) -> App (mk_node p, Id return, [ Id x ])
    | EObject (p, ps) ->
        cps_list (map thd3 ps) throw 
          (fun vs ->
             let x = mk_name () in
               Bind (mk_node p, x, Object (List.combine (map snd3 ps) vs),
                     App (mk_node p, Id return, [ Id x ])))
    | ESeq (p, e1, e2) -> cps e1 throw (fun _ -> tailcps e2 throw return)
    | EUpdateField (p, e1, e2, e3) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  cps e3 throw
                    (fun v3 ->
                       let x = mk_name () in
                        Bind (mk_node p, x, UpdateField (v1, v2, v3),
                              App (mk_node p, Id return, [ Id x ])))))
    | EOp1 (p, op, e) ->
        cps e throw
          (fun v ->
             let r = mk_name () in
               Bind (mk_node p, r, Op1 (op, v), 
                     App (mk_node p, Id return, [ Id r ])))
    | EOp2 (p, op, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  let r = mk_name () in
                    Bind (mk_node p, r, Op2 (op, v1, v2),
                          App (mk_node p, Id return, [ Id r ]))))
    | ELet (p, x, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             Bind (mk_node p, x, Let v1,
                   tailcps e2 throw return))
    | EIf (p, e1, e2, e3) ->
        cps e1 throw
          (fun v1 ->
             If (mk_node p, v1, 
                 tailcps e2 throw return,
                 tailcps e3 throw return))
    | EFix (p, binds, body) ->
        Fix (mk_node p,
             map cps_bind binds,
             tailcps body throw return)
    | ELambda (p, args, body) -> 
      let f = mk_name ()
      and cont = mk_name ()
      and throw = mk_name () in
        Fix (mk_node p,
             [ (f, cont :: throw :: args, tailcps body throw cont) ],
             App (mk_node p, Id return, [ Id f ]))
    | EApp (p, f, args) ->
        cps f throw
          (fun fv ->
             cps_list args throw
               (fun argsv ->
                  App (mk_node p, fv, Id return :: Id throw :: argsv)))
    | ETryCatch (p, body, ELambda (p', [exn], catch_body)) -> 
        let throw' = mk_name () in
          Fix (mk_node p, 
               [ throw', [exn], tailcps catch_body throw return ],
               tailcps body throw' return)
    | ETryCatch _ -> failwith "cps : ill-formed catch block"
    | EThrow (p, e) -> 
        tailcps e throw throw (* that's right *)
    | ETryFinally (p, body, finally) ->
        let normal_exit = mk_name () 
        and throw_exit = mk_name () in
          (* TODO: account for breaks *)
          Fix (mk_node p,
               [ (normal_exit, [mk_name ()], tailcps finally throw return);
                 let exn = mk_name () in
                   (throw_exit, [exn],
                    cps finally throw 
                      (fun _ -> App (mk_node p, Id throw, [Id exn]))) ],
               tailcps body throw_exit normal_exit)
    | ELabel (p, lbl, e) ->
        Fix (mk_node p,
             [ let r = mk_name () in
                 (lbl, [r], App (mk_node p, Id return, [ Id r ])) ],
             tailcps e throw lbl)
    | EBreak (p, lbl, e) ->
        tailcps e throw lbl

  and cps_bind ((f, exp) : id * exp) = match exp with
      ELambda (p, args, body) ->
        let cont = mk_name () in
        let throw = mk_name () in
          (f, cont :: throw :: args, tailcps body throw cont)
    | _ -> failwith "cps: ill-formed fix-binding"

end (* struct Cps *)

let cpsexp_idx (cpsexp : cpsexp) = match cpsexp with
  | Fix ((n, _), _, _) -> n
  | App ((n, _), _, _) -> n
  | If ((n, _), _, _, _) -> n
  | Bind ((n, _), _, _, _) -> n

module Pretty = struct

  open Format
  open FormatExt

  let rec p_cpsval (cpsval : cpsval) : printer = match cpsval with
      Const c -> JavaScript.Pretty.p_const c
    | Id x -> text x
    
  and p_prop (x, v) : printer = brackets (horz [ text x; p_cpsval v ])

  let p_bindexp (bindexp : bindexp) : printer = match bindexp with
    | Let v -> p_cpsval v
    | Op1 (op, v1) -> parens (horz [ Pretty.p_op1 op; p_cpsval v1 ])
    | Op2 (op, v1, v2) -> 
        parens (horz [ Pretty.p_op2 op; p_cpsval v1; p_cpsval v2 ])
    | UpdateField (v1, v2, v3) -> 
        parens (horz [ text "update-field"; p_cpsval v1; p_cpsval v2; 
                       p_cpsval v3 ])
    | Object ps -> parens (vert (text "object" :: (map p_prop ps)))

  let p_lambda (p_body : cpsexp -> printer) (f, args, body) : printer =
    horz [ text f; text "=";
           nest (parens (vert [ text "lambda"; parens (horz (map text args)); 
                                p_body body ])) ]

  let rec p_cpsexp (cpsexp : cpsexp) : printer = match cpsexp with
    | Fix ((n, _), binds, body) ->
        vert [ text ("fix/" ^ string_of_int n);
               nest (vert (map (p_lambda p_cpsexp) binds)); p_cpsexp body ]
    | App ((n, _), f, args ) ->
        parens (horz (text ("app/" ^ string_of_int n)
                      :: p_cpsval f :: (map p_cpsval args) ))
    | If ((n, _), v1, e2, e3) -> 
        parens (vert [ horz [ text ("if/" ^ string_of_int n); p_cpsval v1 ];
                       p_cpsexp e2; 
                       p_cpsexp e3 ])
    | Bind ((n, _), x, b, k) ->
        vert [ horz [ text ("let/" ^ string_of_int n); 
                      text x; text "="; p_bindexp b; text "in" ]; 
               p_cpsexp k ]
      
end


let lambda_name (f, _, _) = f

let p_cpsexp  = Pretty.p_cpsexp      

let p = Lexing.dummy_pos, Lexing.dummy_pos

let mk_node = Cps.mk_node

let cps (exp : exp) : cpsexp = 
  Fix (mk_node p,
       [ ("[[return_value]]", [ "v" ],
          App (mk_node p, Id "[[exit]]", [ Id "v" ]));
         ("[[uncaught_exception]]", [ "v" ],
          App (mk_node p, Id "[[exit]]", [ Id "v" ])) ],
  Cps.tailcps exp "[[uncaught_exception]]" "[[return_value]]")



let rec fv (cpsexp : cpsexp) : IdSet.t = match cpsexp with
  |  Fix (_, binds, body) ->
       let bound_ids = IdSetExt.from_list (map fst3 binds) in
         IdSet.diff (IdSetExt.unions (fv body :: map fv_bind binds))
         bound_ids
  | App (_, v, vs) -> IdSetExt.unions (map fv_val (v :: vs))
  | If (_, v1, e2, e3) ->
      IdSetExt.unions [ fv_val v1; fv e2; fv e3 ]
  | Bind (_, x, b, e) -> 
      IdSet.union (fv_bindexp b)
        (IdSet.remove x (fv e))

and fv_val (cpsval : cpsval) = match cpsval with
  | Const _ -> IdSet.empty
  | Id x -> IdSet.singleton x

and fv_bindexp (bindexp : bindexp) = match bindexp with
  | Let v -> fv_val v
  | Op1 (_, v) -> fv_val v
  | Op2 (_, v1, v2) -> IdSet.union (fv_val v1) (fv_val v2)
  | Object ps -> IdSetExt.unions (map (fun (_, v) -> fv_val v) ps)
  | UpdateField (v1, v2, v3) -> 
      IdSet.union (fv_val v1) (IdSet.union (fv_val v2) (fv_val v3))

and fv_bind (_, args, body) =
  IdSet.diff (fv body) (IdSetExt.from_list args)

let fv_immediate (cpsexp : cpsexp) : IdSet.t = match cpsexp with
  |  Fix (_, binds, body) -> IdSet.empty
  | App (_, v, vs) -> IdSetExt.unions (map fv_val (v :: vs))
  | If (_, v1, e2, e3) -> fv_val v1
  | Bind (_, x, b, e) -> fv_bindexp b
