open Prelude

type value =
  | Const of JavaScript_syntax.const
      (* A VarCell can contain an ObjCell, but not vice versa.  This
      mimics the semantics of heap-like object refs alongside mutable
      variables *)
  | VarCell of value ref 
      (* Objects shouldn't have VarCells in them, but can have any of
      the other kinds of values *)
  | ObjCell of (value IdMap.t * ((value IdMap.t) IdMap.t)) ref
  | Closure of (value list -> value)

type env = value IdMap.t
type label = string

exception Break of label * value
exception Throw of value
