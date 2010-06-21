open Prelude

type value = 
  | Const of JavaScript_syntax.const
  | Cell of value ref
  | Closure of (value list -> value)
  | Object of value IdMap.t

val evaluate : Lambdajs_syntax.exp -> value
