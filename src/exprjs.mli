open Prelude
open Exprjs_syntax

val from_javascript : JavaScript_syntax.prog -> expr

module Pretty : sig

  open FormatExt

  val p_expr : expr -> printer
end
