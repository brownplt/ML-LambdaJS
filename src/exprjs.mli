open Prelude
open Exprjs_syntax

val from_javascript : JavaScript_syntax.prog -> expr

val lift_decls : expr -> expr

module Pretty : sig

  open FormatExt

  val p_expr : expr -> printer
end
