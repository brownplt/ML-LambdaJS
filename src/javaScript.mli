open Prelude
open JavaScript_syntax

val parse_javascript : in_channel -> string -> prog

module Pretty : sig

  open Format
  open FormatExt

  val p_const : const -> printer
  val p_expr : expr -> printer
  val p_stmt : stmt -> printer
  val p_prog : prog -> printer
  val p_infixOp : infixOp -> printer
  val p_prefixOp : prefixOp -> printer
end
