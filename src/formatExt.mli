(** Helper functions for working with the builtin [Format] library. *)
open Format

type printer = formatter -> unit

val empty : printer
val print_space : printer
val cut : printer

val nest : printer -> printer
val hnest : int -> printer -> printer

val sep : printer list -> printer

val squish : printer list -> printer

val vert : printer list -> printer
val hov : int -> int -> printer list -> printer
val horz : printer list -> printer
val horzOrVert : printer list -> printer
val hvert : printer list -> printer
val wrapBox : printer list -> printer

val text : string -> printer
val int : int -> printer
val float : float -> printer
val bool : bool -> printer
val string : string -> printer (* wraps argument in quotes, and escapes it per ML's escaping rules *)

val inter : printer -> printer list -> printer

val add_sep_between : printer -> printer list -> (printer) list

val enclose : int -> string -> printer -> printer -> printer -> printer list -> printer
val label : string -> printer list -> printer

val label_parens : string -> printer -> printer list -> printer
val parens : printer list -> printer

val label_braces : string -> printer -> printer list -> printer
val braces : printer list -> printer

val label_brackets : string -> printer -> printer list -> printer
val brackets : printer list -> printer

val label_angles : string -> printer -> printer list -> printer
val angles : printer list -> printer

val label_pair : string -> printer -> printer -> printer
val pair : printer -> printer -> printer

(** [to_string f x] uses [Format.str_formatter] as the buffer for printing [x]
    with [f]. *)
val to_string : ('a -> printer) -> 'a -> string
