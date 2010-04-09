open Prelude
open Lambdajs_syntax

type parsed_env

val parse_env : in_channel -> string -> parsed_env

val enclose_in_env : parsed_env -> exp -> exp
