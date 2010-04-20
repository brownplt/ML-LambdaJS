{
open Prelude
open Lexing
open JavaScript_parser
open JavaScript_syntax

module S = String

(* TODO: if integer conversions overflow, treat as a float *)
let parse_num_lit (s : string) : token =
  if S.contains s 'x' || S.contains s 'X'
    then Int (int_of_string s)
    else if S.contains s '.'
           then Float (float_of_string s)
           else if S.contains s 'e' || S.contains s 'E'
                  then Float (float_of_string s)
                  else Int (int_of_string s)

let mk_loc (buf : lexbuf) : pos =
  Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf

let block_comment_buf = Buffer.create 120

let string_buf = Buffer.create 100

let comment_start_p = ref dummy_pos
}

(* dec_digit+ corresponds to DecimalDigits in the spec. *)
let dec_digit = ['0'-'9']

let signed_int = dec_digit+ | ('+' dec_digit+) | ('-' dec_digit+)

let expt_part = ['e' 'E'] signed_int

let dec_int_lit = '0' | (['1'-'9'] dec_digit*)

let hex = ['0'-'9' 'A'-'f' 'a'-'f']

let hex_lit = ("0x" | "0X") hex+

let dec_lit = 
  (dec_int_lit '.' dec_digit* expt_part?) | 
  ('.' dec_digit+ expt_part?) |
  (dec_int_lit expt_part?)

let num_lit = dec_lit | hex_lit

let ident = ['a'-'z' 'A'-'Z' '$' '_']['a'-'z' 'A'-'Z' '0'-'9' '$' '_']*

let digit = ['0'-'9']

let char = [^ '"' '\\']

let blank = [ ' ' '\t' ]

let escape_sequence
  = [^ '\r' '\n'] | ('x' hex hex) | ('u' hex hex hex hex)

let double_quoted_string_char = 
  [^ '\r' '\n' '"' '\\'] | ('\\' escape_sequence)

rule token = parse
   | blank + { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
   | '\r' { new_line lexbuf; token lexbuf }
   | "\r\n" { new_line lexbuf; token lexbuf }
   | "/*" { block_comment lexbuf }
   | "//"[^ '\r' '\n']* [ '\r' '\n' ] { new_line lexbuf; token lexbuf }

   | "/*:" { comment_start_p := lexeme_start_p lexbuf; hint lexbuf }

   (* ContinueId and BreakId are tokens for labelled break and continue.  They
    * include their target label.
    *)
   | "continue" [ ' ' '\t' ]+ (ident as x) { ContinueId x }
   | "break" [ ' ' '\t' ]+ (ident as x) { BreakId x }
   | '/' ([^ '*'] double_quoted_string_char* as x) "/gi"
       { Regexp (x, true, true) }
   | '/' ([^ '*'] double_quoted_string_char* as x) "/g"
       { Regexp (x, true, false) }
   | '/' ([^ '*'] double_quoted_string_char* as x) "/i"
       { Regexp (x, false, true) }
   | '/' ([^ '*'] double_quoted_string_char* as x) "/"
       { Regexp (x, false, false) }

   | '"' { string_lit '"' lexbuf }
   | '\'' { string_lit '\'' lexbuf }
   
   | num_lit as x { parse_num_lit x }
   | "{" { LBrace }
   | "}" { RBrace }
   | '(' { LParen }
   | ')' { RParen }
   | "|=" { AssignOp OpAssignBOr }
   | "^=" { AssignOp OpAssignBXor }
   | "&=" { AssignOp OpAssignBAnd }
   | "<<=" { AssignOp OpAssignLShift }
   | ">>=" { AssignOp OpAssignZfRShift }
   | ">>>=" { AssignOp OpAssignSpRShift }
   | "+=" { AssignOp OpAssignAdd }
   | "-=" { AssignOp OpAssignSub }
   | "*=" { AssignOp OpAssignMul }
   | "/=" { AssignOp OpAssignDiv }
   | "%=" { AssignOp OpAssignMod }
   | "%" { Mod }
   | "=" { Assign }
   | ";" { Semi }
   | "," { Comma }
   | "?" { Ques }
   | ":" { Colon }
   | "||" { LOr }
   | "&&" { LAnd }
   | "|" { BOr }
   | "^" { BXor }
   | "&" { BAnd }
   | "===" { StrictEq }
   | "==" { AbstractEq }
   | "!=" { AbstractNEq }
   | "!==" { StrictNEq }
   | "<<" { LShift }
   | ">>" { RShift }
   | ">>>" { SpRShift }
   | "<=" { LEq }
   | "<" { LT }
   | ">=" { GEq }
   | ">" { GT }
   | "++" { PlusPlus }
   | "--" { MinusMinus }
   | "+" { Plus }
   | "-" { Minus }
   | "*" { Times }
   | "/" { Div }
   | "!" { Exclamation }
   | "~" { Tilde }
   | "!" { Exclamation }
   | "." { Period }
   | "[" { LBrack }
   | "]" { RBrack }

   | "if" { If  }
   | "else" { Else  }
   | "true" { True  }
   | "false" { False  }
   | "new" { New  }
   | "instanceof" { Instanceof  }
   | "this" { This  }
   | "null" { Null  }
   | "function" { Function  }
   | "typeof" { Typeof  }
   | "void" { Void  }
   | "delete" { Delete  }
   | "switch" { Switch  }
   | "default" { Default  }
   | "case" { Case  }
   | "while" { While  }
   | "do" { Do  }
   | "break" { Break  }
   | "var" { Var  }
   | "in" { In  }
   | "for" { For  }
   | "try" { Try  }
   | "catch" { Catch  }
   | "finally" { Finally  }
   | "throw" { Throw  }
   | "return" { Return  }
   | "with" { With  }
   | "continue" { Continue  }
   | "instanceof" { Instanceof  }
   | ident as x { Id x }
   | eof { EOF }

and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf; }
  | [ '\n' '\r' ]  { new_line lexbuf; block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

and hint = parse
  | "*/" { let str = Buffer.contents block_comment_buf in
             Buffer.clear block_comment_buf; HINT str }
  | '*' { Buffer.add_char block_comment_buf '*'; hint lexbuf }
  | "\r\n" { new_line lexbuf; Buffer.add_char block_comment_buf '\n'; 
             hint lexbuf }
  | [ '\n' '\r' ] { new_line lexbuf; Buffer.add_char block_comment_buf '\n';
                    hint lexbuf }
  | ([^ '\n' '\r' '*'])+ as txt { Buffer.add_string block_comment_buf txt;
                                  hint lexbuf }

and string_lit end_ch = parse
  (* multi-line *)
  | "\\\r" { string_lit end_ch lexbuf }
  | "\\\n" { string_lit end_ch lexbuf }
  (* escape codes *)
  | "\\'"  { Buffer.add_char string_buf '\''; string_lit end_ch lexbuf }
  | "\\\"" { Buffer.add_char string_buf '\"'; string_lit end_ch lexbuf }
  | "\\\\" { Buffer.add_char string_buf '\\'; string_lit end_ch lexbuf }
  | "\\b" { Buffer.add_char string_buf '\b'; string_lit end_ch lexbuf }
  | "\\n" { Buffer.add_char string_buf '\n'; string_lit end_ch lexbuf }
  | "\\r" { Buffer.add_char string_buf '\r'; string_lit end_ch lexbuf }
  | "\\t" { Buffer.add_char string_buf '\t'; string_lit end_ch lexbuf }
  (* NOTE: OCaml does not support Unicode characters. See the OCaml "Batteries"
     for a library that does. *)
  | "\\v" { Buffer.add_char string_buf '\x0B'; string_lit end_ch lexbuf }
  | "\\ " { Buffer.add_char string_buf ' '; string_lit end_ch lexbuf }
  | "\\0" { Buffer.add_char string_buf '\x00'; string_lit end_ch lexbuf }
  | "\\x" (hex hex as ascii)
      { Buffer.add_char string_buf (char_of_int (int_of_string ("0x" ^ ascii)));
        string_lit end_ch lexbuf }
  (* NOTE: This is probably wrong, due to lack of Unicode support. *)
  | "\\u" (hex hex hex hex as uni)
      { Buffer.add_char string_buf (char_of_int (int_of_string ("0x" ^ uni)));
        string_lit end_ch lexbuf }
  | _ as ch
      { if end_ch = ch then begin
          let s = Buffer.contents string_buf in
            Buffer.clear string_buf;
            String s
        end
        else begin
          Buffer.add_char string_buf ch; 
          string_lit end_ch lexbuf
        end
      }
          

