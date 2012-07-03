open Format

type printer = formatter -> unit

let empty (fmt : formatter) : unit = ()
 
let nest (p : printer) (fmt : formatter) : unit =
  pp_open_vbox fmt 2;
  p fmt;
  pp_close_box fmt ()

let text s fmt = pp_print_string fmt s

let bool b fmt = pp_print_bool fmt b
 
let rec inter (sep : printer) (lst : printer list) (fmt : formatter) : unit = match lst with
    x1 :: x2 :: xs' ->
      pp_open_box fmt 2;
      x1 fmt; 
      pp_close_box fmt ();
      sep fmt;
      inter sep (x2 :: xs') fmt
  | [x] -> 
      pp_open_box fmt 2;
      x fmt;
      pp_close_box fmt ()
  | [] -> ()

let sep = inter (fun fmt -> pp_print_space fmt ())

let rec squish (lst : printer list) (fmt : formatter) : unit = match lst with
  | x :: xs -> x fmt; squish xs fmt
  | [] -> ()

let add_sep_between sep items =
  if items = [] then [] else
  let rec repl n tl = if n <= 0 then tl else repl (n-1) (sep::tl) in
  let seps = repl (List.length items - 1) [empty] in
  List.map2 (fun item s -> squish[item; s]) items seps 
 
let vert (p : printer list) (fmt : formatter) : unit = 
  pp_open_vbox fmt 0;
  sep p fmt;
  pp_close_box fmt ()
 
let hov n b (p : printer list) (fmt : formatter) : unit = 
  pp_open_hvbox fmt b;
  inter (fun fmt -> pp_print_break fmt n 0) p fmt;
  pp_close_box fmt ()

let horz (p : printer list) (fmt : formatter) : unit = 
  pp_open_hbox fmt ();
  sep p fmt;
  pp_close_box fmt ()
  
let horzOrVert (p : printer list) (fmt : formatter) : unit =
  pp_open_hvbox fmt 0;
  sep p fmt;
  pp_close_box fmt ()

let hnest n (p : printer) (fmt : formatter) : unit =
  pp_open_hvbox fmt n;
  p fmt;
  pp_close_box fmt ()

let print_space fmt = pp_print_space fmt ()

let hvert (p : printer list) (fmt : formatter) : unit =
  let rec intersperse a lst = match lst with
      [] -> []
    | [x] -> [x]
    | x :: xs -> x :: a :: (intersperse a xs)
  in
  hnest 2 (squish (intersperse print_space p)) fmt

let wrapBox (p : printer list) (fmt : formatter) : unit =
  pp_open_hovbox fmt 0;
  sep p fmt;
  pp_close_box fmt ()

let int n fmt = pp_print_int fmt n
 
let float f fmt = pp_print_float fmt f
 
let enclose indent label cut l r (inner : printer list) (fmt : formatter) =
  pp_open_hvbox fmt indent;
  pp_open_hbox fmt ();
  pp_print_string fmt label;
  l fmt;
  pp_close_box fmt ();
  cut fmt;
  sep inner fmt;
  if cut != empty then pp_print_break fmt 0 (0 - indent);
  r fmt;
  pp_close_box fmt ()

let label l = enclose 2 l empty empty empty
 
let label_parens l cut = enclose 2 l cut (text "(") (text ")")
let parens = label_parens "" empty
 
let label_braces l cut = enclose 2 l cut (text "{") (text "}")
let braces = label_braces "" empty
 
let label_brackets l cut = enclose 2 l cut (text "[") (text "]")
let brackets = label_brackets "" empty

let label_angles l cut = enclose 2 l cut (text "<") (text ">")
let angles = label_angles "" empty

let string s = enclose 2 "" empty (text "\"") (text "\"") [text (String.escaped s)]

let label_pair l p1 p2 fmt = 
  pp_open_hvbox fmt 1;
  pp_open_hbox fmt ();
  pp_print_string fmt l;
  pp_print_string fmt "(";
  pp_close_box fmt ();
  sep [squish [p1; text ","]; p2] fmt;
  pp_print_string fmt ")";
  pp_close_box fmt ()

let pair = label_pair ""

let to_string (f : 'a -> printer) (x : 'a) : string  =
  f x str_formatter;
  flush_str_formatter ()
