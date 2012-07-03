open Lexing

type id = string


module IdOrderedType = struct
  type t = id
  let compare = Pervasives.compare
end

module Pos = struct

  type t = Lexing.position * Lexing.position * bool (* start, end, is synthetic? *)

  let dummy = (Lexing.dummy_pos, Lexing.dummy_pos, true)
  let compare = Pervasives.compare

  let before (_, p1_end, _) (p2_start, _, _) = 
    p1_end.pos_cnum < p2_start.pos_cnum

  let synth (p_start, p_end, _) = (p_start, p_end, true)
  let synthetic (p_start, p_end) = (p_start, p_end, true)
  let real (p_start, p_end) = (p_start, p_end, false)
  let rangeToString p e =
    if (p.pos_lnum = e.pos_lnum) 
    then Format.sprintf "%s:%d:%d-%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
      (e.pos_cnum - e.pos_bol)
    else Format.sprintf "%s:%d:%d-%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
      e.pos_lnum (e.pos_cnum - e.pos_bol)
  let toString (p, e, _) = rangeToString p e
  let toLexPos (s, e, _) = (s, e)
  let isSynthetic (_, _, synth) = synth
end

module Int = struct
  type t = int
  let compare = Pervasives.compare
end

module IntSet = Set.Make (Int)
module IntSetExt = SetExt.Make (IntSet)

module IdSet = Set.Make (IdOrderedType)

module IdSetExt = SetExt.Make (IdSet)

module PosSet = Set.Make (Pos)

module PosSetExt = SetExt.Make (PosSet)

module PosMap = Map.Make (Pos)

module PosMapExt = MapExt.Make (Pos) (PosMap)

module IdMap = Map.Make (IdOrderedType)

module IdMapExt = MapExt.Make (IdOrderedType) (IdMap)

let fold_left = List.fold_left

let fold_right = List.fold_right

let map = List.map

let printf = Printf.printf

let eprintf = Printf.eprintf

let sprintf = Printf.sprintf

let second2 f (a, b) = (a, f b)

let third3 f (a, b, c) = (a, b, f c)

let snd3 (a, b, c) = b

let snd2 (a, b) = b

let fst2 (a, b) = a

let fst3 (a, _, _) = a

let thd3 (_, _, c) = c

let rec intersperse a lst = match lst with
    [] -> []
  | [x] -> [x]
  | x :: xs -> x :: a :: (intersperse a xs)

let rec take_while f xs = match xs with
    [] -> [], []
  | x :: xs' -> 
      if f x then
        let lhs, rhs = take_while f xs' in
          x :: lhs, rhs
      else
        [], xs

let rec match_while f xs = match xs with
    [] -> [], []
  | x :: xs' -> begin match f x with
        Some y ->
          let ys, xs'' = match_while f xs' in
            y :: ys, xs''
      | None -> [], xs
    end



let rec rem (elt : 'a) (lst : 'a list) : 'a list = match lst with
    [] -> []
  | x :: xs -> if elt = x then rem elt xs else x :: (rem elt xs)

let rec nub (lst : 'a list) : 'a list = match lst with
    [] -> []
  | x :: xs -> x :: (nub (rem x xs))

let rec iota' m lst = 
  if m < 0 then lst
  else iota' (m - 1) (m :: lst)

let iota n = iota' (n - 1) []

let timefn msg f arg =
  let startT = Unix.time () in
  Printf.eprintf "%s: Starting...\n" msg; Pervasives.flush stderr;
  try
    let ret = f arg in
    let endT = Unix.time () in
    Printf.eprintf "%s: Done.  Time elapsed: %d\n" msg (int_of_float (endT -. startT));
    Pervasives.flush stderr;
    ret
  with e ->
    let endT = Unix.time () in
    Printf.eprintf "%s: Done.  Time elapsed: %d\n" msg (int_of_float (endT -. startT));
    Pervasives.flush stderr;
    raise e

let opt_map f o = match o with None -> None | Some o -> Some (f o)
let map_pair f (x, y) = (f x, f y)
