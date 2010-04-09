open Prelude
open Lambdajs_cps

type loc = 
  | Loc of int
  | LocField of int * string

module Loc = struct

  type t = loc

  let compare = Pervasives.compare

  open FormatExt

  let pp loc = match loc with
    | Loc n -> int n
    | LocField (n, f) -> text (string_of_int n ^ ":" ^ f)


end

module RT = struct
  type t =
    | Number
    | String
    | Boolean
    | Function
    | Object
    | Undefined

  let compare = Pervasives.compare

  open FormatExt

  let pp v = match v with
    | Number -> text "number"
    | String -> text "string"
    | Boolean -> text "boolean"
    | Function -> text "function"
    | Object -> text "object"
    | Undefined -> text "undefined"

end
  
module AV = struct
  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of JavaScript_syntax.const
    | ARef of loc
    | AObj of loc IdMap.t
    | AClosure of int * id list * cpsexp
      
  let compare = Pervasives.compare 

  open FormatExt

  let pp v = match v with
  | AConst c -> JavaScript.Pretty.p_const c
  | ARef l ->  sep [ text "*"; Loc.pp l ]
  | AObj dict ->
      IdMapExt.p_map (fun s -> text ("\"" ^ String.escaped s ^ "\"")) Loc.pp
        dict
  | AClosure (n, args, _) -> text ("closure" ^ string_of_int n)
  | ABool -> text "boolean"
  | ANumber -> text "number"
  | AString -> text "string"

end

module AVSet = Set.Make (AV)
module AVSetExt = SetExt.Make (AVSet)
module RTSet = Set.Make (RT)
module RTSetExt = SetExt.Make (RTSet)

module Heap = Map.Make (Loc)
module HeapExt = MapExt.Make (Loc) (Heap)

type heap = AVSet.t Heap.t

let deref loc heap =
  try 
    Heap.find loc heap
  with Not_found ->
    eprintf "%s is not a location in the heap " 
      (FormatExt.to_string Loc.pp loc);
    raise Not_found

module Type = struct

  type t =
    | Set of AVSet.t
    | LocTypeof of Loc.t
    | LocTypeIs of Loc.t * RTSet.t
    | Deref of Loc.t

  let compare t1 t2 = match t1, t2 with
    | Set r1, Set r2 -> AVSet.compare r1 r2
    | LocTypeIs (l1, s1), LocTypeIs (l2, s2) when l1 = l2 -> RTSet.compare s1 s2
    | _ -> Pervasives.compare t1 t2

  let up (h : heap) t = match t with
    | Deref l -> deref l h
    | Set r -> r
    | LocTypeof _ -> AVSet.singleton AV.AString
    | LocTypeIs _ -> AVSet.singleton AV.ABool

  let union h av1 av2 = match av1, av2 with
    | Set r1, Set r2 -> Set (AVSet.union r1 r2)
    | LocTypeof x, LocTypeof y when x = y -> LocTypeof x
    | LocTypeIs (x, s), LocTypeIs (y, t) when x = y -> 
        LocTypeIs (x, RTSet.union s t)
    | Deref x, Deref y when x = y -> Deref x
    | _ -> Set (AVSet.union (up h av1) (up h av2))



  open FormatExt

  let pp t = match t with
    | Set set -> AVSetExt.p_set AV.pp set
    | LocTypeof x -> sep [ text "typeof"; Loc.pp x ]
    | LocTypeIs (x, t) -> sep [ text "typeis";  Loc.pp x; 
                                 RTSetExt.p_set RT.pp t ]
    | Deref l -> sep [ text "deref"; Loc.pp l ]

end


type env = Type.t IdMap.t

open FormatExt


let singleton t = Type.Set (AVSet.singleton t)

let empty = Type.Set AVSet.empty

let union_env heap (env1 : env) (env2 : env) : env = 
  IdMapExt.join (fun _ -> Type.union heap) env1 env2

let p_env env = IdMapExt.p_map text Type.pp env

let p_heap heap = HeapExt.p_map Loc.pp (AVSetExt.p_set AV.pp) heap

let lookup (x : id) (env : env) =
  try
    IdMap.find x env
  with Not_found ->
    eprintf "%s is unbound in the environment:\n%s\n" x
    (FormatExt.to_string p_env env);
    raise Not_found

let bind (x : id) v  (env : env) : env = IdMap.add x v env

let empty_env = IdMap.add "[[exit]]" (singleton AV.ABool) IdMap.empty


let union_heap h1 h2 = HeapExt.join (fun _ -> AVSet.union) h1 h2

let set_ref loc value heap =
  Heap.add loc value heap

let empty_heap = Heap.empty

let compare_heap h1 h2 = Heap.compare AVSet.compare h1 h2

let compare_env env1 env2 = IdMap.compare Type.compare env1 env2

let locations set = 
  let f v lst = match v with
    | AV.ARef loc -> loc :: lst
    | _ -> lst in
    AVSet.fold f set []
