open FormatExt

(** Additional functions for working with maps. *)
module type S = sig
  type key
  type +'a t

  val from_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val map_to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  val find_default : 'a t -> key -> 'a -> 'a
  val keys : 'a t -> key list
  val values : 'a t -> 'a list

  (** [union f map1 map2] requires both [map1] and [map2] to have the same set
      of keys. [union] signals a [Not_found] exception if either contains keys
      that the other does not. *)
  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** [join f map1 map2] returns a new map with all the keys of [map1] and 
      [map2]. If a key exists on both maps, its two values are combined using
      [f]. *)
  val join : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** p_map lbl cut k v prints lbl {cut[k(key) => v(val)], [k(key) => v(val)], ..., [k(key) => v(val)]}, 
      with optional linebreaks (cut) at the braces *)
  val p_map : string -> printer -> (key -> printer) -> ('a -> printer) -> 'a t -> printer

  val diff : 'a t -> 'a t -> 'a t

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

end

module Make : functor (Ord : Map.OrderedType) -> 
  functor (Map : Map.S with type key = Ord.t) -> S
  with type key = Ord.t
  and type +'a t = 'a Map.t
