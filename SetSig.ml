(* General Set Signature *)

module type SetS = sig

  type 'a t

  val empty : 'a t

  val insert : 'a -> 'a t -> 'a t

  val elem : 'a -> 'a t -> bool

end
