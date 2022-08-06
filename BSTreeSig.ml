
(* Binary Search Tree Signature*)

module type BSTreeS = sig

  include SetSig.SetS

  (* Returns the greatest number of nodes any branch in a
     given tree has. *)
  val height: 'a t -> int

  (* Returns the number of nodes in a given tree. *)
  val size: 'a t -> int

  (* Checks if a given tree satisfies the requirements of
     a binary search tree. *)
  val is_bst: 'a t -> bool

end
