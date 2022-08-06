(* Red Black Tree Signature *)

module type RBTreeS = sig

  include BSTreeSig.BSTreeS

  (* A function to check that the invariants of a red-black 
     tree hold.  These are:
     - no red node has a red child
     - all paths have the same number of black nodes
   *)
  val is_red_black_tree: 'a t -> bool

end  
