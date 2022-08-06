
(* Binary Search Tree *)

module type  BSTreeImplS = sig
  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  include BSTreeSig.BSTreeS with type 'a t = 'a tree
end


module BSTreeImplM : BSTreeImplS = struct

  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  type 'a t = 'a tree


  let empty = Leaf

  let rec insert (v: 'a) (t: 'a tree) : 'a tree = 
    match t with
    | Leaf -> Fork(Leaf, v, Leaf)
    | Fork(l,x,r) when v > x -> Fork(l, x, insert v r)
    | Fork(l,x,r) -> Fork(insert v l, x, r)

  let rec elem (v: 'a) (t: 'a tree) : bool = 
    match t with
    | Leaf -> false
    | Fork(l,x,r) when v = x -> true
    | Fork(l,x,r) when v < x -> elem v l
    | Fork(l,x,r) -> elem v r

  let rec height (t: 'a tree) : int =
    match t with
    | Leaf -> 0
    | Fork(l,x,r) -> max (1 + height l) (1 + height r)

  let rec size (t: 'a tree) : int = 
    match t with
    | Leaf -> 0
    | Fork(l,x,r) -> 1 + size l + size r
  
  let rec min_t (t: 'a tree) : 'a option =
    let min_o a b =
      match (a, b) with
      | (Some x, Some y) -> if x < y then Some x else Some y
      | (Some x, None) -> Some x
      | (None, Some y) -> Some y
      | (None, None) -> None
    in match t with
    | Leaf -> None
    | Fork (l, v, r) -> min_o (Some v) (min_o (min_t l) (min_t r))

  let rec max_t (t: 'a tree) : 'a option =
    let max_o a b =
      match (a, b) with
      | (Some x, Some y) -> if x > y then Some x else Some y
      | (Some x, None) -> Some x
      | (None, Some y) -> Some y
      | (None, None) -> None
    in match t with
    | Leaf -> None
    | Fork (l, v, r) -> max_o (Some v) (max_o (max_t l) (max_t r))

  let rec is_bst (t: 'a tree) : bool = 
    match t with
    | Leaf -> true
    | Fork(l, v, r) -> let a = (max_t l) in let b = (min_t r) in
                      ((a < Some v) || a = None) && 
                      ((b > Some v) || b = None) &&
                      is_bst l && is_bst r

end

module BSTreeM : BSTreeSig.BSTreeS = BSTreeImplM

module BSTreeSetM : SetSig.SetS = BSTreeM


