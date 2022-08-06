
(* Red Black Tree Signature *)

module type  RBTreeImplS = sig

  type color = R | B

  type 'a tree = E
               | T of color * 'a tree * 'a * 'a tree

  include RBTreeSig.RBTreeS with type 'a t = 'a tree

  (* You may add additional functions below that you wish to
     expose so that they can be more easily tested.

     Some implementations of `is_red_black_tree` may use helper
     function and you may add them here to facilitate testing.
   *)

end

module RBTreeImplM : RBTreeImplS = struct

  type color = R | B

  type 'a tree = E
               | T of color * 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  let empty = E

  let balance color left value right : 'a tree =
    match color, left, value, right with
    | B, T(R, T(R, a, x, b), y, c), z, d
    | B, a, x, T(R, b, y, T(R, c, z, d))
    | B, T(R, a, x, T(R, b, y, c)), z, d
    | B, a, x, T(R, T(R, b, y, c), z, d)
        -> T(R, T(B, a, x, b), y, T(B, c, z, d))
    | c, l, v, r -> T(c,l,v,r)

  (*Adapted from Class Slides 7.2*)
  let rec insert (v: 'a) (t: 'a tree) : 'a tree =
    let rec ins t' =
      (match t' with 
      | E -> T(R,E,v,E)
      | T(c,l,x,r) ->
          if v < x then balance c (ins l) x r
          else if v > x
            then balance c l x (ins r) 
          else t')
    in match ins t with
    | E -> raise(Failure "Error in insert")
    | T(_,l,x,r) -> T(B,l,x,r)
  
  let rec elem (v: 'a) (t: 'a tree) : bool = 
    match t with
    | E -> false
    | T(_, l,x,r) when v = x -> true
    | T(_, l,x,r) when v < x -> elem v l
    | T(_, l,x,r) -> elem v r

  let rec height (t: 'a tree) =
    match t with
    | E -> 0
    | T(_,l,x,r) -> max (1 + height l) (1 + height r)

  let rec size (t: 'a tree) = 
    match t with
    | E -> 0
    | T(_,l,x,r) -> 1 + size l + size r

  let rec min_t (t: 'a tree) : 'a option =
    let min_o a b =
      match (a, b) with
      | (Some x, Some y) -> if x < y then Some x else Some y
      | (Some x, None) -> Some x
      | (None, Some y) -> Some y
      | (None, None) -> None
    in match t with
    | E -> None
    | T (_,l, v, r) -> min_o (Some v) (min_o (min_t l) (min_t r))

  let rec max_t (t: 'a tree) : 'a option =
    let max_o a b =
      match (a, b) with
      | (Some x, Some y) -> if x > y then Some x else Some y
      | (Some x, None) -> Some x
      | (None, Some y) -> Some y
      | (None, None) -> None
    in match t with
    | E -> None
    | T (_,l, v, r) -> max_o (Some v) (max_o (max_t l) (max_t r))

  let rec is_bst (t: 'a tree) : bool = 
    match t with
    | E -> true
    | T(_,l, v, r) -> let a = (max_t l) in let b = (min_t r) in
                      ((a < Some v) || a = None) && 
                      ((b > Some v) || b = None) &&
                      is_bst l && is_bst r

  
  let rec check_reds (t: 'a tree) : bool =
    match t with 
    | E -> true
    | T (R, T(R,_,_,_), _, _) -> false 
    | T (R, _, _, T(R,_,_,_)) -> false        
    | T (_, l, _, r) -> true && check_reds l && check_reds r
  
  (*let rec check_blacks (t: 'a tree) : bool =
    match t with
    | E -> true
    | T(B,E,_,T(R,l,v,r)) | T(B,T(R,l,v,r),_,E)
       -> true && check_blacks (T(R,l,v,r))
    | T(B,T(R,l1,v1,r1),_,T(R,l2,v2,r2)) -> true
    | T(_,E,_,E) -> true
    | T(R,T(B,l1,v1,r1),_,T(B,l2,v2,r2))
       -> true && check_blacks (T(B,l1,v1,r1)) && check_blacks (T(B,l2,v2,r2))
    | _ -> false*)
  let check_blacks (t: 'a tree) : bool =
    let rec blk_in_leftmost t' acc =
      match t' with
        | E -> acc
        | T(B,l,_,_) -> blk_in_leftmost l (acc+1)
        | T(R,l,_,_) -> blk_in_leftmost l acc
    in let c = blk_in_leftmost t 0
    in let rec check_paths t acc =
      match t with
      | E -> acc == c
      | T(B,l,_,r) -> (check_paths l (acc+1)) && (check_paths r (acc+1))
      | T(R,l,_,r) -> (check_paths l acc) && (check_paths r acc)
    in check_paths t 0

  
  let is_red_black_tree (t: 'a tree) : bool =
    check_reds t && check_blacks t && is_bst t


end



(* Below we create a new modules `RBTreeM` that only exposes the
   red-black tree functionality in the `RBTreeS` signature.
   Functions that may be useful for testing, such as `all_paths`
   are not accessible in `RBTreeM`.

   We "seal" `RBTreeM` with the signature `RBTreeS` so that it only
   exposes the elements of `RBTreeS`.   
*)
module RBTreeM : RBTreeSig.RBTreeS = RBTreeImplM



(* Below we create a new module `TreeSetM` that only exposes the
   set functionality in the `SetS` signature. Functions like
   `height` and `size` that are accessible in `RBTreeM` are not
   accessible in `RBTreeSetM`.

   We "seal" `RBTreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module RBTreeBSTM : BSTreeSig.BSTreeS = RBTreeImplM



(* Below we create a new module `RBTreeSetM` that only exposes the
   set functionality in the `SetSig` signature. Functions like
   `height` and `size` that are accessible in `RBTreeM` are not
   accessible in `RBTreeSetM`.

   We "seal" `RBTreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module RBTreeSetM : SetSig.SetS = RBTreeImplM
