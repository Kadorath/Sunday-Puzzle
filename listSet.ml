open SetSig

(* Ordered List *)

module ListSetM : SetS = struct

  type 'a t = 'a list

  let empty = []

  let rec insert (v: 'a) (lst: 'a list) : 'a list = 
    match lst with
    | [] -> [v]
    | x::xs when v < x -> v::x::xs
    | x::xs -> x::(insert v xs)

  let rec elem (v: 'a) (lst: 'a list) : bool = 
    match lst with
    | [] -> false
    | x::xs -> x = v || (v > x && elem v xs)
  
end
