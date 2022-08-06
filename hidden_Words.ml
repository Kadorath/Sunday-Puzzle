open SetSig
open Hidden_Words_Sig
open Util

(* Solution given as a functor, which solves the puzzle utilizing
   a given set data structure. *)
module HiddenWordsImplF (S: SetS)  = struct

  (*Functions implode and explode and read_words 
    taken from HW specifications*)
  let implode (cs: char list) : string =
    String.concat "" (List.map  (String.make 1) cs)
  
  let explode (s: string) : char list =
    let l = String.length s in
    let rec f i = if i = l then [] else s.[i] :: f (i+1) in
    f 0
  
  let read_words (file_name: string) : string list =
    let ic = open_in file_name in
    let rec read_lines ic = try
        let next_line = input_line ic in
        next_line :: read_lines ic
      with _ -> []
    in
    let raw_strings = read_lines ic 
    in
    List.filter (fun s -> String.length s > 0)
      (List.map String.trim raw_strings)    

  (*Takes an int x and a string list and returns a string S.t containing words
    of length x*)
  let rec words_of_length x words : string S.t = 
    match words with
    | [] -> S.empty
    | w::ws when String.length w = x -> S.insert w (words_of_length x ws)
    | w::ws -> words_of_length x ws
  

  (*Takes a string and return a char list list of all divisions of the word*)
  let rec affix_word (word: string) : (char list * char list) list =
    let rec find_suffixes (w: char list) : char list list =
      match w with
      | [] -> []
      | c::cs -> (w)::find_suffixes(cs)
    in let rec find_prefixes (w: char list) : char list list =
        let rec find_prefixes_helper (w: 'a list) (acc: 'a list) : 'a list list =
          match w with
          | [] -> []
          | c::cs -> acc::(find_prefixes_helper cs (acc @ [c]))
        in find_prefixes_helper w []
    in let rec all_affixes (pre: char list list) (suf: char list list) : (char list * char list) list =
      match suf with
      | [y] -> let [x] = pre in [(x, y)]
      | y::ys -> let x::xs = pre in
                (x,y)::(all_affixes xs ys)
    in let cs = explode word in let suffs = find_suffixes cs 
    in let pres = find_prefixes cs in 
    let x::xs = all_affixes pres suffs in xs

  (*Takes a list of splits [as in from affix_word] and a word, 
    and returns a list of insertions*)
  let rec find_candidates (splits: (char list * char list) list) (word: string) : (string*string*string) list =
    let rec insert (split: (char list * char list)) (word: string) =
      let (pre,suf) = split in (implode pre, word, implode suf)
    in match splits with
    | [] -> []
    | x::xs -> (insert x word)::(find_candidates xs word)
  
  let ans_condense ((prefix,infix,suffix): (string*string*string)) : string =
    prefix^infix^suffix
  
  (*Divides the word list into three-letters, five-letters, and eight-letters, creates a list
    of five-letter splits using affix_word, creates a list of three-letter five-letter combos
    using find_candidates, and then searches the eight-letter words to find which candidates
    are valid words, returning a list of those valid candidates, and then it formats those
    answers*)
  let rec hidden_words (lst: string list) : (string*string*string) list =
    let threes = List.filter (fun s -> String.length s = 3) lst in
    let fives = List.filter (fun s -> String.length s = 5) lst in
    let eights = words_of_length 8 lst in
    let rec create_splits words =
      match words with
      | [] -> []
      | w::ws -> (affix_word w) @ (create_splits ws)
    in let rec find_all_candidates splits words =
      match words with
      | [] -> []
      | w::ws -> (find_candidates splits w) @ (find_all_candidates splits ws)
    in let candidates = find_all_candidates (create_splits fives) threes
    in let answers = List.filter (fun s -> S.elem (ans_condense s) eights) candidates
    in List.map (fun (f,m,l) -> (m,f^l,f^m^l)) answers

    let rec print_answers (ans: (string * string * string) list) =
      match ans with
      | [] -> ()
      | (w3,w5,w8) :: rest -> 
         print_endline ( "(" ^ w3 ^ ", " ^ w5 ^ ", " ^ w8 ^ ")" );
         print_answers rest

end

module HiddenWordsF (S: SetS) : HiddenS = HiddenWordsImplF(S)
