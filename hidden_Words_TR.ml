open SetSig
open Hidden_Words_Sig
open Util

(* Solution module utilizing primarily tail recursive functions *)
module HiddenImpl_TR_F (S: SetS)  = struct

  let rev_tr (lst: 'a list) : 'a list =
    let rec rev_aux lst acc =
      match lst with
      | [] -> acc
      | x::xs -> rev_aux xs (x::acc)
    in rev_aux lst []

  (*Takes an int x and a string list and returns a string S.t containing words
    of length x*)
  let words_of_length_tr x words: string S.t = 
    let rec wol_aux x words acc =
      match words with
      | [] -> acc
      | w::ws when String.length w = x -> wol_aux x ws (S.insert w acc)
      | w::ws -> wol_aux x ws acc
    in wol_aux x words S.empty
  
  let filter_tr (f: 'a -> bool) (lst: 'a list) : 'a list =
    let rec filter_aux f lst acc =
      match lst with
      | [] -> acc
      | x::xs when f x -> filter_aux f xs (x::acc)
      | _::xs -> filter_aux f xs acc
    in filter_aux f lst []
  
  (*Functions to takes a string and 
    return a char list list of all divisions of the word*)
  let find_suffixes_tr (w: char list) : char list list =
    let rec find_suf_aux w acc =
      match w with
        | [] -> acc
        | c::cs -> find_suf_aux cs (w::acc)
    in find_suf_aux w []

  let find_prefixes_tr (w: char list) : char list list =
    let rec find_pre_aux w acc cur_pre =
      match w with
      | [] -> acc
      | c::cs -> find_pre_aux cs (rev_tr (c::cur_pre)::acc) (c::cur_pre)
    in find_pre_aux (w) [] []

  let affix_word_tr (word: string) : (char list * char list) list =
    let rec all_affixes pres sufs acc = 
      match pres with
      | [] -> acc
      | p::ps -> let s::ss = sufs in all_affixes ps ss ((p,s)::acc)
    in let _::prefixes = find_prefixes_tr (UtilM.explode word)
    in let suffixes = find_suffixes_tr (UtilM.explode word)
    in all_affixes prefixes suffixes []

  let rec flatten_tr (lst: 'a list) acc =
    let rec fs_aux lst acc =
      match lst with
      | [] -> acc
      | x::xs -> fs_aux xs (x::acc)
    in match lst with
       | [] -> acc
       | x::xs -> flatten_tr xs (fs_aux x acc)

  let create_splits_tr (words: string list) : (char list * char list) list =
    let rec splits_aux words acc =
      match words with
      | [] -> acc
      | w::ws -> splits_aux ws ((affix_word_tr w)::acc)
    in flatten_tr(splits_aux words []) []

  let find_candidates_tr (splits: (char list * char list) list) (word: string) 
    : (string*string*string) list =
    let rec insert (split: (char list * char list)) (word: string) =
      let (pre,suf) = split in (UtilM.implode pre, word, UtilM.implode suf)
    in let rec fc_aux splits word acc =
      match splits with
      | [] -> acc
      | x::xs -> (fc_aux xs word ((insert x word)::acc))
    in fc_aux splits word []
  
  let find_all_candidates_tr (splits: (char list * char list) list) (words: string list) =
    let rec fac_aux splits words acc =
      match words with
      | [] -> acc
      | w::ws -> fac_aux splits ws ((find_candidates_tr splits w)::acc)
    in flatten_tr(fac_aux splits words []) []
  
  let ans_condense ((prefix,infix,suffix): (string*string*string)) : string =
      prefix^infix^suffix

  let format_ans (ans: (string*string*string) list) : (string*string*string) list =
    let rec fa_tr ans acc =
      match ans with
      | [] -> acc
      | x::xs -> let (a,b,c) = x in fa_tr xs ((b,a^c,a^b^c)::acc)
    in fa_tr ans []

  let hidden_words (word_list: string list) = 
    let threes = filter_tr (fun s -> String.length s = 3) word_list in
    let fives = filter_tr (fun s -> String.length s = 5) word_list in
    let eights = words_of_length_tr 8 word_list in
    let candidates = find_all_candidates_tr (create_splits_tr fives) threes in
    let answers = filter_tr (fun s -> S.elem (ans_condense s) eights) candidates in
    format_ans answers



end

module HiddenWords_TR_F (S: SetS) : HiddenS = HiddenImpl_TR_F(S)