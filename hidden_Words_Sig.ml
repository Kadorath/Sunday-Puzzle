(* Signature for Solution modules and functors that only exposes
   the solution function. *)
module type HiddenS = sig
  val hidden_words : string list -> (string * string * string) list
  
end
