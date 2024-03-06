(* [01] Tail of a list

   Write a function `last : 'a list -> 'a option` that returns the last element of a list. *)

let rec last list =
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> last tail
