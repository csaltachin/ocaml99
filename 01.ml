(* [01] Tail of a list

   Write a function that returns the last element of a list. The signature
   should be `last : 'a list -> 'a option`. *)

let rec last list =
  match list with [] -> None | [ x ] -> Some x | _ :: tail -> last tail
