(* [02] Last two elements of a list

   Find the last but one (last and penultimate) elements of a list.
   Here the signature is `last_two : 'a list -> ('a * 'a) option`. If the list has less than two elements, it returns `None`.
*)

let rec last_two list =
  match list with
  | [] | [ _ ] -> None
  | [ penul; last ] -> Some (penul, last)
  | _ :: tail -> last_two tail
