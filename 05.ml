(* [05] Reverse a List

   Reverse a list; similar to `List.rev`. The signature is `reverse : 'a list ->
   'a list`. *)

(* Using the `@` operator (list concatenation) *)
let rec reverse list =
  match list with [] -> [] | head :: tail -> reverse tail @ [ head ]

(* Tail-recursive, without using `@` *)
let reverse_tr =
  let rec aux acc list =
    match list with [] -> acc | head :: tail -> aux (head :: acc) tail
  in
  aux []
