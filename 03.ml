(* [03] N'th Element of a List

   Find the N'th element of a list. This should be similar to `List.nth` from
   the standard library. The signature here is `at : int -> 'a list -> 'a
   option`. Instead of possibly throwing, it returns an option. *)

let rec at n list =
  if n < 0 then None
  else
    match (n, list) with
    | _, [] -> None
    | 0, head :: _ -> Some head
    | k, _ :: tail -> at (k - 1) tail
