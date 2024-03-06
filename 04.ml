(* [05] Length of a List

   Find the number of elements of a list.
   The signature is `length : 'a list -> int`. *)

let length =
  let rec aux acc list =
    match list with
    | [] -> acc
    | _ :: tail -> aux (acc + 1) tail
  in
  aux 0
