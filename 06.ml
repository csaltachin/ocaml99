(* [06] Palindrome

   Find out if a list is a palindrome.
   The signature is `is_palindrome : 'a list -> bool`.
*)

let is_palindrome =
  (* We'll use our solution to [05] as a helper. *)
  let rev =
    let rec aux acc ls =
      match ls with
      | [] -> acc
      | hd :: tl -> aux (hd :: acc) tl
    in
    aux []
  in
  fun arr -> arr = rev arr
