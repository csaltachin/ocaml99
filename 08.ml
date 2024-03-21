(* [08] Eliminate duplicates

   Eliminate consecutive duplicates of list elements. The signature should be
   `compress : 'a list -> 'a list`. *)

let rec compress arr =
  match arr with
  | hd :: (ne :: _ as tl) -> if hd = ne then compress tl else hd :: compress tl
  | a -> a

(* We can also make it tail-recursive by building the accumulator in reverse
   (like in P07) and reversing it at the end. Here we'll just use List.rev. *)
let compress_tr =
  let rec aux acc arr' =
    match arr' with
    | [] -> acc
    | [ hd ] -> hd :: acc
    | hd :: (ne :: _ as tl) ->
        if hd = ne then aux acc tl else aux (hd :: acc) tl
  in
  fun arr -> arr |> aux [] |> List.rev
