(* [09] Pack duplicates

   Pack consecutive duplicates of list elements into sublists, returning a list
   of lists. The signature should be `pack : 'a list -> 'a list list`. *)

let pack =
  let rec aux acc_block acc_top arr' =
    match (acc_block, arr') with
    | [], hd :: tl -> aux [ hd ] acc_top tl
    | ahd :: _, hd :: tl ->
        if ahd = hd then aux (hd :: acc_block) acc_top tl
        else aux [] (acc_block :: acc_top) arr'
    | _, [] ->
        if acc_block = [] then List.rev acc_top
        else List.rev (acc_block :: acc_top)
  in
  fun arr -> aux [] [] arr
