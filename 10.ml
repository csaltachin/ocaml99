(* [10] Run-Length Encoding

   Perform run-length encoding on a list
   [https://en.wikipedia.org/wiki/Run-length_encoding]. Basically, replace each
   length-c block of consecutive copies of x with the tuple (c, x). The
   signature should be `encode : 'a list -> (int * 'a) list`. *)

(* Very similar to P09. In fact, we could first compress the list into blocks,
   like in P09, and then map each block into an `('a * int)` tuple. *)

let encode =
  let rec aux acc_block acc_top arr' =
    match (acc_block, arr') with
    (* Base cases *)
    | None, [] -> List.rev acc_top
    | Some (count, x), [] -> List.rev ((count, x) :: acc_top)
    (* Recursive cases *)
    | None, hd :: tl -> aux (Some (1, hd)) acc_top tl
    | Some (count, x), hd :: tl ->
        if x = hd then aux (Some (count + 1, x)) acc_top tl
        else aux (Some (1, hd)) ((count, x) :: acc_top) tl
  in
  aux None []

(* Here's another idea, where instead of accumulating an `('a * int)` tuple, we
   accumulate a counter. We just keep peeking the next item. *)

let encode_alt =
  let rec aux count top arr' =
    match arr' with
    (* This case is a shortcut only for when we encode the empty list *)
    | [] -> []
    (* These are the real base and recursive cases. We maintain this invariant:
       if `arr' = hd :: _`, then we are currently building an `hd` block of
       current size `count`. *)
    | [ x ] -> List.rev ((count + 1, x) :: top)
    | hd :: (ne :: _ as tl) ->
        if hd = ne then aux (count + 1) top tl
        else aux 0 ((count + 1, hd) :: top) tl
  in
  aux 0 []
