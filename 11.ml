(* [11] Modified Run-Length Encoding

   Modify run-length encoding to include single elements and (count, element) pairs.
   We define a type `'a rle` below. The function signature should be `encode : 'a list -> 'a rle list`.
*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

(* This is very similar to P10 and P09. Here, we accumulate a "block" (this time an `'a rle` value) inside an option. This does mean we have to peek inside the block very often. Also, the branches in aux where `block_opt` is `None` are reached very rarely (if at all), since after a first element is processed, block_opt always stays `Some`. *)

let encode =
  let unwrap block =
    match block with
    | One x | Many (_, x) -> x
  in
  let increment block =
    match block with
    | One x -> Many (2, x)
    | Many (c, x) -> Many (c + 1, x)
  in
  let rec aux block_opt top_acc arr' =
    match arr', block_opt with
    | [], None -> List.rev top_acc
    | [], Some block -> List.rev (block :: top_acc)
    | hd :: tl, None -> aux (Some (One hd)) top_acc tl
    | hd :: tl, Some block ->
      if hd = unwrap block
      then aux (Some (increment block)) top_acc tl
      else aux (Some (One hd)) (block :: top_acc) tl
  in
  aux None []

(* We could also use `compress` from P09 and then map. But that's too easy. A third approach would be similar to our `encode_alt` from P10. Instead of accumulating the block, we accumulate a counter, and convert to an `'a rle` value just in time before appending. Instead of peeking inside a current block, we constantly peek at the next item in the list. *)

let encode_alt =
  let wrap c x =
    match c with
    | 1 -> One x
    | c -> Many (c, x)
  in
  let rec aux count top_acc arr' =
    match arr' with
    (* Just like in P10, this case is only reached if we get the empty list passed in at the start... *)
    | [] -> []
    (* ...and these are the actual base and recursive cases. *)
    (* We keep the same invariant as we did in the P10 version. *)
    | [ hd ] ->
      let last_block = if count = 0 then One hd else Many (count + 1, hd) in
      List.rev (last_block :: top_acc)
    | hd :: (ne :: _ as tl) ->
      if hd = ne
      then aux (count + 1) top_acc tl
      else (
        (* Note that we shouldn't skip ahead both `hd` and `ne`, as we could end up in the `arr' = []` branch, which should not happen here. *)
        let new_block = wrap (count + 1) hd in
        aux 0 (new_block :: top_acc) tl)
  in
  aux 0 []

(* Note that in the previous function, the `wrap` helper function by itself does not guarantee that we cannot accidentally construct a `Many (0, x)` object, or a `Many (n, x)` object for some negative n. But we can reason that our `aux` implementation will never do that mistake. Indeed, we only ever pass in `count + 1` as the first argument to `wrap`, and since `count` never drops below zero, `count + 1` will always be positive.

   That said, if we wanted to further enforce this at the type level, we could have `wrap` take in a parameter of some stricter `pos_int` type instead of int, and have `count` be something like a `pos_int option`, where `None` corresponds to zero. The easiest way to define `pos_int` would probably be as a variant `One | Succ of pos_int`, but then you have to convert this to an int later because the `'a rle` items have type `(int * 'a)`. And you could do that conversion tail-recursively, but I think it might be quite slower than when using ints directly.

   In a sense, this "unsafety" is already present in the `'a rle` type itself, as it happens with many other types in many programs. As far as I understand it, an idiomatic thing to do is to hide as much as you can behind module signatures (the .mli interfaces), and only expose safe ways to create, modify or consume values of those types to outsiders. Or alternatively, only expose the things you need, and take extra care to reason that those things work safely when they're supposed to. Raising exceptions is a side effect! (Not really, but you should always document them, and using them sparingly feels nice.)
*)
