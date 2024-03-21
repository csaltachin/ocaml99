(* [07] Flatten list

   Write a function that flattens a nested list structure represented by the 'a
   node type. The signature here is `flatten: 'a node -> 'a list`. This is
   slightly different from the original exercise description, where it wants us
   to feed the function an `'a node list`. But that's practically the same as a
   `Many` node. *)

type 'a node = One of 'a | Many of 'a node list

(* First approach.

   Note that the time complexity of `a @ b` is linear on the length of a; it's
   just prepending the elements of a, one by one, from right to left, to b. So
   if `a1, a2, ... an` are lists whose lengths add up to p, and we compute

   `(((a1 @ a2) @ ...) @ an`

   in that order, we will not do O(p) work, but rather O(p^2)! The k-th
   concatenation takes work linear on the length of `a1 @ ... @ ak`. In
   contrast, if we compute

   `a1 @ (a2 @ (... @ an) ...)`

   in that order, the k-th concatenation will take work linear on the length of
   `an-k`, and the total work will be O(p).

   I think this could be a reason why in the standard library, the `@` operator
   is right-associative. Both of the above compute to the same final list, but
   only the latter has the expected linear time complexity.

   Unfortunately for our recursion below, if the node tree is very left-heavy,
   we'll fall prey to the quadratic complexity from the former. *)

let rec flatten = function
  | One leaf -> [ leaf ]
  | Many [] -> []
  | Many (child :: tail) -> flatten child @ flatten (Many tail)

(* Now, without using `@`.

   We want to build the final list only by cons-ing. If we build the list by
   successive appends, each append will be expensive: we have to travel the
   entire acc list until the tail, and then cons the tail with the new element.

   On the other hand, prepending to a list is cheap; we just cons the list
   (head) from the left. So we can travel the node tree from left to right,
   build the list with prepends, and when we're done, we just reverse the final
   list before returning it. Each cons takes O(1) time, and each leaf value
   participates in only one cons. So if we can reverse the resulting list in
   linear time, we're all done in worst-case linear time!

   This is not quite tail recursive, since we nest a call to aux in one of the
   aux branches. At least we only need h call stack frames, where h is the
   height of the tree. *)

let rev =
  let rec aux acc = function [] -> acc | hd :: tl -> aux (hd :: acc) tl in
  aux []

let flatten_cons =
  let rec aux acc = function
    | One leaf -> leaf :: acc
    | Many [] -> acc
    | Many (child :: tail) -> aux (aux acc child) (Many tail)
  in
  fun node -> rev (aux [] node)
