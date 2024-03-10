(* [12] Decode a Run-Length Encoded List

   Reconstruct the uncompressed list from a run-length code list.
   The `'a rle` type is the same as before, but now we need to implement the inverse direction. The signature should be `decode : 'a rle list -> 'a list`.
*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

(* Straightforward. We keep an accumulator, and every time we read an encoded item, we decode it (i.e. convert it to a list `[x; x; ...; x]`) and prepend it to the accumulator. Of course, we reverse at the end to get the correct order. This is basically a `List.concat_map`. *)

let decode =
  let rec f acc item =
    match item with
    | One x -> x :: acc
    | Many (2, x) -> f (x :: acc) (One x)
    | Many (c, x) -> f (x :: acc) (Many (c - 1, x))
  in
  let rec aux acc arr' =
    match arr' with
    | [] -> List.rev acc
    | hd :: tl -> aux (f acc hd) tl
  in
  fun arr -> aux [] arr

(* Notice that if we rewrite this as `let decode = ... aux []` and check the type of `decode`, we'll get some weak types in there. This means that once we apply decode to some list, say an `int rle list`, it will from there on be typed as `int rle list -> int list`, and will give a type error when used with non-ints.

   One way to fix this (without needing to redefine f on every call to decode) is to mention the argument `arr` explicitly at the end, as we've done here. This explicitly declares `decode` as a "syntactic value", which makes the type-checker happy enough to allow it to be polymorphic (instead of non-polymorphic but with pending type deduction). I'll expand on that now.

   This phenomenon is called the "value restriction", and OCamlverse has a great explanation here: [https://ocamlverse.net/content/weak_type_variables.html]. So far, what I gather is: the type-checker has some restrictions on what kind of values with type-holes (like values of type _ -> _) it can label as polymorphic (can work fine with any type), as opposed to just "not typed enough yet" (can work with an undetermined type, but must set that type in stone once chosen). This is because, if the computation has some hidden mutation inside of it (like a cache) that happens inside the computation closure, but the type of which does not show up in the type-holes of the computation type, then such mutation can be used to raise type errors.

   For example, suppose `decode` had an `_ option ref` cache that started out as holding `None`, captured its output if empty, and otherwise always returned the cache contents. When `decode` is declared, it now contains a persistent cache; but since the cache's contents start out as `None`, the type-checker cannot yet deduce the type of the inner option, or the type of the cache itself. Say we call it for the first time with an `int rle list`; then the option inside the cache is now holding a value of type `int list`. But this means that the type-checker can now deduce (with full certainty) that the cache itself has type `int list option ref`. And the cache's type cannot change, so it can further deduce that `decode x` will be of type `int list` for any x we feed it later. In other words, the type-checker now knows that the hole in `decode: _ rle list -> _ list` must absolutely be filled by `int`. The type-checker has delayed-ly, but finally, finished the type deduction of `decode` as non-polymorphic, and so it will no longer allow `decode` to be called with an argument that is not of type `int rle list`.

   So yeah, when the type-checker encounters type-holes, there is a tension between labeling the value as "I know right away that this can be polymorphic" or as "I cannot be 100% sure right away that this can be polymorphic, so to be safe, I will hereon assume it will *not* be polymorphic, and I will finish filling its holes at some point in the future, either at the toplevel or when reading later declarations". We want our type-checker to be sound (no false positives), and this hesitance is a compromise that we end up needing to make. Why do some perfectly innocent, mutation-free functions (declared as computed values) give weak types too? Well, apparently, the type checker can't peek inside the "body" of a computed value too well. If a computed value has the same type/holes as another "bad" value like the previous example, the type-checker cannot distinguish between them, so it has to be strict: it assumes that both could be bad, and allows neither to be polymorphic.

   Okay, so why does the above work when we write `fun arr -> aux [] arr`? I think the idea is that the `fun` keyword, along with the body, gives the type-checker some extra information about what `decode` is. It declares `decode` as a "syntactic value", and it so happens that the type-checker is happy to allow syntactic values with type-holes to be polymorphic. This `fun` declaration is slightly more explicit than `... in aux []`, in the same sense that a *function declaration* is more syntactically explicit than a *variable declaration* in other languages. Contrasting what we said earlier, the type-checker can now view the "computation body" as the actual *body of a function declaration*, so it can peek inside in a smarter way, I think (see below).
*)

(* My understanding of all this is not super-complete yet; perhaps I'll update this rant when I have a better grasp on it. For now, here's an example of what the `fun` syntactic declaration buys you, from what I've been able to observe.

   In this version of `List.rev`, we are not using `fun`, but rather declaring a computed value of holed type `_ list -> _ list`. Lo and behold, `rev_weak` gets weakly-typed, regardless of whether it actually contains a ref (or any record with mutable fields) inside. Check this yourself by commenting out the `let _ = ref None in` line and inspecting the type of `rev_weak` before and after.
*)

let rev_weak =
  let _ = ref None in
  let rec aux acc arr =
    match arr with
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux []

(* And here's the same declaration as below, but made into an explicit, syntactic `fun` declaration. As it is right now, it's also weakly typed (check this). Now try commenting out the same line as before! What's the type of `rev_stronger` now? *)

let rev_stronger =
  let _ = ref None in
  let rec aux acc arr =
    match arr with
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  fun arr -> aux [] arr

(* It's worth noting that none of this happens at runtime. In OCaml, all type information is erased during compilation, so types don't "exist" at runtime. Well, it's a bit different in the toplevel (utop), because the compilation over there is more interactive, so we can peek types of previously declared values in there. But the point is that no type-checking or hole-filling happens at runtime.

   Indeed, you can already observe the kind of type errors we discussed earlier, right here in the source code. Try the following:
   (1) Check that `rev_weak` is weakly-typed.
   (2) Uncomment the first line below, and check the type of `rev_weak` again. Notice anything different?
   (3) Now uncomment the second line. What happens? Why?
*)

(* let _ = rev_weak [1; 2; 3] *)
(* let _ = rev_weak ["ew"; "weak"; "types"] *)
