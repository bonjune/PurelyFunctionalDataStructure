namespace Chap2

type 'a Stack =
    | Nil
    | Cons of 'a * 'a Stack

module Stack =
    let empty = Nil
    let isEmpty = function
        | Nil -> true
        | _ -> false

    // where is the cons function? (there is cons function on the book)
    // cons function is equivalent to the type constructor Cons in the type definition
    // no need to write again

    let head = function
        | Nil -> failwith "Head of empty stack"
        | Cons(hd, _) -> hd

    let tail = function
        | Nil -> failwith "Tail of empty stack"
        | Cons(_, tl) -> tl

    let tryHead = function
        | Nil -> None
        | Cons(hd, _) -> Some hd

    let tryTail = function
        | Nil -> None
        | Cons(_, tl) -> Some tl

    // Concatenation function
    // which provides persistence on xs and ys
    // but this is inefficient: time complexity O(n)
    let rec concat xs ys =
        match xs, ys with
        | Nil, _ -> ys
        | _, Nil -> xs
        | Cons(xHead, xTail), Cons(_) -> Cons(xHead, concat xTail ys)
    
    let (++) xs ys  = concat xs ys

    // Another function to concantenate two lists
    let rec update xs i y =
        match xs, i with
        | Nil, _ -> failwith "Update of empty stack"
        | Cons(x, tl), 0 -> Cons(y, tl)
        | Cons(x, tl), i -> Cons(x, update tl (i-1) y)
    
    // Exercise 2.1
    let rec suffixes = function
    | Nil -> Nil
    | Cons(hd, tl) as s -> Cons(s, suffixes tl)

