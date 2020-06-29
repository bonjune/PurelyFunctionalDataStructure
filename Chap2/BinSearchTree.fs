namespace Chap2

type 'a Tree =
    | Leaf
    | Node of 'a Tree * 'a * 'a Tree

type 'a Set = 'a Tree

module Tree =
    // member is a keyword of F#
    let rec isMember x = function
    | Leaf -> false
    | Node (left, e, right) ->
        if x < e then isMember x left
        else if e < x then isMember x right
        else true
    
    let rec insert x = function
    | Leaf -> Node(Leaf, x, Leaf)
    | Node(left, e, right) as node ->
        if x < e then Node(insert x left, e, right)
        else if x > e then Node(left, e, insert x right)
        else node
    
    // TODO
    // Exercise 2.2
    // Exercise 2.3
    // Exercise 2.4
    // Exercise 2.5
    // Exercise 2.6 - Implement FiniteMap
        

