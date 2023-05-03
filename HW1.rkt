#lang plait
#|
Razie Hyria
CMPSC 460 - Homework #1
1/22/23
|#

#|provided datatype|#
; would an error be thrown if we used a different type with the same tree structure
(define-type Tree
    (leaf [val : Number])
    (node [val : Number]
          [left : Tree]
          [right : Tree]))


#|Part 1 — Sum 
Implement a sum function that takes a tree and returns the sum of the numbers in the 
tree. 
Example: (sum (node 5 (leaf 6) (leaf 7))) should produce 18. |#

(define (sum num)
  (cond ; for the nodes, add the left and right recursively
    ; can i use foldl for this?
    [(node? num) (+ (+ (node-val num) (sum (node-left num))) (sum (node-right num)))]
    [(leaf? num) (leaf-val num)])) ; else there is only a leaf, 

(display "sum of node 5 leaf 6 leaf 7 is: ")
(sum (node 5 (leaf 6) (leaf 7)))

#|Part 2 — Negate
Implement the function negate, which takes a tree and returns a tree that has the 
same shape, but with all the numbers negated.|#
(define (negate num)
  (cond
    ; if the value is a node, then take the value at that node and change it to the negated
    ; form of the current node-val, then recurse call the left and right nodes
  [(node? num) (node (* -1 (node-val num)) (negate(node-left num)) (negate(node-right num)))]
  [(leaf? num) (leaf(* -1 (leaf-val num)))]
  ; the same process is repeated for the leaf node
))
(display "before negation: node 5 leaf 6 leaf 7, after: ")
(negate(node 5 (leaf 6) (leaf 7)))

#|Part 3 — Contains?
Implement the function contains?, which takes a tree and a number and returns #t if 
the number is in the tree, #f otherwise. |#
; is it better practice to specific tree : Tree and num : Number?
; theres a contains? package that you can download for racket
(define (contains? tree num)
  (cond
    ;checking the nodes
    ; if the leaf node in the tree is equal to the number
    [(leaf? tree) (= (leaf-val tree) num)] 
    [(node? tree) (or (= (node-val tree) num)(or(contains? (node-left tree) num) (contains? (node-right tree) num)))]))
    ; elif the node in the tree is the value, OR if we recurse the left OR right sides and check if its present,
    ; otherwise its false

(display "Contains 5? expected true: ")
(contains? (node 5 (leaf 6) (leaf 7)) 5)

#|Part 4 — Big Leaves? 
Implement the function big-leaves?, which takes a tree and returns #t if every leaf 
is bigger than the sum of numbers in the path of nodes from the root that reaches 
the leaf. |#
; creating helper function
(define (big-helper tree path)
  (cond
    ; exit condition, if the leafs value is greater than the sum
    [(leaf? tree) (> (leaf-val tree) path)]
    ; if not will continue to recurse and update the value of the path by adding the values for LR nodes 
    [(node? tree) (and (big-helper (node-left tree) (+ (node-val tree) path))(big-helper (node-right tree) (+ (node-val tree) path)))]
    ))

; calling on the helper function, and setting the path to zero
(define (big-leaves? tree)
  (big-helper tree 0))

(display "Big Leaves? expected true: ") ; thought id spoil you with some cute console msgs
(big-leaves? (node 5 (leaf 6) (leaf 7)))

(display "Big Leaves? expected false: ")
(big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) 
#|Part 5 — Positive Trees? 
Implement the function positive-trees?, which takes a list of trees and 
returns #t if every member of the list is a positive tree, where a positive tree is 
one whose numbers sum to a positive value.|#
;(define (positiveTrees lst))

(define (positive-trees? lst)
  (if [empty? lst] ; first condition of empty list being true
      #t 
      (and (> (sum (first lst)) 0) (positive-trees? (rest lst)))))
      ; else keep cyling through the list index and checking the sum of the value 

(display "testing empty: ")
(test (positive-trees? empty) #t)

(display "testing one: ")
(test (positive-trees? (cons (leaf 6) empty)) #t)

(display "testing negative: ")
(test (positive-trees? (cons (leaf -6) empty)) #f)

(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))(cons (node 0 (leaf 0) (leaf 1))empty))) #t)
(test (positive-trees? (cons (node -1 (leaf 6) (leaf -6))(cons (node 0 (leaf 0) (leaf 1))empty)))#f)
(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 7) #t)
#|Part 6 - Bonus : Flatten
Implement the function flatten, which takes a tree and
returns a list that has all numbers in the tree’s nodes and leaves.|#
