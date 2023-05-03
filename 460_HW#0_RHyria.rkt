#lang plait
#|
Razie Hyria
CMPSC 460 - Homework #0
1/27/23
|#

(define option 'extra)

#|PROBLEM 1
returns a list containing n copies of x|#
(define (duple n x)
  (cond ; use condition
    [(zero? n) empty] ; to check if the multiplier is zero, then just return an empty list
    [else (cons x (duple (- n 1) x))]))
    ; otherwise,

(test (duple 2 3) '(3 3))
(test (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha)))
(test (duple 0 '(word)) '())

#|PROBLEM 2
return a sorted list of integers based on operator
less than < ascending, greater than > descending > |#
(define (merge [op : (Number Number -> Boolean)][int-list1 : (Listof Number)][int-list2 : (Listof Number)]) : (Listof Number)
  (cond
    [(and (empty? int-list1) (empty? int-list2)) '()] ; check if both lists are empty, return empty
    [(empty? int-list1) int-list2];check if one list is empty, return the other respective list
    [(empty? int-list2) int-list1]
    [(op (first int-list1) (first int-list2)) ;checking if first element in first list is greater than first element in second list
     (cons (first int-list1) (merge op (rest int-list1) int-list2))]
    ; if so, combine the contents into a list make a recursive call to check the value of the remaining items in list 1 w list 2
     [else (cons (first int-list2) (merge op int-list1 (rest int-list2)))]))
    ; otherwise, combine the contents of the second list into a list make a recursive call to check the value of the remaining items with list 1 
    ;if neither are empty, ascending logic
 
(test (merge < '(1 4 6) '(2 5 8)) '(1 2 4 5 6 8))
(test (merge > '(6 4 1) '(8 5 2)) '(8 6 5 4 2 1))

#|Problem 3
return an association list from a list of symbols and a list of numbers
define a type to allow for the output of a list of associations
?_t is to be replaced with your appropriately named type |# 
(define-type Assoclist
  (asc [sym : Symbol]
        [num : Number])) ;creating association list for the numbers and symbols

(define (make-assoc [names : (Listof Symbol)] [values : (Listof Number)]): (Listof Assoclist)
  (if (empty? names) empty ; if the names arent present, then empty
      (cons (asc (first names) (first values)) (make-assoc (rest names)(rest values)))))
;otherise use cons to put into a list the type associated values, then recurse the remaining values through the function
      

(test (make-assoc '(a b c d) '(1 2 3 4)) (list (asc 'a 1) (asc 'b 2) (asc 'c 3) (asc 'd 4)))
(test (make-assoc '(t a c o tuesday) '(0 1 34 1729 42)) (list (asc 't 0) (asc 'a 1) (asc 'c 34) (asc 'o 1729) (asc 'tuesday 42))) 