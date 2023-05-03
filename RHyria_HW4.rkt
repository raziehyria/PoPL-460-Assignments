#lang plait
; Name:            Razie Hyria
; Parts Completed: 1, and 2
; Date:            3/5/23

;SRC: store-with.rkt
(define-type-alias Location Number)

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boxV [l : Location]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (boxE [arg : Exp])
  (unboxE [arg : Exp])
  (setboxE [bx : Exp] ; PART 1- Change set-box!
           [val : Exp])
  ;(beginE [l : Exp] ; removing the old definition of begin
          ;[r : Exp])
  (beginE+ [l : (Listof Exp)])) ; Generalize begin to allow one or more sub-expressions
  
(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))
(define mt-env empty)
(define extend-env cons)
(define-type Storage
  (cell [location : Location] 
        [val : Value]))

(define-type-alias Store (Listof Storage))
(define mt-store empty)
(define override-store cons)
(define-type Result
  (v*s [v : Value] [s : Store]))

#|DEPRECIATED: updating the overrider directly to 
(define (replace-overrider []))
was going to change override behavior to drop set values instead, but too much code to change
didnt feel like it, but it wouldve roughly been the same with instead
using Storage/store and val to check if store is empty and recursing through cell collection w the rest of the store|#


;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{box ANY} s)
     (boxE (parse (second (s-exp->list s))))]
    
    [(s-exp-match? `{unbox ANY} s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (setboxE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    
    ;[(s-exp-match? `{begin ANY ANY} s)
     ;(beginE (parse (second (s-exp->list s)))
             ;(parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{begin ANY ...} s) ; updating s-exp
     (beginE+ (map parse (rest (s-exp->list s))))]
    
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))


;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
    body)
  (type-case Result call
    [(v*s v-id sto-id) body]))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [sto : Store]) : Result
  (type-case Exp a
    [(numE n) (v*s (numV n) sto)]
    [(idE s) (v*s (lookup s env) sto)]
    [(plusE l r)
     (with [(v-l sto-l) (interp l env sto)]
       (with [(v-r sto-r) (interp r env sto-l)]
         (v*s (num+ v-l v-r) sto-r)))]

    [(multE l r)
     (with [(v-l sto-l) (interp l env sto)]
       (with [(v-r sto-r) (interp r env sto-l)]
         (v*s (num* v-l v-r) sto-r)))]

    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
       (interp body
               (extend-env
                (bind n v-rhs)
                env)
               sto-rhs))]
    [(lamE n body)
     (v*s (closV n body env) sto)]

    [(appE fun arg)
     (with [(v-f sto-f) (interp fun env sto)]
       (with [(v-a sto-a) (interp arg env sto-f)]
         (type-case Value v-f
           [(closV n body c-env)
            (interp body
                    (extend-env
                     (bind n v-a)
                     c-env)
                    sto-a)]
           [else (error 'interp "not a function")])))]

    [(boxE a)
     (with [(v sto-v) (interp a env sto)]
       (let ([l (new-loc sto-v)])
         (v*s (boxV l) 
              (override-store (cell l v) 
                              sto-v))))]
    [(unboxE a)
     (with [(v sto-v) (interp a env sto)]
       (type-case Value v
         [(boxV l) (v*s (fetch l sto-v) 
                        sto-v)]
         [else (error 'interp "not a box")]))]

    ; calling set-box+ helper function here
    [(setboxE bx val)
     (with [(v-b sto-b) (interp bx env sto)]
       (with [(v-v sto-v) (interp val env sto-b)]
         (type-case Value v-b
           [(boxV l)
            (v*s v-v ;removing override function call (originally was going to alter override func)
                 (set-box+ l v-v sto-v))]
           [else (error 'interp "not a box")])))]

     #|DEPRECIATED: [(beginE l r)
     (with [(v-l sto-l) (interp l env sto)]
     (interp r env sto-l))]|#
   
    ; update/extend begin - > interp functions
    ; for Begin E+ WE consider two cases:
    ; list has two cases: empty and (cons item list).
    ;nonempty-list has two different cases: (consitem empty) and (cons item nonempty-list)

    [(beginE+ vals) (let [(len (length vals))]  ; so take these inputs, vals, and vals will be processed by the length of their expr
                     (cond
                       [(= len 1) (interp (first vals) env sto)] ; if the len is 1 print out the singular value in the list
                       [(> len 1) ; if more values are present in the list, the len is larger (meaning multiple expressions), and the values 
                        (with [(v1 sto1) (interp (first vals) env sto)] ; this recursive call evalutes the first value, V1, stored it, and moves on until
                          (interp (beginE+ (rest vals)) env sto1))] ; the last value/expr is returned
                   [else (error 'interp "empty sequence of expressions")]))])) ; otherwise, there arent exprs present

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

;; store and cell operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (type-case (Listof Storage) sto
   [empty 0]
   [(cons c rst-sto) (max (cell-location c)
                          (max-address rst-sto))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (type-case (Listof Storage) sto
   [empty (error 'interp "unallocated location")]
   [(cons c rst-sto) (if (equal? l (cell-location c))
                         (cell-val c)
                         (fetch l rst-sto))]))
; PART 1
; update set box - > store functions
; borrowing the structure of the fetch function in #store but changing -> to "store"(copy and paste)
; becayse we need the old value dropped, we consider the "val" paremeter now 
(define (set-box+ [l : Location] [val : Value] [store : Store])
  (type-case (Listof Storage) store
    [empty (error 'interp "unallocated location")]
    [(cons c rst-sto) (if (equal? l (cell-location c))
                          (cons (cell 1 val) rst-sto)
                          ;(fetch l rst-sto))])) instead of iterating through fetch,
                          ; recurse until until the end of the stores
                          (cons c (set-box+ l val rst-sto)))]))

#|DEPRECIATED part 2
(define (beginE-helper [l : (ListOf Exp)] [env : Env] [store : Store])
(cond
  [(l.len(1) (interp (first l) env store))]
  [(with ]
  [else(unless (= length 0))(error 'empty-input "invalid exps entry")]|#


;all tests ---------------------------------------- (From HW4 pdf)

;(test/exn (interp (set-box+ '()cell-location mt-env mt-store) "unallocated location"))
;(test/exn (set-box+ 2 mt-store) "unallocated location") ; cant figure out how to test this
;(test/exn (set-box+ (numV 0 (mt-store))) "unallocated location")

(test (interp (parse `{let {[b {box 2}]}
                        {begin
                          {set-box! b 2}
                          {unbox b}}})
              mt-env
              mt-store)
      (v*s (numV 2)
           (override-store (cell 1 (numV 2))
                           mt-store)))

(test (interp (parse `{let {[b {box 1}]}
                        {begin
                          {set-box! b 2}
                          {unbox b}}})
              mt-env
              mt-store)
      (v*s (numV 2)
           (override-store (cell 1 (numV 2))
                           mt-store)))


(test (interp (parse `{let {[b {box 1}]}
                        {begin
                          {set-box! b {+ 2 {unbox b}}}
                          {set-box! b {+ 3 {unbox b}}}
                          {set-box! b {+ 4 {unbox b}}}
                          {unbox b}}})
              mt-env
              mt-store)
      (v*s (numV 10)
           (override-store (cell 1 (numV 10))
                           mt-store)))

(test/exn (interp (beginE+ '()) mt-env mt-store) "empty sequence of expressions")
(test (parse `x) ; note: backquote instead of normal quote
      (idE 'x))

(test (max-address mt-store)
      0)
(test (max-address (override-store (cell 2 (numV 9))
                                   mt-store))
      2)
  
(test (fetch 2 (override-store (cell 2 (numV 9))
                               mt-store))
      (numV 9))
(test (fetch 2 (override-store (cell 2 (numV 10))
                               (override-store (cell 2 (numV 9))
                                               mt-store)))
      (numV 10))
(test (fetch 3 (override-store (cell 2 (numV 10))
                               (override-store (cell 3 (numV 9))
                                               mt-store)))
      (numV 9))
(test/exn (fetch 2 mt-store)
          "unallocated location")


(test (interp (parse `{let {[b {box 1}]} 
                        {begin 
                          {set-box! b 2} 
                          {unbox b}}}) 
              mt-env 
              mt-store) 
      (v*s (numV 2) 
           (override-store (cell 1 (numV 2)) 
                           mt-store)))

(test/exn (fetch 2 mt-store)
          "unallocated location")
(test (parse `2)
      (numE 2))
(test (parse `x) 
      (idE 'x))
(test (parse `{+ 2 1})
      (plusE (numE 2) (numE 1)))
(test (parse `{* 3 4})
      (multE (numE 3) (numE 4)))
(test (parse `{+ {* 3 4} 8})
      (plusE (multE (numE 3) (numE 4))
             (numE 8)))
(test (parse `{let {[x {+ 1 2}]}
                y})
      (letE 'x (plusE (numE 1) (numE 2))
            (idE 'y)))
(test (parse `{lambda {x} 9})
      (lamE 'x (numE 9)))
(test (parse `{double 9})
      (appE (idE 'double) (numE 9)))
(test (parse `{box 0})
      (boxE (numE 0)))
(test (parse `{unbox b})
      (unboxE (idE 'b)))
(test (parse `{set-box! b 0})
      (setboxE (idE 'b) (numE 0)))
(test (parse `{begin 1 2})
      (beginE+ (list (numE 1) (numE 2))))
(test/exn (parse `{{+ 1 2}})
          "invalid input")
(test (interp (parse `2) mt-env mt-store)
      (v*s (numV 2) 
           mt-store))
(test/exn (interp (parse `x) mt-env mt-store)
          "free variable")
(test (interp (parse `x) 
              (extend-env (bind 'x (numV 9)) mt-env)
              mt-store)
      (v*s (numV 9)
           mt-store))
(test (interp (parse `{+ 2 1}) mt-env mt-store)
      (v*s (numV 3)
           mt-store))
(test (interp (parse `{* 2 1}) mt-env mt-store)
      (v*s (numV 2)
           mt-store))
(test (interp (parse `{+ {* 2 3} {+ 5 8}})
              mt-env
              mt-store)
      (v*s (numV 19)
           mt-store))
(test (interp (parse `{lambda {x} {+ x x}})
              mt-env
              mt-store)
      (v*s (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
           mt-store))
(test (interp (parse `{let {[x 5]}
                        {+ x x}})
              mt-env
              mt-store)
      (v*s (numV 10)
           mt-store))
(test (interp (parse `{let {[x 5]}
                        {let {[x {+ 1 x}]}
                          {+ x x}}})
              mt-env
              mt-store)
      (v*s (numV 12)
           mt-store))
(test (interp (parse `{let {[x 5]}
                        {let {[y 6]}
                          x}})
              mt-env
              mt-store)
      (v*s (numV 5)
           mt-store))
(test (interp (parse `{{lambda {x} {+ x x}} 8})
              mt-env
              mt-store)
      (v*s (numV 16)
           mt-store))
(test (interp (parse `{box 5})
              mt-env
              mt-store)
      (v*s (boxV 1)
           (override-store (cell 1 (numV 5))
                           mt-store)))
(test (interp (parse `{unbox {box 5}})
              mt-env
              mt-store)
      (v*s (numV 5)
           (override-store (cell 1 (numV 5))
                           mt-store)))
(test (interp (parse `{set-box! {box 5} 6})
              mt-env
              mt-store)
      (v*s (numV 6)
           (override-store (cell 1 (numV 6))
                           mt-store)))

(test (interp (parse `{begin 1 2})
              mt-env
              mt-store)
      (v*s (numV 2)
           mt-store))

(test (interp (parse `{begin 1 2 3})
              mt-env
              mt-store)
      (v*s (numV 3)
           mt-store))

(test (interp (parse `{unbox {begin {box 5} {box 6}}})
              mt-env
              mt-store)
      (v*s (numV 6)
           (override-store (cell 2 (numV 6))
                           (override-store (cell 1 (numV 5))
                                           mt-store))))

(test (interp (parse `{let {[b (box 5)]}
                        {begin
                          {set-box! b 6}
                          {unbox b}}})
              mt-env
              mt-store)
      (v*s (numV 6)
           (override-store (cell 1 (numV 6))
                           mt-store)))
  
(test/exn (interp (parse `{1 2}) mt-env mt-store)
          "not a function")

(test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env mt-store)
          "not a number")

(test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                            {let {[y 5]}
                              {bad 2}}})
                  mt-env
                  mt-store)
          "free variable")

(test/exn (interp (parse `{unbox 1}) mt-env mt-store)
          "not a box")

(test/exn (interp (parse `{set-box! 1 1}) mt-env mt-store)
          "not a box")

(test (interp (parse `{let {[b {box 1}]} 
                        {begin 
                          {set-box! b {+ 2 {unbox b}}} 
                          {set-box! b {+ 3 {unbox b}}} 
                          {set-box! b {+ 4 {unbox b}}} 
                          {unbox b}}}) 
              mt-env 
              mt-store) 
      (v*s (numV 10) 
           (override-store (cell 1 (numV 10)) 
                           mt-store)))