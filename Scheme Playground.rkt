#lang racket
(require (lib "eopl.ss" "eopl"))
; collection of list editing and general scheme playground code

;make as many copies of whatyouwant, howmanytimes you want it
(define duple
	(lambda (howmanytimes whatyouwant)
		(if (zero? howmanytimes)
			'()
			(cons whatyouwant (duple (- howmanytimes 1) whatyouwant)))))

(duple 10 10)
(duple 1 10)
(duple 2 10)

;reverse tuples
(define invert
   (lambda (lst)
     (map (lambda (x) (list (cadr x) (car x)))
          lst)))

(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '((x y) (y x) (f g) (x f)))

; put parenthesis around each piece of the list
(define down
  (lambda (lst)
    (map (lambda (x) (list x))
         lst)))

(down '(1 2 3))
(down '(3 2 1))
(down '(and test stuff))

; replace s1 with all instances of s2, and all s2 replaced by s1
(define swapper
  (lambda (s1 s2 slist); function call (parameters)
    (map (lambda (sexp); 
           (if (symbol? sexp)
               (if (eqv? sexp s1)
                   s2
                   (if (eqv? sexp s2)
                       s1
                       sexp))
               (swapper s1 s2 sexp)))
         slist)))

(swapper 'a 'd '(a b (c) d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))

;replace the index (n) w/ x.
(define list-set
  (lambda (lst n x)
    (if (zero? n)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)

; count # of s in sexp/slist
(define count-occurrences-sexp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? sexp s) 1 0)
        (count-occurrences s sexp))))

(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-sexp s (car slist))
           (count-occurrences s (cdr slist))))))

(count-occurrences 'x '((f  x)  y  (((x  z) x))))
(count-occurrences 'x '((f  x)  y  (((x  z) () x))))
(count-occurrences 'w '((f  x)  y  (((x  z) x))))

;rough syntax for cases in scheme
(case (+ 5 7)
  [(0 12) 'this]
  [(1 2) 'that])

;(Everything is based on a list, for example:)
'()
'(1 2 3 4)
;((+ 3 5) evaluates to)
(+ 3 5)
;(because of prefix notation)

;(car of (1 2 3) gets the first ele in a list)
(car '(1 2 3))

;(cdr gets the rest of the list (1 2 3))
(cdr '(1 2 3))
;()

;(cons 1 (cons 2 '()) builds list from the front)
(cons 1 (cons 2 '()))

; structure of a command is (operator operand1 operand2 ...)
;
;defining a list
;
;
;(define lst '(1 2 3 4) set's lst to the given quoted list)
(define lst '(1 2 3 4))
lst
;(procedures are like functions!)
;(use (define NAME (args)) to declare a global function)
;(use "define incr (lambda (x) (+ x 1))" to declare an example function to increment by 1)
(define incr (lambda (x) (+ x 1)))
;(incr 5)
(incr 5)
;(define incr2 (lambda (x) (+ x 2)))
(define incr2 (lambda (x) (+ x 2)))
;(incr2 1)
(incr2 1)
;(the first thing in a list is applied to the following items in the list, for example (+ 1 2 3 4) gives)
(+ 1 2 3 4)
;((+ (car lst) (cadr lst) (caddr lst) (cadddr lst)) adds up the list just the same)
(+ (car lst) (cadr lst) (caddr lst) (cadddr lst))
;((list 1 2 3 4) quotes a defined list. (list a b c) does not work since it is not defined (but it could be))
(list 1 2 3 4) ;(list a b c)
;( IF SYNTAX == (if (boolean) (boolean true) (else boolen false)))
;( if (< 0 5) 'this 'that gives 'this since the first statement is true, thus the first option is chosen)
(if (< 0 5) 'this 'that)
;
;first procedures
;when defining predicates always use a "?"
;
(define factorial (lambda (n)
                    (if (< n 2)
                        1
                        (* n (factorial (- n 1))))))
;(factorial is a procedure which gives the factorial for one arg)
'(factorial 4 and 5)
(factorial 4)
(factorial 5)

;(length is a native function to get length of list (lst))
(length lst)

;(list-length is a new function which gives 0 as length if list is null, else length of 1 + cdr of lst. this is a common way to traverse a list in scheme!)

(define list-length (lambda (lst)
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst))))))

(define lstb '(1 2))

'(list-length of a null list, and more!)
(list-length lst)
(list-length lstb)
(list-length '())

(define sumlist (lambda (lst)
                  (if (null? lst)
                      0
                      (+ (car lst) (sumlist (cdr lst))))))

;(sumlist sums/adds the first element to the following for the entire list)
(sumlist lst)

;????subtract a list
;this applies - in a weird way 
(define subt (lambda (lst)
               (if (null? lst)
                   0
                   (- (car lst) (subt (cdr lst))))))

'(subt subtracts a list)
(subt lst)
(subt '())

(define prodlist (lambda (lst)
  (if (null? lst)
      1
      (* (car lst) (prodlist (cdr lst))))))

'(prodlist multiplies the elements in order)
(prodlist lst)

;functions and abstraction
;writing similar structure for functions allows a variance of the paramaters which correspond to the output (what the function does eg: add, mult, /...))

(define applylst (lambda (lst base f)
                   ;(write lst)
                   
  (if (null? lst)
      base
      (f (car lst) (applylst (cdr lst) base f)))))

; redlist here is used to add the list together
(applylst '(1 2 3 4) 0 +)

; here it multiplies
(applylst '(1 2 3 4) 1 *)
'(  _   _  )
'(  _   _  )
'("double quotes to print a string")
'("squidward" "spongebob" "patrick" "mr. krabs")


; initially this parameter set of (lst base) == (lst 0 -) was meant to subtract the ele one by one
; instead it applies a * -1 to the cdr of the list and then subtracts
; now it just is mostly useless but alternates w/ 
; 
(applylst '() 0 -)
(applylst '(1) 0 -)
(applylst '(1 2) 0 -) ; if the function is only applied to 2 args, it subtracts. 
(applylst '(1 2 3) 0 -)
(applylst '(1 2 3 4) 0 -)
(applylst '(1 2 3 4 5) 0 -)
(applylst '(1 2 3 4 5 6) 0 -)
(applylst '(1 2 3 4 5 6 7) 0 -)
(applylst '(1 2 3 4 5 6 7 8) 0 -)
(applylst '(1 2 3 4 5 6 733333 8) 0 -)
'()

;divide (but not conquering)
(applylst '(10 2) 1 /)
(applylst '(100 10 2) 1 /) ;this doesn't work to divide, only 2 args work.

'()
;
(applylst '(10 9) 0 -)

; addition using lambda 
(applylst '(1 2 3 4) 0 (lambda (elem result) (+ elem result)))
(applylst lst 0 +)

; length of list using applylst
(applylst '(1 2 3 4 () ()) 0 (lambda (elem result) (+ 1 result)))

; return original list with all elements modified in the same way
;change the logic inbetween cons and result in parenthesis
(applylst '(1 2 3 4) '() (lambda (elem result) (cons (* 2 elem) result)))
(applylst '(1 2 3 4) '() (lambda (elem result) (cons (+ 10 elem) result)))
(applylst '(1 2 3 4) '() (lambda (elem result) (cons (- 1 elem) result)))

;raise a number to power 2 
(define (square x) (* x x))
(square 4)
(+ (square 2) (square 3))

;simple control flow w/ if statement
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(abs -3)
(abs 3)

; using cons and define
(cons 1 2)
(cons (cons 1 2) 3)
(define foo (cons 1 2))
foo
(cons 1 null)
(car (cons 1 null))
(cdr (cons 1 null))
(cons 1 (cons 2 (cons 3 (cons 4 null))))

;check equality of two lists
(equal? (list 1 2 3 4) lst)

;use list ref to get a specific postion in a list index first pos at 0
(list-ref lst 0)
lst
(list-ref (list "mario" "luigi" "wario") 0)
(list-ref (list "mario" "luigi" "wario") 1)
(list-ref (list "mario" "luigi" "wario") 2)

;map applies the given function to each ele in the given list, and returns list
(define baz (list 1 2 3))
(define (double x) (* x 2))
(map double baz)
(define (double-all x) (map double x))
(double-all baz)
(double-all lst)

;implementing map
(define (mmap fn lst)
  (if (null? lst)
      null ;reaching here is the end of the lst
      (cons (fn (car lst)) (mmap fn (cdr lst)))))

(mmap double baz)

;adding a list using built in foldr
(foldr + 0 lst)
(foldr + 2000 lst)

; remove first instance of ele in list
;hw 2 question 3
;returns original list if no s in los
(define remove-first (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))
; other version of the same procedure??
; this returns an empty list if s not in los
(define remove-firstother (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (remove-firstother s (cdr los))))))
  
'()
'()
;hw 2 q 4 0 - remove all instances of ele in a list
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

(remove-first 1 '(2 1 1 2 1 3 4))
(remove-first 1 '(2 2 2 2 2))
(remove-firstother 1 '(2 2 2 2))
;(remove-first 'a '('a 'x 'b 'd 'e 'a))
(remove-firstother 1 '(1 1 2 1 3 4))
(remove 1 '(1 1 2 3 4))

; reverse a list
; snoc is reverse cons
(define (rev lis)
  (define (snoc elem lis)
    (if (null? lis)
        (cons elem lis)
        (cons (car lis) (snoc elem (cdr lis)))))
  (if (null? lis)
      lis
      (snoc (car lis) (rev (cdr lis)))))

(rev lst)
(define (isSortedAscendingly lst)
      (cond ((null? lst) #t)
            ((eq? (length lst) 1) #t)
      ((>= (car (cdr lst)) (car lst))
        (isSortedAscendingly (cdr lst)))
      (else #f))
     )

(isSortedAscendingly lst)
(isSortedAscendingly '(1 2 4 3))
(isSortedAscendingly '(1 2 3 4 5 6))
'()
(isSortedAscendingly '(1 4 8))
(isSortedAscendingly '(1 8 3))
(isSortedAscendingly '(1 8 8))
(isSortedAscendingly '(1))
(isSortedAscendingly '())

;binary tree
(define-datatype bintree bintree? 
   (leaf-node 
      (num integer?))
   (interior-node
      (key symbol?)
      (left bintree?)
      (right bintree?))) 

 (define bintree-to-list
  (lambda (tree) 
    (cases bintree tree
      [interior-node (key left right) ;each node has the item, left & right tree
                     (list 'interior-node key (bintree-to-list left) (bintree-to-list right))]
      [leaf-node (num) (list 'leaf-node num)])))

(define (max-interior tree)
  (cases bintree tree
    [leaf-node (num) (eopl:error 'max-interior "~s is not an interior node" tree)]
    [interior-node (key left right)
                   (display (car (max-interior-helper tree)))]))

;calc-tree recursively adds all nodes of a tree to find the best/highest value
(define (calc-tree tree)
  (cases bintree tree
    [leaf-node (num) num]
    [interior-node (key left right)
                   (+ (calc-tree left) (calc-tree right))])) 


(define (leaf-node? tree)
  (cases bintree tree
    [leaf-node (num) #t]
    [interior-node (key left right) #f]))
  
(define (max-interior-helper tree)
  (cases bintree tree
    [leaf-node (num) (list 'invalid-tree num)]
    [interior-node (key left right)
                   (if (and (leaf-node? left) (leaf-node? right))
                       (list key (calc-tree tree))
                       (if (and (>= (calc-tree tree) (cadr (max-interior-helper left))) (>= (calc-tree tree) (cadr (max-interior-helper right))))
                           (list key (calc-tree tree))
                           (if (>= (cadr (max-interior-helper left)) (cadr (max-interior-helper right)))
                               (max-interior-helper left)
                               (max-interior-helper right))))]))



(define tree-1
    (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
    (interior-node 'bar  (leaf-node -1) tree-1))
(define tree-3
    (interior-node 'baz  tree-2 (leaf-node 1)))

(max-interior tree-1)
(max-interior tree-2)
(max-interior tree-3)

