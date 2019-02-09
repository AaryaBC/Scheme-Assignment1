;QUESTION 1

;guile 2.0.11
;Reverse a list

#|
function to reverse a list; checks if the list is empty, if it is, returns the empty list
else, calls the reverse function with first element appended in the end
|#
(define (reverse-helper lst)
  (if (null? lst)
     '()
     (append (reverse-helper (cdr lst)) (list (car lst)))
  )
)

#|
function to recursively go inside the nested lists and reverse them using the above function
first, it checks if the element is a list. if it is, it passes the list to reverse-general 
function again until it is a list with no nesting. Now, this list is passed to
the above function <reverse-helper> which reverses the list.

These lists all append together
|#
(define (reverse-general l)
  (map (lambda (x) (if (list? x) (reverse-general x) x)) 
       (reverse-helper l)
  )
)

#|
TEST CASES
|#
(display (reverse-general '() ))
(display "\n")
(display (reverse-general '(a b c) ))
(display "\n")
(display (reverse-general '(a b ()) ))
(display "\n")
(display (reverse-general '((a b c)) ))
(display "\n")
(display (reverse-general '((a b c) (d e f)) ))
(display "\n")
(display (reverse-general '(a (b c) ((d e) f) g) ))
(display "\n")
(display (reverse-general '(1 (2 3) (4 (a (b (c d)))))))
(display "\n")



;;;;;;;;;;;;;;;;;;;;;;;;;;;

;QUESTION 2

;guile 2.0.11

#|
conditions: 
1st - checks whether the list is empty, if so --> returns 0
2nd - checks whether the current first element is a number, if so --> returns first element + function call excluding the first element
3rd - if not a number, returns the function without that element and carry-on
|#
(define (sum-up-numbers-simple L)
  (cond ((null? L) 0)
        ( (number? (car L)) (+ (car L) (sum-up-numbers-simple (cdr L))))
        ( (sum-up-numbers-simple (cdr L)) )
  )
)

;TEST CASES
(display (sum-up-numbers-simple '() ))
(display "\n")
(display (sum-up-numbers-simple '(100 200) ))
(display "\n")
(display (sum-up-numbers-simple '(a b c) ))
(display "\n")
(display (sum-up-numbers-simple '(100 a) ))
(display "\n")
(display (sum-up-numbers-simple '(a 100) ))
(display "\n")
(display (sum-up-numbers-simple '(a 100 b 200 c 300 d) ))
(display "\n")
(display (sum-up-numbers-simple '(()) ))
(display "\n")
(display (sum-up-numbers-simple '((100)) ))
(display "\n")
(display (sum-up-numbers-simple '(100 (200)) ))
(display "\n")
(display (sum-up-numbers-simple '(a 100 b (200) c 300 d) ))
(display "\n")
