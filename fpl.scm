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
(define (reverse-general L)
  (map (lambda (x) (if (list? x) (reverse-general x) x)) 
       (reverse-helper L)
  )
)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;QUESTION 3

;guile 2.0.11

#|
conditions: 
1st - checks whether the list is empty, if so --> returns 0
2nd - checks whether the current first element is a number, if so --> returns first element + function call excluding the first element
3rd - checks if the current first element is a list, if so --> recursively goes inside the nested list
4th - if the current first element is not a number or a list, just skips that element
|#

(define (sum-up-numbers-general L)
  (cond ((null? L) 0)
        ((number? (car L)) (+ (car L) (sum-up-numbers-general (cdr L))))
        ((list? (car L)) (+ (sum-up-numbers-general (car L)) (sum-up-numbers-general (cdr L))))
        ( (sum-up-numbers-general (cdr L)) )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;QUESTION 4

;guile 2.0.11

;Function to extract only the numbers from a given list
(define (extract-numbers list1)
  (cond ( (null? list1) '() )
        ( (number? (car list1)) (cons (car list1) (extract-numbers (cdr list1))) )
        ( else (extract-numbers (cdr list1)) )
  )
)

;function to get the minimum number from a list --> Used to get the min of L2 when needed
(define (get-min given-list)
  (cond ( (null? (cdr given-list)) (car given-list) )
        ( (< (car given-list) (get-min (cdr given-list))) (car given-list) )
        ( (get-min (cdr given-list)) )
  )
)

;function to get the max of a list --> Used as starter value for the element we need to return when needed
(define (get-max given-list)
  (cond ( (null? (cdr given-list)) (car given-list) )
        ( (> (car given-list) (get-max (cdr given-list))) (car given-list) )
        ( (get-max (cdr given-list)) )
  )
)

;function to get the min from L1 which is larger than min from L2
(define (get-min-2 list1 min1 min2)
  (cond ((null? list1) min2)
        ((and (< min1 (car list1)) (< (car list1) min2)) (get-min-2 (cdr list1) min1 (car list1)))
        (else (get-min-2 (cdr list1) min1 min2))
  )
)
  
#|
conditions: 
1st - checks whether the list L1 doesn't have any numbers --> returns FALSE in that case
2nd - checks if L1 contains numbers <condition fulfilled above> but L2 doesn't contain numbers, returns the minimum from L1
3rd - if both conditions above fail, then extracts the min from L1 which is larger than min from L2
    - checks if that minimum from L1 is less than or equal to minimum from L2 --> returns false in that case
4th - if all above conditions are false, it means that there is an element in L1 such that it's the
    - it's the smallest element from L1 bigger than the minimum of L2. --> Returns that element
|#  
(define (min-above-min L1 L2)
  (cond ((null? (extract-numbers L1)) #f)
        ((null? (extract-numbers L2)) (get-min (extract-numbers L1)))
        ( (<= (get-min-2 (extract-numbers L1) (get-min (extract-numbers L2)) (get-max (extract-numbers L1))) (get-min (extract-numbers L2)) ) #f)
        (else (get-min-2 (extract-numbers L1) (get-min (extract-numbers L2)) (get-max (extract-numbers L1))))
  )
)

;TEST CASES Q1
(display "QUESTION 1: REVERSE GENERAL \n")
(display "---------------------------------------\n")
(display "expected: () \t\t\t\t Result: ")
(display (reverse-general '() ))
(display "\n")
(display "expected: (c b a) \t\t\t Result: ")
(display (reverse-general '(a b c) ))
(display "\n")
(display "expected: (() b a) \t\t\t Result: ")
(display (reverse-general '(a b ()) ))
(display "\n")
(display "expected: ((c b a)) \t\t\t Result: ")
(display (reverse-general '((a b c)) ))
(display "\n")
(display "expected: ((f e d) (c b a)) \t\t Result: ")
(display (reverse-general '((a b c) (d e f)) ))
(display "\n")
(display "expected: (g (f (e d)) (c b) a) \t Result: ")
(display (reverse-general '(a (b c) ((d e) f) g) ))
(display "\n")
(display "expected: (((((d c) b) a) 4) (3 2) 1) \t Result: ")
(display (reverse-general '(1 (2 3) (4 (a (b (c d)))))))
(display "\n\n\n")

;TEST CASES Q2
(display "QUESTION 2: SUM OF LISTS SIMPLE \n")
(display "---------------------------------------\n")
(display "expected result: 0 \t Result: ")
(display (sum-up-numbers-simple '() ))
(display (if (= 0 (sum-up-numbers-simple '() )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 300 \t Result: ")
(display (sum-up-numbers-simple '(100 200) ))
(display (if (= 300 (sum-up-numbers-simple '(100 200) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 0 \t Result: ")
(display (sum-up-numbers-simple '(a b c) ))
(display (if (= 0 (sum-up-numbers-simple '(a b c) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 100 \t Result: ")
(display (sum-up-numbers-simple '(100 a) ))
(display (if (= 100 (sum-up-numbers-simple '(100 a) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 100 \t Result: ")
(display (sum-up-numbers-simple '(a 100) ))
(display (if (= 100 (sum-up-numbers-simple '(a 100) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 600 \t Result: ")
(display (sum-up-numbers-simple '(a 100 b 200 c 300 d) ))
(display (if (= 600 (sum-up-numbers-simple '(a 100 b 200 c 300 d) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 0 \t Result: ")
(display (sum-up-numbers-simple '(()) ))
(display (if (= 0 (sum-up-numbers-simple '(()) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 0 \t Result: ")
(display (sum-up-numbers-simple '((100)) ))
(display (if (= 0 (sum-up-numbers-simple '((100)) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 100 \t Result: ")
(display (sum-up-numbers-simple '(100 (200)) ))
(display (if (= 100 (sum-up-numbers-simple '(100 (200)) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 400 \t Result: ")
(display (sum-up-numbers-simple '(a 100 b (200) c 300 d) ))
(display (if (= 400 (sum-up-numbers-simple '(a 100 b (200) c 300 d) )) "\tPASS" "\tFAIL"))
(display "\n\n\n")

;TEST CASES Q3
(display "QUESTION 3: SUM OF LISTS GENERAL \n")
(display "---------------------------------------\n")
(display "expected result: 0 \t Result: ")
(display (sum-up-numbers-general '() ))
(display (if (= 0 (sum-up-numbers-general '() )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 100 \t Result: ")
(display (sum-up-numbers-general '(100) ))
(display (if (= 100 (sum-up-numbers-general '(100) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 300 \t Result: ")
(display (sum-up-numbers-general '(100 200) ))
(display (if (= 300 (sum-up-numbers-general '(100 200) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 0 \t Result: ")
(display (sum-up-numbers-general '(a) ))
(display (if (= 0 (sum-up-numbers-general '(a) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 600 \t Result: ")
(display (sum-up-numbers-general '(a 100 b 200 c 300 d) ))
(display (if (= 600 (sum-up-numbers-general '(a 100 b 200 c 300 d) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 0 \t Result: ")
(display (sum-up-numbers-general '(()) ))
(display (if (= 0 (sum-up-numbers-general '(()) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 100 \t Result: ")
(display (sum-up-numbers-general '((100)) ))
(display (if (= 100 (sum-up-numbers-general '((100)) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 300 \t Result: ")
(display (sum-up-numbers-general '(100 (200)) ))
(display (if (= 300 (sum-up-numbers-general '(100 (200)) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 600 \t Result: ")
(display (sum-up-numbers-general '(a 100 b (200) c 300 d) ))
(display (if (= 600 (sum-up-numbers-general '(a 100 b (200) c 300 d) )) "\tPASS" "\tFAIL"))
(display "\n")
(display "expected result: 600 \t Result: ")
(display (sum-up-numbers-general '(a 100 ((b ((200) c)) 300 d)) ))
(display (if (= 600 (sum-up-numbers-general '(a 100 ((b ((200) c)) 300 d)) )) "\tPASS" "\tFAIL"))
(display "\n\n\n")

;TEST CASES Q4
(display "QUESTION 4: Min Above Min \n")
(display "---------------------------------------\n")
(display "expected result: #f \t Result: ")
(display(min-above-min '() '(a 100 b 200 c 300 d)))
(display (if (eq? #f (min-above-min '() '(a 100 b 200 c 300 d))) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: 100 \t Result: ")
(display(min-above-min '(100) '()))
(display (if (eq? 100 (min-above-min '(100) '())) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: 100 \t Result: ")
(display(min-above-min '(a 200 b 100 c 300 d) '()))
(display (if (eq? 100 (min-above-min '(a 200 b 100 c 300 d) '())) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: #f \t Result: ")
(display(min-above-min '(a) '()))
(display (if (eq? #f (min-above-min '(a) '())) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: #f \t Result: ")
(display(min-above-min '(a) '(a 200 b 300 c 100 d)))
(display (if (eq? #f (min-above-min '(a) '(a 200 b 300 c 100 d))) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: #f \t Result: ")
(display(min-above-min '(a b c) '(a 200 b 300 c 100 d)))
(display (if (eq? #f (min-above-min '(a b c) '(a 200 b 300 c 100 d))) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: 200 \t Result: ")
(display(min-above-min '(a 200) '(a 200 b 300 c 100 d)))
(display (if (eq? 200 (min-above-min '(a 200) '(a 200 b 300 c 100 d))) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: #f \t Result: ")
(display(min-above-min '(a 100) '(a 200 b 300 c 100 d)))
(display (if (eq? #f (min-above-min '(a 100) '(a 200 b 300 c 100 d))) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: 200 \t Result: ")
(display(min-above-min '(100 200 300) '(300 100 200)))
(display (if (eq? 200 (min-above-min '(100 200 300) '(300 100 200))) "\tPASS" "\tFAIL"))
(display "\n")

(display "expected result: 200 \t Result: ")
(display(min-above-min '(a 300 b 100 c 200 d) '(a 200 b 300 c 100 d)))
(display (if (eq? 200 (min-above-min '(a 300 b 100 c 200 d) '(a 200 b 300 c 100 d))) "\tPASS" "\tFAIL"))
(display "\n")
