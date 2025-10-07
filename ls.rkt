#lang scheme
(require racket/trace)
(require racket/format)


; chapter 1
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


; chapter 2
(define lat? ; is the argument a list of atoms? - we first check for atom? if its true we break
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
          

; chapter 3 [28.10.24]
(trace-define rember 
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat) ;cons first calls rember only cons'ing in the unwinding. run the trace
                          (rember a
                                  (cdr lat)))))))))
; call as: (rember 'and '(bacon egg and cheese))
; (rember 'and '(bacon and egg and cheese))

; NEXT pg 39 - unwinding the recursion to cons the result list

(trace-define rember2
  (lambda (a lat)
    (cond
      ((null? lat) (quote())) ; '()
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember2 a (cdr lat)))))))
; > (rember2 'and '(bacon egg and cheese)) - initial call
; >(rember2 and (bacon egg and cheese))
; > (rember2 and (egg and cheese)) - bacon is saved for consing - rember2 called with cdr of (bacon egg and cheese)
; > >(rember2 and (and cheese)) - egg is saved for consing - rember2 called with cdr of (and cheese)
; < <(cheese) - cheese is returned
; < (egg cheese) - egg cons'd to it
; <(bacon egg cheese) - bacon cons'd to it

;p43
(trace-define firsts
  (lambda (l) ; if l is not null, it must contain only non null lists and no atoms
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l))))))) ; cons the first element of the first element which is a list

(trace-define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

; p48
(trace-define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
       ((eq? (car lat) old)
        (cons old (cons new (cdr lat)))) ; if we find old we need to insert new right after it along with (cdr lat)
       (else (cons (car lat) (insertR new old (cdr lat)))))))))

; p51
(trace-define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
       ((eq? (car lat) old)
        (cons new lat)) ; lat is being cdr'd in the next line - 
       (else (cons (car lat) (insertL new old (cdr lat)))))))))

;p51 subst
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old)
               (cons new (cdr lat))) ; lat is being cdr'd in the next line -
              (else (cons (car lat) (subst new old (cdr lat)))))))))

;p52 subst2
; replaces either the first occurrence of o1 or the first occurrence of o2 by new
(define subst2
  (lambda (new o1 o2 lat)
     (cond
       ((null? lat) '())
       (else
        (cond
          ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
            (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

;p53 - multirember - remove all occurences of a
(trace-define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
       (cond
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))))

;p56 multiinsertR
(trace-define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old)
          (cons old (cons new (multiinsertR new old (cdr lat)))))
         (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))


;p57
(trace-define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
       ((eq? (car lat) old)
        (cons new
              (cons old
                    (multiinsertL new old
                                  (cdr lat)))))
       (else (cons (car lat)
                   (multiinsertL new old
                                 (cdr lat)))))))))


;p57
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old)
               (cons new (multisubst new old (cdr lat)))) ; lat is being cdr'd in the next line -
              (else (cons (car lat) (multisubst new old (cdr lat)))))))))

; Chap 4

;p60
(define +
  (lambda (n m)
    (cond
      ((zero? m) n)
    (else (add1 (+ n (sub1 m)))))))

;p61
(define -
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (- n (sub1 m)))))))

;p64
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (+ (car tup) (addtup (cdr tup)))))))

;p65
(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (+ n (* n (sub1 m)))))))

;p65
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (+ n (x n (sub1 m)))))))

;67
(define tup+
  (lambda (tup1 tup2) ; expects that both tups == length
    (cond
      ((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2) ; support unequal length tups!
      ((null? tup2) tup1)
      (else
       (cons
        (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;NEXT p71 - simplified tup+
(define tup++
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2) ; support unequal length tups!
      ((null? tup2) tup1)
      (else
       (cons
        (+ (car tup1) (car tup2)) (tup++ (cdr tup1) (cdr tup2)))))))

;p72
(trace-define gt
  (lambda (n m) ; n > m ?
    (cond
      ((zero? n) #f) ; order matters to handle the case where they are ==
      ((zero? m) #t) ; the one that reaches zero first is the lesser
      (else
       (gt (sub1 n) (sub1 m))))))

;p73
(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (lt (sub1 n) (sub1 m))))))
; NEXT p74
(trace-define equals
  (lambda (n m)
    (cond
      ((zero? m)
       (zero? n))
      ((zero? n) #f)
      (else
       (equals (sub1 n) (sub1 m))))))

; rewrite equals in terms of gt and lt
(define equals2
  (lambda (n m)
    (cond
      ((zero? m)
       (zero? n))
      ((zero? n) #f)
      (else
       (and (not (gt n m)) (not (lt n m)))))))

; much better:
(define equals3
  (lambda (n m)
    (cond
      ((gt n m) #f)
      ((lt n m) #f)
    (else #t))))

;p74
(define ^  
  (lambda (n m)
    (cond
      ((zero? m) 1)
       (else (* n (^ n (sub1 m)))))))


;p74
(define divide
  (lambda (n m)
    (cond
      ((lt n m) 0)
      (else (add1 (divide (- n m) m))))))

;p76
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (+ 1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
       (else
        (pick (sub1 n) (cdr lat))))))

(trace-define rempick-old
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;p77
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat)) (no-nums (cdr lat)))
         (else
          (cons (car lat) (no-nums (cdr lat)))))))))


;p78
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else
          (all-nums (cdr lat))))))))


(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f )
      (else (eq? a1 a2)))))

; TODO check this one and the one? variants
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? (car lat) a)
          (add1 (occur a (cdr lat))))
         (else
          (occur a (cdr lat))))))))


(define one?
  (lambda (n)
    (= n 1)))

;p79
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;;;;; Chap 5

; modify original rember to remove all a's
(define rember-all
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (rember-all (car lat) (cdr lat)))
       (else (cons (car lat)
                  (rember-all a (cdr lat)))))))


;p81 - 12.11.24 - half cheated on this one :-0
(trace-define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          (rember* a (cdr l))) ; if it is eq? will suffice - skip the (car l)
         (else
          (cons (car l) (rember* a (cdr l))))))
       (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

; (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
; (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

;p82

(trace-define insertR*
  (trace-lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old
                (cons new
                      (insertR* new old
                                (cdr l)))))
         (else (cons (car l)
                     (insertR* new old
                               (cdr l))))))
      (else (cons (insertR* new old
                            (car l))
                  (insertR* new old
                            (cdr l)))))))

;(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

;p85

(trace-define occur* ; counts the number of a's in l
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else
          (occur* a (cdr l)
           ))))
      (else
       (+ (occur* a (car l)) (occur* a (cdr l)))))))

;(occur* 'banana '((banana) (split (((banana ice))) (cream (banana)) (sherbet)) (banana) (bread) (banana brandy)))


;p85

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

;(subst* 'orange 'banana  '((banana) (split (((banana ice))) (cream (banana)) (sherbet)) (banana) (bread) (banana brandy)))

;p86
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (cons old
                      (insertL* new old
                                (cdr l)))))
         (else (cons (car l)
                     (insertL* new old
                               (cdr l))))))
      (else (cons (insertL* new old
                            (car l))
                  (insertL* new old
                            (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else
       (or (member* a (car l))
           (member* a (cdr l)))))))
              

;p88
(define leftmost
  (lambda (l)
    (cond
      (atom? (car l) (car l))
      (else (leftmost (car l))))))


;p91

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t) ; base case;
      ; if we made it here: we only know that l1 may be null - l2 is definitely not null
      ((and (null? l1) (atom? (car l2))) #f) ; An empty list cannot be equal to a non-empty list
      ; at this point l1 may be not null - we need to check it directly
      ((null? l1) #f)
      ; now we know that l1 is not null so we check nullity of l2 directly
      ((and (atom? (car l1)) (null? l2)) #f)
      ; if we are here then they are def both not null
      ((and (atom? (car l1)) (atom? (car l2))) ; are they both atoms
       ;are they both equal numbers?
       (and (eqan? (car l1) (car l2)) ; if so recurse!
            (eqlist? (cdr l1) (cdr l2))))
      ; now we need to check that we aren't dealing with sublists
      ; first list starts with an atom but second does not
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      ; we have two sublists !
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))


(trace-define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and (eqlist2? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2)))))))
      
;p93
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

;p93 rewrite eqlist? using equal?

(define eqlist3?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

;p94
(define rember4
  (lambda (s l)
    (cond
      ((null? l ) '())
      ((atom? (car l))
       (cond
         ((equal? (car l) s) (cdr l))
         (else
          (cons (car l) (rember4 (cdr l))))))
      (else
       (cond
         ((equal? (car l) s) (cdr l))
         (else
          (cons (car l) (rember4 s (cdr l)))))))))

;simplify rember4
(define rember5
  (lambda (s l)
    (cond
      ((null? l) '())
      (else
       (cond
         ((equal? (car l) s) (cdr l))
         (else
          (cons (car l) (rember5 s (cdr l)))))))))


(define rember6 ; again, only removes the first case
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember6 s (cdr l)))))))

(define rember7 ; this one removes them all
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (rember7 s (cdr l)))
      (else (cons (car l) (rember7 s (cdr l)))))))

;simplify insertL*

;p99
(trace-define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp)) ; this is the only place that returns #t/f
      ((eq? (car (cdr aexp)) (quote +))
       (and (numbered? (car aexp))
            (numbered?
             (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote x))
       (and (numbered? (car aexp))
            (numbered?
             (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^))
       (and (numbered? (car aexp))
            (numbered?
             (car (cdr (cdr aexp)))))))))


(trace-define numbered2?
  (trace-lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered2? (car aexp))
            (numbered2? (car (cdr (cdr aexp))))))))) ; chop off the first two and return the 3rd


(trace-define numbered3?
  (lambda (aexp)
    (cond
      ;; Base case: if `aexp` is an atom, it must be a number.
      ((atom? aexp) (number? aexp))

      ;; Recursive case: if `aexp` is a compound expression, process it.
      ((list? aexp)
       (let ((op (cadr aexp))             ; Extract the operator
             (left (car aexp))            ; Left operand
             (right (caddr aexp)))        ; Right operand
         (and (or (eq? op '+)             ; Valid operators
                  (eq? op 'x)
                  (eq? op '^))
              (numbered3? left)            ; Check left operand recursively
              (numbered3? right))))        ; Check right operand recursively

      ;; Invalid case: anything else is not a valid arithmetic expression.
      (else #f))))


(trace-define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) (quote +))
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote x))
       (x (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote ^))
       (^ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      (else #f))))

(define value2 ; this one validates the operands
  (lambda (nexp)
    (cond
      ((or (atom? nexp) (member? nexp '(+ x ^ -))) nexp)
      ((eq? (car (cdr nexp)) (quote +))
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote x))
       (x (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote ^))
       (^ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote -))
       (- (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      (else #f))))

(define value3 ; directly eval the operands
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((member? (car (cdr nexp)) '(+ x ^ -))
       (eval (car (cdr nexp)) (value3 (car nexp)) (value3 (car (cdr (cdr nexp))))))
      (else #f))))


(define value4
  (lambda (nexp)
    (cond
      ;; Base case: if `nexp` is atomic, return it as is.
      ((atom? nexp) nexp)

      ;; Recursive case: dynamically evaluate the operator with eval.
      ((member? (car (cdr nexp)) '(+ x ^ -))
       (eval (cons (car (cdr nexp))           ; Operator
                   (list (value4 (car nexp)) ; Left operand
                         (value4 (car (cdr (cdr nexp)))))))) ; Right operand

      ;; Default case: unknown input.
      (else #f))))

(trace-define value5 ; cons only
  (lambda (nexp)
    (cond
      ;; Base case: if `nexp` is atomic, return it as is.
      ((atom? nexp) nexp)

      ;; Recursive case: dynamically evaluate the operator with eval.
      ((member (car (cdr nexp)) '(+ x ^ -))
       (eval (cons (car (cdr nexp))              ; Operator
                   (cons (value5 (car nexp))     ; Left operand
                         (cons (value5 (car (cdr (cdr nexp)))) '()))))) ; Right operand

      ;; Default case: unknown input.
      (else #f))))

; p105
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; p106
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

; p106
(trace-define value6
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (value6 (1st-sub-exp nexp))
          (value6 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote x))
       (x (value6 (1st-sub-exp nexp))
          (value6 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote ^))
       (^ (value6 (1st-sub-exp nexp))
          (value6 (2nd-sub-exp nexp))))
      (else #f))))

;p108
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define pluss
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (pluss n (zub1 m)))))))


; CHAPTER 7

; p111
(define set-first?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((member? (car lat) (cdr lat)) #f)
          (else (set? (cdr lat))))))))
   

; iterative version of *
(trace-define *-iter
  (lambda (n m)
    (trace-let loop ((acc 0) (count m))
      (if (zero? count)
          acc
          (loop (+ acc n) (sub1 count))))))

; REMINDER: from above
; (define member?
;  (lambda (a lat)
;    (cond
;      ((null? lat) #f)
;      (else (or (eq? (car lat) a)
;                (member? a (cdr lat)))))))


(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

; history - ctrl-cmd-up

;p 112
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(trace-define makeset2
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cons (car lat) (multirember (car lat) (makeset2 (cdr lat))))))))

;p113
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (subset? (cdr set1) set2))
      (else #f))))


(define subset2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
       (subset2? (cdr set1) set2))))))

;p 114
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

;p115
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))
    
; intersect? or version
(define intersect2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2)
           (intersect2? (cdr set1) set2))))))

;p116 
(trace-define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
               

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))
;p117

(define set-diff
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (set-diff (cdr set1) set2))
      (else
       (cons (car set1) (set-diff (cdr set1) set2))))))

(trace-define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
       (intersect (car l-set) (intersectall (cdr l-set)))))))

;p 118
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p)))) ; or cadr

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; p120
; tests if all first elements in pairs of rel are a seq and therefore unique
; ((8 3) (4 2) (7 6)) #t
; ((4 3) (4 2) (7 6)) #f
(define fun?
  (lambda (rel)
    (set? (firsts rel)))) 


; revrel: reverse relation pairs
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (revpair (car rel)) (revrel (cdr rel)))))))

; p121

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;fullfun => injective or one-to-one

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

; ch 8 - Lambda The Ultimate
;p125

; based on rember7
(define rember-f
  (lambda (test? s l)
    (cond
      ((null? l) '())
      ((test? (car l) s) (rember-f test? s (cdr l)))
      (else (cons (car l) (rember-f test? s (cdr l)))))))

;p127

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;usages p128
; ((eq?-c 'salad) 'salad)
; ((eq?-c 'salad) 'tuna)

(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l) (rember-f2 test? a (cdr l))))))))

; > (rember-f2 eq?)
; #<procedure:.../little schemer.rkt:950:4>
; > ((rember-f2 eq?) 'a '(a b c))
; (b c)

(define rember-eq? (rember-f2 eq?))
(define rember-eq2? (rember-f2 equal?)) ; list equality too

;p 130
;((rember-f2 eq?) 'eq? '(eq? equal?))
; or
;((rember-f2 eq?) eq? (list eq? equal?))

(trace-define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
                ((test? (car lat) old)
                 (cons new (cons old (cdr lat))))
                (else (cons (car lat)
                            ((insertL-f test?) new old (cdr lat))))))))


(trace-define insertR-f
              (lambda (test?)
                (lambda (new old lat)
                  (cond
                    ((null? lat) '())
                    ((test? (car lat) old)
                     (cons old (cons new (cdr lat))))
                    (else (cons (car lat)
                                ((insertR-f test?) new old (cdr lat))))))))



; first cut calls insert{L,R}-f
(define insert-g-1
  (lambda (test? leftOrRight? new old lat)
    (cond
      ((eq? leftOrRight? 'left)
       ((insertL-f test?) new old lat))
       ((eq? leftOrRight? 'right)
       ((insertR-f test?) new old lat))
       (else lat))))

; p131
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))
; (seqL 'new 'old (cdr '(old x x)))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))
; (seqR 'new 'old (cdr '(old x x)))

; p132
; insert-q with seq but no test?
(define insert-g-seq
  (lambda (seq)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((eq? (car lat) old)
         (seq new old (cdr lat)))
         (else (cons (car lat)
                     ((insert-g-seq seq) new old
                                         (cdr lat))))))))

(define insertL-g (insert-g-seq seqL))
(define insertR-g (insert-g-seq seqR))

(define insertL-lambda
  (insert-g-seq
   (lambda (new old lat)
     (cons new (cons old lat)))))

(define insertR-lambda
  (insert-g-seq
   (lambda (new old lat)
     (cons old (cons new lat)))))

; p133
(define subst-again
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst-again new old (cdr lat)))))))

(define seqS
  (lambda (new old lat)
    (cons new lat)))

(define subst-g
  (insert-g-seq seqS))

; p134

; remember value - rewrite with car/cdr/operator
(trace-define value-ch6
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) (quote +))
       (+ (value-ch6 (car nexp))
          (value-ch6 (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote x))
       (x (value-ch6 (car nexp))
          (value-ch6 (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote ^))
       (^ (value-ch6 (car nexp))
          (value-ch6 (car (cdr (cdr nexp))))))
      (else #f))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      ((eq? x  '^) ^)
    (else #f))))


; p135

; use the function + after extracting it
; ((atom-to-function (operator '(+ 5 3))) 3 4)

;value with two conds utilizing atom-to-function

(define value-atof
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value-atof (1st-sub-exp nexp)) ; returns nexp after (atom nexp)
        (value-atof (2nd-sub-exp nexp))))))) ; same

(define multirember-again
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (multirember-again a (cdr lat)))
       (else (cons (car lat)
                   (multirember-again a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

;; > ((multirember-f eq?) 'a '(a b a c a d))
;; (b c d)
;; > ((multirember-f equal?) 'a '(a b a c a d))
;; (b c d)
;; > ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
;; (shrimp salad salad and)

; p136
(define multirember-eq?
  (multirember-f eq?))

; the following refers back to eq?-c from p127
;;(define eq?-c
;;  (lambda (a)
;;   (lambda (x)
;;      (eq? x a))))

(define eq?-tuna
  (eq?-c (quote tuna)))

; p137

; we don't need a here - we are using eq?-c e.g. eq?-tuna
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
      (multiremberT test? (cdr lat)))
    (else
     (cons (car lat)
           (multiremberT test? (cdr lat)))))))

; continuation passing style (CPS) https://en.wikipedia.org/wiki/Continuation-passing_style
; https://stackoverflow.com/a/7005024
(define multirember&co
  (lambda (a lat col) ; col == collector (a continuation) with 2 arguments
    (printf "incoming -> a: ~a, lat: ~a, col: ~a\n" a lat col)
    (cond
      ((null? lat) ; if lat is null invoked col with 2 empty lists
       (printf "NULL\n")
       (col (quote ()) (quote ())))
      ((eq? (car lat) a) ; Case: (car lat) == a
       (printf "eq: car: ~a, a: ~a\n" (car lat) a)
       (multirember&co a
                       (cdr lat)
                       ; newlat = x, seen = y
                       (trace-lambda (newlat seen) ; a new continuation wrapping col
                         (col newlat
                              (cons (car lat) seen)))))
      (else ; Case: (car lat) != a
       (printf "neq: car: ~a, a: ~a\n" (car lat) a)
       (multirember&co a
                       (cdr lat)
                       ; newlat = x, seen = y
                       (trace-lambda (newlat seen)  ; a new continuation wrapping col
                         (col (cons (car lat) newlat)
                              seen)))))))

; p138

(trace-define a-friend
  (lambda (x y)
    (null? y))) ; returns nullity of the second parameter only - false means that a was seen

;;  (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)

;; (multirember&co 'tuna '(tuna) a-friend)

; p141

; remember: diff between multiinsertL and multiinsertR is the order of cons new / old
; multiinsertL: (cons new (cons old ...
; multiinsertR: (cons old (cons new ...

(define multiinsertLR
  (lambda (new oldL oldR lat)
          (cond
            ((null? lat) (quote ()))
            ((eq? (car lat) oldL)
                  (cons new (cons oldL
                                  (multiinsertLR new oldL oldR (cdr lat)))))
             ((eq? (car lat) oldR)
                   (cons oldR (cons new
                                    (multiinsertLR new oldL oldR (cdr lat)))))
              (else
               (cons (car lat)
                     (multiinsertLR new oldL oldR (cdr lat)))))))

; p143

(define multiinsertLR&co
  (lambda (new oldL oldR lat collector)
    (cond
      ((null? lat)
       (collector (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (collector (cons new
                                            (cons oldL newlat))
                                      (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (collector (cons oldR
                                            (cons new oldR newlat))
                                      L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (collector (cons (car lat) newlat L R))))))))

; p144

(define (is-even? n) ;shorthand lambda syntax
  (= (modulo n 2) 0))

; removes all odd numbers from a list of nested lists
(define (evens-only* list-of-numbers)
  (cond
    ((null? list-of-numbers) '())
    ((atom? (car list-of-numbers))
     (cond
       ((even? (car list-of-numbers))
        (cons (car list-of-numbers)
              (evens-only* (cdr list-of-numbers))))
       (else
        (evens-only* (cdr  list-of-numbers)))))
    (else
     (cons (evens-only* (car list-of-numbers))
           (evens-only* (cdr list-of-numbers))))))

; p145
;evens-only*&co - builds the evens list, multiplies the evens, sums the odds
(trace-define evens-only*&co
  (lambda (list-of-nums collector)
    (cond
      ((null? list-of-nums)
       (collector '() 1 0)) ; Base case: empty list -> empty evens list, product 1, sum 0

      ((atom? (car list-of-nums)) ; Case: atomic element
       (cond
         ((even? (car list-of-nums))
           (evens-only*&co (cdr list-of-nums)
                           (lambda (newlist p s)
                             (collector (cons (car list-of-nums) newlist)
                                        (* (car list-of-nums) p) s))))
         (else
          (evens-only*&co (cdr list-of-nums)
                          (lambda (newlist p s)
                            (collector newlist p (+ (car list-of-nums) s)))))))
      (else ; Case: nested list
       (evens-only*&co (car list-of-nums)
                       (lambda (al ap as)
                         (evens-only*&co (cdr list-of-nums)
                                         (lambda (dl dp ds)
                                           (collector (cons al dl)
                                                      (* ap dp)
                                                      (+ as ds))))))))))

; p146
(trace-define (the-last-friend newl product sum)
              (display "XXX")
  (cons sum
        (cons product
              newl)))
  
; (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

; p149 Ch. 9 Y-Combinator

  (define looking ; a partial function
    (lambda (a lat)
      (keep-looking a (pick 1 lat) lat))) ; pick see line 292

; looking logic: searches lat for a number, n of and tries to match a to lat[n]
; ex1.  a = caviar, lat: (6 2 4 caviar 5 7 3). For each # in lat (6 2 4 5 7 3) picks lo
; and tests eq? a 'caviar. Here it is true as 4 is the position of 'caviar
; ex2 a = cavair,  (6 2 grits caviar 5 7 3). #f as no number 4 is present in numbers present. 

 
  (define keep-looking
    (lambda (a picked lat)
      (cond
        ((number? picked)
         (keep-looking a (pick picked lat) lat)) ; unnatural recursion - recurring on all of lat
        (else
         (eq? picked a)))))

; p151
; infinite search -> (looking 'caviar '(7 2 4 7 5 6 3))

(define eternity ; "the most unnatural recursion possible" :-O
  (lambda (x)    ; also "the most partial function"
    (eternity x)))

; p152

; review build, first, second:

; take a pair which has a first element pair and build (cons) a new pair by shifting the
; second element of the first pair onto the second pair
; sample: ((a b) c)
(define shift
  (lambda (pair)
    (build (first (first pair)) ; returns a - first must be a pair
          (build (second (first pair)) ;returns b
                 (second pair))))) ; c

;;> (shift '((a b) (c d)))
;;(a (b (c d)))
;;> (shift '((a b) (c d e)))
;;(a (b (c d e)))

(define align
  (lambda (pora)
          (cond
            ((atom? pora) pora)
            ((a-pair? (first pora))
             (align (shift pora))) ; recurring on a shifted initial argument!
            (else
             (build (first pora)
                    (align (second pora)))))))

; p153

(define length*
  (lambda (pora)
    (cond
    ((atom? pora) 1)
    (else
     (+ (length* (first pora))
     (length* (second pora)))))))
    
; p154
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (x (weight* (first pora)) 2)
          (weight* (second pora)))))))

; as align is called again the shifting causes the weight (len) to decrease
; it is therefore not partial bc it yields a value
; (weight* '((a b) c)) -> 7
; (weight* '(a (b c))) -> 5

(trace-define shuffle
  (lambda (pora)
    (cond
      ((atom? pora)
      (printf "atom\n") ; base case
       pora)
      ((a-pair? (first pora))
       (printf "a-pair\n")
       (shuffle (revpair pora)))
      (else
       (build (first pora)
              (shuffle (second pora)))))))

; don't try this: (shuffle '((a b) (c d)))
;; >(shuffle ((a b) (c d)))
;; a-pair
;; >(shuffle ((c d) (a b)))
;; a-pair
;; >(shuffle ((a b) (c d)))
;; a-pair

; p155 Collatz https://en.wikipedia.org/wiki/Collatz_conjecture
(trace-define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))

(define collatz C)

; p156
; https://en.wikipedia.org/wiki/Ackermann_function
(trace-define A 
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

; p157-160: this section describes the Halting Problem.
; will-stop? can be described but not actually defined

; p161
; length of list of length <= 1
;(
(lambda (l)
  (cond
    ((null? l) 0) ; case: length == 0
    (else
     (add1 ; case: length == 1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else ; case: length > 1 - eternity...
            (add1 
             (eternity (cdr l))))))
       (cdr l))))))
;'())

; length of list of length <= 2
;(
(lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1
          ((lambda (l)
            (cond
              ((null? l) 0)
              (else
               (add1
                ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else
                     (add1
                      (eternity (cdr l))))))
                (cdr l))))))
     (cdr l))))))
;'(1 2))


; p162
;(
((lambda (length) ; create a function with length being the eternity function
   ;used as a placeholder which is going to get replaced by something useful... :0
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)
;'())

; which is equivalent to
;(
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))
;'())

; p163

; len <= 1
; nesting of eternity down one level... allows add1 to get called without invoking eternity
;(
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    eternity))
;'(1))

; len <= 2
;(
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    eternity)))
;'(1 2))

; with debugging
;(
((lambda (length)
   (printf "building outer with length = ~a~n" length)
   (lambda (l)
     (cond
       ((null? l)
        (printf "outer base case ~a~n" l)
        0)
       (else
        (printf "outer recurse on (cdr ~a)~n" l)
        (add1 (length (cdr l)))))))
 ((lambda (length)
    (printf "building inner with length = ~a~n" length)
    (lambda (l)
      (cond
        ((null? l)
         (printf "inner base case ~a~n" l)
         0)
        (else
         (printf "inner recurse on (cdr ~a)~n" l)
         (add1 (length (cdr l)))))))
  ((lambda (length)
    (printf "building inner inner with length = ~a~n" length)
    (lambda (l)
      (cond
        ((null? l)
         (printf "inner inner base case ~a~n" l)
         0)
        (else
         (printf "inner inner recurse on (cdr ~a)~n" l)
         (add1 (length (cdr l)))))))
  eternity)))
;'(1 2))

; p164 - get rid of the repetition by creating another function

; length = 0 
;(
((lambda (mk-length)
 ; (printf "1. mk-length: ~a\n" mk-length)
  (mk-length eternity))
 (lambda (length)
  ;(printf "2. lambda length called with length: ~a\n" length)
   (lambda (l)
     (printf "3. lambda l called with l: ~a:\n" l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;'())


;; The following mk-length variations don't use eternity - they work for len <= 1
;(
((lambda (mk-length)
   (mk-length length)) ; calling mk-length needs some "final" - try scheme.length and see what happens
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;'(1))

;(
((lambda (mk-length)
   (mk-length (lambda (l) 0))) ; calling mk-length needs some "final" - try a lambda returning 0 and see what happens
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;'())

(
((lambda (mk-length)
   (printf "outer: mk-length = ~a~n" mk-length)
   (printf "outer: applying mk-length to eternity = ~a~n" eternity)
   (mk-length eternity))
 (lambda (length)
   (printf "mk-length: received length = ~a~n" length)
   (lambda (l)
     (printf "length: called with l = ~a~n" l)
     (cond
       ((null? l)
        (printf "length: base case ~a~n" l)
        0)
       (else
        (printf "length: recurse on (cdr ~a)~n" l)
        (printf "length: invoking captured length ~a on ~a~n" length (cdr l))
        (add1 (length (cdr l))))))))
'())

;; Back to the book

; p164 - len <= 1
;(
((lambda (mk-length)
  (mk-length (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (printf "length got: ~a\n" l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;'(1))

; len <= 2
;(
((lambda (mk-length)
  (mk-length (mk-length (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;'(1 2))

; len <= 3
;(
((lambda (mk-length)
  (mk-length (mk-length (mk-length (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;'(1 2 3))

; len <= 4
;(
((lambda (mk-length)
   (mk-length (mk-length (mk-length (mk-length (mk-length eternity))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;'(1 2 3 4))

; p165
; len = 0
;(
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              (mk-length (cdr l))))))))
;'())

; p166
; len <= 1
;(
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((mk-length eternity)
               (cdr l))))))))
; '(1))

; p167
; remove eternity - keep on passing mk-length in - this should work for all n
(
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((mk-length mk-length)
               (cdr l))))))))
'(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

; p168
; no good - out of memory
;; ((lambda (mk-length)
;;    (mk-length mk-length))
;;  (lambda (mk-length)
;;    ((lambda (length)
;;       (lambda (l)
;;         (cond
;;           ((null? l) 0)
;;           (else (add1 (length (cdr l)))))))
;;     (mk-length mk-length))))

; p170
; revert to previous 
;(define xxx
(trace-define add1traced add1)

(
((lambda (mk-length)
   (printf "(1): making a length with ~a\n" mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (printf "(2): length lambda wrapper called with ~a\n" mk-length)
   (lambda (l)
     (printf "inner length lambda called with l: ~a\n" l)
     (cond
       ((null? l)
        (printf "*** cond null reached returning 0 and unwinding\n")
        0)
       (else
        (printf "else reached with l: ~a\n" l)
        (add1traced
              ((mk-length mk-length) ;create another function which applies the cdr of l - redundancy par excellance
               (cdr l))))))))
'(1 2 3 4 5 6 7))
;)

; this is a function which will apply x to the application of mk-length to itself
;(lambda (x)
;  (mk-length mk-length) x)

; p171
; now insert this new lambda into our else...
; don't run it yet
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              (lambda (x)
              ((mk-length mk-length) x))
               (cdr l)))))))

; now wrap the inner lambda (l) and move out the new lambda from the end...
; still creating a new lambda on each invocation
(
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length) ; (*)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              (length 
               (cdr l)))))))
   (lambda (x)
              ((mk-length mk-length) x)))))
'(2 3 4 5))

; p172
; more of the reorganization...
; extract the 'length' function (*)
; NOTE: this is the original 'length' function in mk-length from p164 - that was fantastic :-0
;(
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
; '())  ; still works
;'(1)) ; still works
;'(a b c d e f g)) ; still works!

; separate 'the maker' and rename args for generality
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

; usage:
;(
 (Y (lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l))))))))
;(build-list 99 values))

; factorial with the Y-combinator
;(
 (Y (lambda (fact)
      (lambda (n)
        (cond ((= n 0) 1)
            (else (* n (fact (- n 1))))))))
;5)

; p173
; try this at home...
; (Y Y)

; CHAPTER 10

; p175
(define new-entry build)

; p176
;sample data
(define entries '((appetizer entrée beverage) (paté boeuf vin))) 

; lets do something like mk-length with an internal accumulator for the index
; which we can feed to pick
(define get-index
  (lambda (a lat)
    ((lambda (mk-index)
       (mk-index mk-index lat 1))
     (lambda (mk-index lat idx)
       (cond
         ((null? lat) 0)
         ((eq? a (car lat)) idx)
         (else (mk-index mk-index (cdr lat) (add1 idx))))))))


; lookup name in entry (2 lists, key and values) and call entry-f if not found
; note: entry-f is a dummy for now - it takes shape starting on p177
(define lookup-in-entry-index-pick
  (lambda (name entry entry-f)
    (lookup-in-entry-help-index-pick name ;key
                          (first entry) ;keys list
                          (second entry) ;values list
                          entry-f)))
 
 (define lookup-in-entry-help-index-pick
   (lambda (name names values entry-f)
     (cond
       ((null? names) (entry-f name))
       (else
        ((lambda (index)
          (cond
            ((zero? index) (entry-f name))
            (else
             (pick index values))))
        (get-index name names)
        )))))
;;> (lookup-in-entry-index-pick 'beverage entries (lambda (n) (printf "~a not found\n" n)))
;;vin
;;> (lookup-in-entry-index-pick 'entrée entries (lambda (n) (printf "~a not found\n" n)))
;;boeuf

; now do it the way we are supposed to with only recursion... :-0

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name ;key
                          (first entry) ;keys list
                          (second entry) ;values list
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else
       (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

; p176 a table is a list of entries - list of (names values) lists

(define table
  '(((appetizer entree beverage) (pate boeuf vin))
   ((beverage dessert) ((food is) (number one with us)))))

; cons an entry onto a table
(define extend-table cons)
;; > (extend-table entries table)
;; (((appetizer entrée beverage) (paté boeuf vin))
;;  ((appetizer entree beverage) (pate boeuf vin))
;;  ((beverage dessert) ((food is) (number one with us))))

; p177
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
       (lookup-in-entry name (car table)
                        (lambda (name)
                          (lookup-in-table name
                                           (cdr table)
                                           table-f)))))))

(define table2 '(((entrée dessert)(spaghetti spumoni)) ((appetizer entree beverage) (food tastes good))))
;;> (lookup-in-table 'entrée table2 (lambda (n) (printf "~a not found\n" n)))
;;spaghetti

;; > (quote (a b c))
;; (a b c)

; p178
;; > (car (quote (a b c)))
;; a
;; > (cons 'a
;;         (cons 'b
;;               (cons 'c
;;                     (quote ()))))
;; (a b c)

(cons car
        (cons (cons 'quote
                    (cons
                     (cons 'a
                           (cons 'b
                                 (cons 'c
                                       (quote ()))))
                     (quote ())))
              (quote ())))
;(#<procedure:car> '(a b c))

(define form (cons car
        (cons (cons 'quote
                    (cons
                     (cons 'a
                           (cons 'b
                                 (cons 'c
                                       (quote ()))))
                     (quote ())))
              (quote ()))))

; (eval form (make-base-namespace)) ; returns a using racket environment trick

; const action
(define *const
  (lambda ( e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))



(define initial-table
  (lambda (name)
    (car (quote ()))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
      (cond
        ((eq? (car e) (quote quote)) *quote)
        ((eq? (car e) (quote lambda)) *lambda)
        ((eq? (car e) (quote cond)) *cond)
        (else *application)))
    (else *application))))

; p181
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; p183
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table  initial-table)))

; p184
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)


; p185
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of ( car lines)))
       (meaning (answer-of ( car lines))
                table))
      ((meaning (question-of ( car lines))
                table)
       (meaning (answer-of ( car lines))
                table))
      (else (evcon ( cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

; p186
(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

; p187
(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

; p188
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))



(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))



(define :atom?
  (lambda (x)
    (cond
      ((atom? x ) #t)
      ((null? x ) #f)
      ((eq? (car x) (quote primitive)) #t)
      ((eq? (car x) (quote non-primitive)) #t )
      (else #f))))

; p189-190
; new table for meaning
(define new-table '(
                    ((x y) ((a b c) (d e f)))
                    ((u v w)
                     (1 2 3))
                    ((x y z)
                     (4 5 6))))

