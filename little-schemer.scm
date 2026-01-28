(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define lat?
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


(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a
                                  (cdr lat)))))))))

(define rember2
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember2 a (cdr lat)))))))

;p43
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

; p48
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
       ((eq? (car lat) old)
        (cons old (cons new (cdr lat))))
       (else (cons (car lat) (insertR new old (cdr lat)))))))))

; p51
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
       ((eq? (car lat) old)
        (cons new lat))
       (else (cons (car lat) (insertL new old (cdr lat)))))))))

;p51
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old)
               (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))

;p52
(define subst2
  (lambda (new o1 o2 lat)
     (cond
       ((null? lat) '())
       (else
        (cond
          ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
            (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

;p53
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
       (cond
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))))

;p56
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old)
          (cons old (cons new (multiinsertR new old (cdr lat)))))
         (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))


;p57
(define multiinsertL
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
               (cons new (multisubst new old (cdr lat))))
              (else (cons (car lat) (multisubst new old (cdr lat)))))))))

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

;p67
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons
        (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;p71
(define tup++
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons
        (+ (car tup1) (car tup2)) (tup++ (cdr tup1) (cdr tup2)))))))

;p72
(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
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

;p74
(define equals
  (lambda (n m)
    (cond
      ((zero? m)
       (zero? n))
      ((zero? n) #f)
      (else
       (equals (sub1 n) (sub1 m))))))

(define equals2
  (lambda (n m)
    (cond
      ((zero? m)
       (zero? n))
      ((zero? n) #f)
      (else
       (and (not (gt n m)) (not (lt n m)))))))

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

(define rempick-old
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

;p81
(define rember-all
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (rember-all (car lat) (cdr lat)))
       (else (cons (car lat)
                  (rember-all a (cdr lat)))))))


;p81
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          (rember* a (cdr l)))
         (else
          (cons (car l) (rember* a (cdr l))))))
       (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

;p82
(define insertR*
  (lambda (new old l)
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

;p85
(define occur*
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
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))


(define eqlist2?
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

;p93
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

(define rember5
  (lambda (s l)
    (cond
      ((null? l) '())
      (else
       (cond
         ((equal? (car l) s) (cdr l))
         (else
          (cons (car l) (rember5 s (cdr l)))))))))


(define rember6
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember6 s (cdr l)))))))

(define rember7
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (rember7 s (cdr l)))
      (else (cons (car l) (rember7 s (cdr l)))))))

;p99
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
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


(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered2? (car aexp))
            (numbered2? (car (cdr (cdr aexp)))))))))

(define value
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

(define value2
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
(define value6
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


; p111
(define set-first?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((member? (car lat) (cdr lat)) #f)
          (else (set? (cdr lat))))))))


(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

;p112
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
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

;p114
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

(define intersect2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2)
           (intersect2? (cdr set1) set2))))))

;p116
(define intersect
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

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
       (intersect (car l-set) (intersectall (cdr l-set)))))))

;p118
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
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; p120
(define fun?
  (lambda (rel)
    (set? (firsts rel))))


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

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

;p125
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

(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l) (rember-f2 test? a (cdr l))))))))

(define rember-eq? (rember-f2 eq?))
(define rember-eq2? (rember-f2 equal?))

;p130
(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
                ((test? (car lat) old)
                 (cons new (cons old (cdr lat))))
                (else (cons (car lat)
                            ((insertL-f test?) new old (cdr lat))))))))


(define insertR-f
              (lambda (test?)
                (lambda (new old lat)
                  (cond
                    ((null? lat) '())
                    ((test? (car lat) old)
                     (cons old (cons new (cdr lat))))
                    (else (cons (car lat)
                                ((insertR-f test?) new old (cdr lat))))))))



(define insert-g-1
  (lambda (test? leftOrRight? new old lat)
    (cond
      ((eq? leftOrRight? 'left)
       ((insertL-f test?) new old lat))
       ((eq? leftOrRight? 'right)
       ((insertR-f test?) new old lat))
       (else lat))))

;p131
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; p132
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

;p133
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

;p134
(define value-ch6
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


;p135
(define value-atof
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value-atof (1st-sub-exp nexp))
        (value-atof (2nd-sub-exp nexp)))))))

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

;p136
(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c (quote tuna)))

;p137
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
      (multiremberT test? (cdr lat)))
    (else
     (cons (car lat)
           (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

;p138
(define a-friend
  (lambda (x y)
    (null? y)))

;p141
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

;p143
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

;p144
(define is-even?
  (lambda (n)
    (= (modulo n 2) 0)))

(define evens-only*
  (lambda (list-of-numbers)
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
             (evens-only* (cdr list-of-numbers)))))))

;p145
(define evens-only*&co
  (lambda (list-of-nums collector)
    (cond
      ((null? list-of-nums)
       (collector '() 1 0))

      ((atom? (car list-of-nums))
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
      (else
       (evens-only*&co (car list-of-nums)
                       (lambda (al ap as)
                         (evens-only*&co (cdr list-of-nums)
                                         (lambda (dl dp ds)
                                           (collector (cons al dl)
                                                      (* ap dp)
                                                      (+ as ds))))))))))

;p146
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

;p149
  (define looking
    (lambda (a lat)
      (keep-looking a (pick 1 lat) lat)))

  (define keep-looking
    (lambda (a picked lat)
      (cond
        ((number? picked)
         (keep-looking a (pick picked lat) lat))
        (else
         (eq? picked a)))))

;p151
(define eternity
  (lambda (x)
    (eternity x)))

;p152
(define shift
  (lambda (pair)
    (build (first (first pair))
          (build (second (first pair))
                 (second pair)))))

(define align
  (lambda (pora)
          (cond
            ((atom? pora) pora)
            ((a-pair? (first pora))
             (align (shift pora)))
            (else
             (build (first pora)
                    (align (second pora)))))))

;p153
(define length*
  (lambda (pora)
    (cond
    ((atom? pora) 1)
    (else
     (+ (length* (first pora))
     (length* (second pora)))))))

;p154
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (x (weight* (first pora)) 2)
          (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else
       (build (first pora)
              (shuffle (second pora)))))))

;p155
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))

(define collatz C)

;p156
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

;p172
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;p175
(define new-entry build)

(define entries '((appetizer entrée beverage) (paté boeuf vin)))

(define get-index
  (lambda (a lat)
    ((lambda (mk-index)
       (mk-index mk-index lat 1))
     (lambda (mk-index lat idx)
       (cond
         ((null? lat) 0)
         ((eq? a (car lat)) idx)
         (else (mk-index mk-index (cdr lat) (add1 idx))))))))


(define lookup-in-entry-index-pick
  (lambda (name entry entry-f)
    (lookup-in-entry-help-index-pick name
                          (first entry)
                          (second entry)
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

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else
       (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

;p176
(define table
  '(((appetizer entree beverage) (pate boeuf vin))
   ((beverage dessert) ((food is) (number one with us)))))

(define extend-table cons)

;p177
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

;p181
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;p183
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table  initial-table)))

;p184
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)


;p185
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))

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

;p186
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

;p187
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

;p188
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

;p189
(define new-table '(
                    ((x y) ((a b c) (d e f)))
                    ((u v w)
                     (1 2 3))
                    ((x y z)
                     (4 5 6))))
