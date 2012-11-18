#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat? 
  (lambda (l)(member? 'a '(b k f))
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq?(car lat)a) #t)
      (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) 
                  (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) 
       (cons old (cons new(cdr lat))))
      (else (cons(car lat)
                  (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) 
       (cons new lat))
      (else (cons(car lat)
                  (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons new (cdr lat)))
    (else (cons (car lat) 
                (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) 
          (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat)a) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old 
             (cons new
                   (multiinsertR new old(cdr lat)))))
      (else (cons (car lat) 
                  (multiinsertR new old(cdr lat)))))))


(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new 
             (cons old 
                   (multiinsertL new old(cdr lat)))))
      (else (cons (car lat) 
                  (multiinsertL new old(cdr lat)))))))

(define multisubst 
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat)
                  (multisubst new old (cdr lat)))))))

(define test
  (lambda (s1 s2)
    (cond
      ((equal? s1 s2)'ok)
      (else s1))))

(define o+
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else (add1 (o+ m (sub1 n)))))))

(test (o+ 4 6) 10)
(test (o+ 5 9) 14)

(define o-
  (lambda (m n)
    (cond
    ((zero? n) m)
    (else (sub1 (o- m(sub1 n)))))))

(test (o- 2 1) 1)
(test (o- 45 7) 38)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(test (addtup '(1 2 3)) 6)

(define o*
  (lambda  (m n)
    (cond
      ((zero? n) 0)
      (else (o+ m (o* m (sub1 n)))))))

(test (o* 3 4) 12)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(test (tup+ '(1 2 3) '(3 2 1)) '(4 4 4))

(define >
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (> (sub1 m)(sub1 n))))))

(test (> 4 3) #t)
(test (> 1 3) #f)
(test (> 1 1) #f)
(test (> 0 0) #f)

(define <
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (< (sub1 m)(sub1 n))))))

(test (< 4 3) #f)
(test (< 1 3) #t)
(test (< 1 1) #f)
(test (< 0 0) #f)

(define =.
  (lambda (m n)
    (cond
      ((and(zero? m) (zero? n)) #t)
      ((or (zero? m) (zero? n)) #f)
      (else (=. (sub1 m)(sub1 n))))))

(define =
  (lambda (m n)
    (cond
      ((> m n) #f)
      ((< m n) #f)
      (else #t))))
 
       
(test (= 2 3)#f)
(test (= 3 2)#f)
(test (= 3 3)#t)
(test (= 0 0)#t)

(define ^
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (o* m (^ m (sub1 n)))))))
      
       
(test (^ 2 3) 8)
(test (^ 3 2) 9)
(test (^ 3 1) 3)
(test (^ 0 0) 1)
(test (^ 1 0) 1)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
    
(test (length '(restful cat eats kattetunge))4)
(test (length '(restful (cat eats) kattetunge))3)

(define pick
  (lambda (n lat)
    (cond
      ((= n 1)(car lat))
      (else (pick (sub1 n) (cdr lat))))))

(test (pick 2 '(restful cat eats kattetunge))'cat)
(test (pick 1 '(restful (cat eats) kattetunge))'restful)

(define rempick
  (lambda (n lat)
    (cond
      ((= n 1)(cdr lat))
      (else (cons (car lat) 
                   (rempick (sub1 n) (cdr lat)))))))

(test (rempick 1 '(restful (cat eats) kattetunge))
      '((cat eats) kattetunge))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(test (no-nums '(2 restful 1 4 cat eats 4 kattetunge 4))
              '(restful cat eats kattetunge))


(define all-nums
  (lambda (lat)
    (cond
    ((null? lat)'())
    ((number? (car lat))
     (cons (car lat) (all-nums (cdr lat))))
    (else (all-nums (cdr lat))))))


(test (all-nums '(2 restful 1 4 cat eats 4 kattetunge 4))
              '(2 1 4 4 4))



(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((and (atom? a1) (atom? a2)) (eq? a1 a2))
      (else #f))))

(test (eqan? 'a 4) #f)
(test (eqan? 4 4) #t)
(test (eqan? 'b 'a) #f)
(test (eqan? 'a 'a) #t)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat)0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(test (occur 'a '(b a f 3 a r a f)) 3)
(test (occur 'b '(b a b 3 a b a b)) 4)

(define one?
  (lambda (n)
    (eqan? n 1)))
      
(test (one? 2) #f)
(test (one? 'a) #f)
(test (one? 1) #t)

(define rempick2
  (lambda (nth lat)
    (cond
     ((one? nth) (cdr lat))
     (else (cons (car lat)
                 (rempick2 (sub1 nth)(cdr lat))))))) 


(test (rempick2 2 '(restful cat eats kattetunge))
      '(restful eats kattetunge))

(define rember*
  (lambda (a l)
    (cond
      ((null? l)'())
      ((and (atom? (car l)) (eqan? (car l) a)) 
       (rember* a (cdr l)))
      ((atom? (car l)) 
       (cons (car l) (rember* a (cdr l))))
      (else (cons (rember* a (car l)) 
                  (rember* a (cdr l)))))))
      
(test (rember* 'a '()) '())
(test (rember* 'a '(a)) '())
(test (rember* 'a '(a b)) '(b))
(test (rember* 'a '(a b a)) '(b))
(test (rember* 'a '((a)) ) '(()))
(test (rember* 'a '(a (a b) (b a) b a)) '((b) (b) b))


(define insertR*
  (lambda (new old l)
    (cond
      ((null? l)'())
      ((atom? (car l))
       (cond
         ((eqan? (car l) old)
          (cons old 
                (cons new (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))
      
(test (insertR* 'a 'b '(b (f b) g (b)))
       '(b a (f b a) g (b a)))


(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond 
         ((eqan? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
       (else (+ (occur* a(car l))
                (occur* a (cdr l)))))))
       
(test (occur* 'd '(f r (d h) h d)) 2)
(test (occur* 'a '((r a) (a f) r a))3)


(define subst*
  (lambda (a b l)
    (cond
      ((null? l)'())
      ((atom? (car l))
       (cond
         ((eqan? (car l) a)
          (cons b (subst* a b (cdr l))))
         (else (cons (car l) (subst* a b (cdr l))))))
       (else (cons (subst* a b (car l))
                   (subst* a b (cdr l)))))))

(test (subst* 'd 'a'(f r (d h) h d)) '(f r (a h) h a))
(test (subst* 'a 'b '((a b) (a b) d a f))
      '((b b) (b b)d b f))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? (car l) old)
          (cons new (cons old 
                          (insertL* new old (cdr l)))))
         (else (cons (car l)
                          (insertL* new old (cdr l))))))
       (else (cons (insertL* new old (car l))
                   (insertL* new old (cdr l)))))))
                   
(test (insertL* 'a 'd '(d d (d h) h d)) 
                '(a d a d (a d h) h a d))

(define member*
  (lambda (a l)
    (cond
    ((null? l) #f)
    ((atom? (car l))
     (cond
       ((eqan? (car l) a) #t)
     (else (member* a (cdr l)))))
    (else
     (or (member* a (car l))(member* a(cdr l)))))))
    
(test (member* 'b '((f i b)a (a b i f) a)) #t)
(test (member* 'c '((f i b)a (a b i f) a)) #f)
(test (member*  1 '(3 1 5(3 6 1))) #t)

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(test (leftmost '(a b d)) 'a)
(test (leftmost '((a f d) (g f d))) 'a)

(define eqlist0?
  (lambda (l1 l2)
    (cond 
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2))#f)
      ;((and (atom?(car l1)) (null? l2)) #f)
      ;((and (null? l1) (atom? (car l2)))#f) 
      ((and (atom? (car l1)) (atom?(car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist0? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2)))#f)
      (else(and (eqlist0? (car l1)(car l2))
                       (eqlist0? (cdr l1) (cdr l2)))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
    ((and (null? l1)(null? l2))#t)
    ((or (null? l1)(null? l2))#f)
    (else
     (and (equal? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))))))

(test (eqlist? '()    '())    #t)
(test (eqlist? '(a)   '())    #f)
(test (eqlist? '((a)) '())    #f)
(test (eqlist? '()    '(a))   #f)
(test (eqlist? '(a)   '(a))   #t)
(test (eqlist? '((a)) '(a))   #f)
(test (eqlist? '()    '((a))) #f)
(test (eqlist? '(a)   '((a))) #f)
(test (eqlist? '((a)) '((a))) #t)
(test (eqlist? '(a)   '(b))   #f)
(test (eqlist? '((a)) '((b))) #f)

(define equalS?
  (lambda (s1 s2)
    (cond
      ;((and (null? s1) (null? s2)) #t)
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(test (equalS? 'a   '(a)) #f)
(test (equalS? 'a   'a)   #t)
(test (equalS? '(a) '(a)) #t)
(test (equalS? '(a) 'a)   #f)
(test (equalS? 'a   'b)   #f)
(test (equalS? '(b) '(a)) #f)

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +))  ... )
      ((eq? (car (cdr aexp)) (quote *))  ... )
      ((eq? (car (cdr aexp)) (quote ^)) ... )
      
(test (numbered? 1) #t)
(test (numbered? 'sausage) #f)
(test (numbered? '(3 + (4 ^ 5))) #t)
(test (numbered? '(2 * sausage)) #f)
      
