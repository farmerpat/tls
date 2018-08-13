(module tls *
  (import (except scheme member map))

  (define *total-tests-run* 0)
  (define *total-tests-run-global* 0)
  (define *total-tests-passed* 0)
  (define *total-tests-passed-global* 0)
  (define *total-tests-failed* 0)
  (define *total-tests-failed-global* 0)

  (define-syntax assert-eq
    (syntax-rules ()
      ((_ exp1 exp2)
       (let ((val1 exp1)
             (val2 exp2))
         (if (eq? val1 val2)
             #t
             (begin
               (display (quote exp1))
               (display " !eq? ")
               (display (quote exp2))
               (newline)
               #f))))))

  (define-syntax assert-equal
    (syntax-rules ()
      ((_ exp1 exp2)
       (let((val1 exp1)
            (val2 exp2))
         (if (equal? val1 val2)
             #t
             (begin
               (display (quote exp1))
               (display " !equal? ")
               (display (quote exp2))
               (newline)
               #f))))))

  (define-syntax test
    (syntax-rules ()
      ((_ test-name exp1 exp2)
       (begin
         (set! *total-tests-run* (add1 *total-tests-run*))
         (set! *total-tests-run-global* (add1 *total-tests-run-global*))
         (display "testing that ")
         (display test-name)
         (newline)
         (if (assert-eq exp1 exp2)
             (begin
               (define *total-tests-passed* (add1 *total-tests-passed*))
               (define *total-tests-passed-global* (add1 *total-tests-passed-global*))
               (display "...PASSED")
               (newline))
             (begin
               (define *total-tests-failed* (add1 *total-tests-failed*))
               (define *total-tests-failed-global* (add1 *total-tests-failed-global*))
               (display "...FAILED")
               (newline)))))))

  (define-syntax test-equal
    (syntax-rules ()
      ;; if we know that exp1 is a procedure call,
      ;; should be able to extract the name of the
      ;; procedure and the arguments.
      ;; procedure-information can give us some meta
      ;; info about procedures. for most of them,
      ;; (excluding, so far, +, -, /, *, and apply)
      ;; (also (procedure-information map) gives (map-in-order...)
      ;; on chicken...
      ;; (procedure-information member?)
      ;; (member? atom lst)
      ;; we can get the procedure name
      ;; and the lambda list
      ;; if i do something with this (e.g. write a macro/procedure
      ;; that takes a list of test specifiers (test or test-equal,
      ;; the function to be tested, and then pairs of the arguments
      ;; to the function and the expected results)), I may
      ;; want to stop assuming...well I doubt i would need
      ;; procedure-information to acheive this, but it could
      ;; be used to display the mappings from prcoedure arguments
      ;; to the argument set values
      ((_ test-name exp1 exp2)
       (begin
         (set! *total-tests-run* (add1 *total-tests-run*))
         (set! *total-tests-run-global* (add1 *total-tests-run-global*))
         (display "testing that ")
         (display test-name)
         (newline)
         (if (assert-equal exp1 exp2)
             (begin
               (define *total-tests-passed* (add1 *total-tests-passed*))
               (define *total-tests-passed-global* (add1 *total-tests-passed-global*))
               (display "...PASSED")
               (newline))
             (begin
               (define *total-tests-failed* (add1 *total-tests-failed*))
               (define *total-tests-failed-global* (add1 *total-tests-failed-global*))
               (display "...FAILED")
               (newline)))))))

  (define-syntax test-group
    (syntax-rules ()
      ((_ test-group-name a-test ...)
       (begin
         (define *total-tests-run* 0)
         (define *total-tests-passed* 0)
         (define *total-tests-failed* 0)

         (newline)
         (display "entering test group ")
         (display test-group-name)
         (newline)
         a-test
         ...
         (begin
           (newline)
           (display test-group-name)
           (display " results:")
           (newline)
           (display "*total-tests-run*: ")
           (display *total-tests-run*)
           (newline)
           (display "*total-tests-passed*: ")
           (display *total-tests-passed*)
           (newline)
           (display "*total-tests-failed*: ")
           (display *total-tests-failed*)
           (newline))))))

  (define atom?
    (lambda (x)
      (and (not (pair? x))
           (not (null? x)))))

  (define add1
    (lambda (n)
      (+ n 1)))

  (define sub1
    (lambda (n)
      (- n 1)))

  (define (lat? lst)
    (cond ((null? lst) #t)
          ((atom? (car lst))
           (lat? (cdr lst)))
          (else #f)))

  ;; mine
  (define (member? atom lst)
    (cond ((null? lst) #f)
          ((eq? (car lst) atom)
           #t)
          (else (member? atom (cdr lst)))))

  ;; theirs (a bit simplified)
  (define (their-member? a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (their-member? a (cdr lat))))))

  (define rember
    (lambda (a l)
      (cond ((null? l) '())
            ((eq? (car l) a) (cdr l))
            (else (cons (car l)
                        (rember a (cdr l)))))))

  (define (lol? lol)
    (define (lol-rec l)
      (if (null? l) #t
          (and (list? (car l))
               (lol-rec (cdr l)))))
    (if (null? lol)
        #f
        (and (list? (car lol))
             (lol-rec (cdr lol)))))

  (define (firsts lol)
    (cond ((null? lol) '())
          (else (cons (car (car lol))
                      (firsts (cdr lol))))))

  (define (insertR new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons (car lat)
                 (cons new (cdr lat))))
          (else (cons (car lat)
                      (insertR new old (cdr lat))))))

  (define (insertL new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new lat))
          (else (cons (car lat)
                      (insertL new old (cdr lat))))))

  (define (subst new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new (cdr lat)))
          (else (cons (car lat)
                      (subst new old (cdr lat))))))

  (define (subst2 new old1 old2 lat)
    (cond ((null? lat) '())
          ((or (eq? (car lat) old1)
               (eq? (car lat) old2))
           (cons new (cdr lat)))
          (else (cons (car lat)
                      (subst2 new old1 old2 (cdr lat))))))

  (define (multirember atom lat)
    (cond ((null? lat) '())
          ((eq? (car lat) atom)
           (multirember atom (cdr lat)))
          (else (cons (car lat)
                      (multirember atom (cdr lat))))))

  (define (multiinsertR new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons old
                 (cons new
                       (multiinsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertR new old (cdr lat))))))

  (define (multiinsertL new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new
                 (cons old (multiinsertL new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertL new old (cdr lat))))))

  (define (multisubst new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new
                 (multisubst new old (cdr lat))))
          (else (cons (car lat)
                      (multisubst new old (cdr lat))))))

  ;; end chapter 3
  ;; begin things that occurred to me
  (define (pure-pair? p)
    (and (pair? p)
         (not (list? p))
         (atom? (car p))
         (atom? (cdr p))))

  ;; in the repl, the following
  ;; will print a readable expansion
  ;; of the macro nil!, given the
  ;; argument foo:
  ;; (pp (expand '(nil! foo)))

  ;; it is silly to write out the same sort of
  ;; body all the time...schemes come with
  ;; (every <predicate> <list>), but lets
  ;; assume we don't have it
  (define (every pred? lst)
    (cond ((null? lst) #t)
          ((pred? (car lst))
           (every pred? (cdr lst)))
          (else #f)))

  ;; list of pure pairs?
  (define (lopp? lst)
    (every pure-pair? lst))

  ;; end things that occurred to me
  ;; begin chapter 4
  (define (plus m n)
    (cond ((zero? m) n)
          (else (add1 (plus (sub1 m) n)))))

  (define (minus m n)
    (cond ((zero? n) m)
          (else (sub1 (minus m (sub1 n))))))

  (define (tup? lst)
    (cond ((null? lst) #t)
          ((number? (car lst))
           (tup? (cdr lst)))
          (else #f)))

  (define (addtup tup)
    (cond ((null? tup) 0)
          (else (plus (car tup)
                      (addtup (cdr tup))))))

  (define (times m n)
    (cond ((= m 0) 0)
          (else (plus (times (sub1 m) n)
                      n))))

  (define (tup+ tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (plus (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2))))))

  (define (gt n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (gt (sub1 n) (sub1 m)))))

  (define (lt n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (lt (sub1 n) (sub1 m)))))

  (define (eq n m)
    (and (not (gt n m))
         (not (gt m n))))

  ;; begin tests
  ;; this could/should? be extended to take a
  ;; test-group name as an argument.
  (define run-tests
    (lambda ()
      (test-group "***add1 tests***"
        (test "passing 5 to add1 produces 6" (add1 5) 6)
        (test "passing -1 to add1 produces 0" (add1 -1) 0)
        (test "passing 0 add1 produces 1" (add1 0) 1)
        (test "passing -20 to add1 produces -19" (add1 -20) -19))

      (test-group "***sub1 tests***"
        (test "passing 5 to sub1 produces 4" (sub1 5) 4)
        (test "passing -1 to sub1 produces -2" (sub1 -1) -2)
        (test "passing 0 to sub1 produces -1" (sub1 0) -1)
        (test "passing -20 to sub1 produces -21" (sub1 -20) -21))

      (test-group "***atom? tests***"
        (test "passing 0 to atom? produces #t" (atom? 0) #t)
        (test "passing -20 to atom? produces #t" (atom? -20) #t)
        (test "passing 50 to atom? produces #t" (atom? 50) #t)
        (test "passing 'a to atom? produces #t" (atom? 'a) #t)
        (test "passing (quote ()) to atom? produces #f" (atom? (quote ())) #f)
        (test "passing '() to atom? produces #f" (atom? '()) #f)
        (test "passing '*abc$ to atom? produces #t" (atom? '*abc$) #t)
        (test "passing '(apples) to atom? produces #f" (atom? '(apples)) #f))

      (test-group "***lat? tests***"
        (test "passing '() to lat? produces #t" (lat? '()) #t)
        (test "passing '(a) to atom? produces #t" (lat? '(a)) #t)
        (test "passing '((a)) to atom? produces #f" (lat? '((a))) #f)
        (test "passing '(a b (c) d) to atom? produces #f" (lat? '(a b (c) d)) #f)
        (test "passing '(a b c d e f g) to lat? produces #t" (lat? '(a b c d e f g)) #t))

      (test-group "***member? tests***"
        (test
          "passing 'apples and '() to lat? produces #f"
          (member? 'apples '()) #f)
        (test
          "passing 'apples and '(apples) to lat? produces #t"
          (member? 'apples '(apples)) #t)
        (test
          "passing 'apples and '((bananas) apples) to member? produces #t"
          (member? 'apples '((bananas) apples)) #t)
        (test
          "passing '(apples) and '((apples)) to member? produces #f"
          (member? '(apples) '((apples))) #f))

      (test-group "**rember tests***"
        (test
          "passing 'apples and '() to rember produces '()"
          (rember 'apples '()) '())
        (test-equal
          "passing 'apples and '(apples) to rember produces '()"
          (rember 'apples '(apples)) '())
        (test-equal
          "passing 'apples and '(bananas apples) to rember produces '(bananas)"
          (rember 'apples '(bananas apples)) '(bananas))
        (test-equal
          "passing 'mint and '(lamb chops and mint flavored mint jelly) produces '(lamb chops and flavored mint jelly)"
          (rember 'mint '(lamb chops and mint flavored mint jelly))
          '(lamb chops and flavored mint jelly)))

      (test-group "**lol? tests***"
        (test
          "passing '() to lol? produces #f"
          (lol? '()) #f)
        (test
          "passing '(atom) to lol? produces #f"
          (lol? '(atom)) #f)
        (test
          "passing '((atom)) to lol? produces #t"
          (lol? '((atom))) #t)
        (test
          "passing '((foo) (bar) (baz)) to lol? produces #t"
          (lol? '((foo) (bar) (baz))) #t)
        (test
          "passing '((foo) bar (baz)) to lol? produces #f"
          (lol? '((foo) bar (baz))) #f))

      (test-group "**firsts tests***"
        (test "passing '() to firsts produces '()"
              (firsts '()) '())
        (test-equal "passing '((apples)) to firsts produces '(apples)"
              (firsts '((apples))) '(apples))
        (test-equal
          "passing '((peaches pears) (apples)) to firsts produces '(peaches apples)"
          (firsts '((peaches pears) (apples))) '(peaches apples))
        (test-equal
          "passing '((peaches bananas pears) (bananas apples) (tangerines)) to firsts produces '(peaches bananas tangerines)"
          (firsts '((peaches bananas pears) (bananas apples) (tangerines)))
          '(peaches bananas tangerines))
        (test-equal
          "'((five plums) (four) (eleven green oranges)) produces '(five four eleven)"
          (firsts '((five plums) (four) (eleven green oranges)))
          '(five four eleven))
        (test-equal
          "'((a b) (c d) (e f)) produces '(a c e)"
          (firsts '((a b) (c d) (e f)))
          '(a c e)))

      (test-group "**insertR tests***"
        (test-equal "'foo 'bar '() produces '()"
          (insertR 'foo 'bar '()) '())
        (test-equal "'topping 'fudge '(ice cream with fudge for dessert) produces \
               '(ice cream with fudge topping for dessert)"
          (insertR 'topping 'fudge '(ice cream with fudge for dessert))
          '(ice cream with fudge topping for dessert))
        (test-equal "'jalapeno 'and '(tacos tamales and salsa) equal? \
                     '(tacos tamales and jalapeno salsa)"
          (insertR 'jalapeno 'and '(tacos tamales and salsa))
          '(tacos tamales and jalapeno salsa))
        (test-equal "'e 'd '(a b c d f g d h) equal? '(a b c d e f g d h)"
          (insertR 'e 'd '(a b c d f g d h))
          '(a b c d e f g d h)))

      (test-group "**insertL tests***"
        (test-equal "'foo 'bar '() produces '()"
          (insertL 'foo 'bar '()) '())
        (test-equal "'topping 'fudge '(ice cream with fudge for dessert) produces \
               '(ice cream with topping fudge for dessert)"
          (insertL 'topping 'fudge '(ice cream with fudge for dessert))
          '(ice cream with topping fudge for dessert))
        (test-equal "'jalapeno 'and '(tacos tamales and salsa) equal? \
                     '(tacos tamales jalapeno and salsa)"
          (insertL 'jalapeno 'and '(tacos tamales and salsa))
          '(tacos tamales jalapeno and salsa))
        (test-equal "'e 'd '(a b c d f g d h) equal? '(a b c d e f g d h)"
          (insertL 'e 'd '(a b c d f g d h))
          '(a b c e d f g d h)))

      (test-group "**subst**"
        (test-equal "'foo 'bar '() equal? '()"
          (subst 'foo 'bar '())
          '())
        (test-equal "'foo 'bar '(bar) equal? '(foo)"
          (subst 'foo 'bar '(bar))
          '(foo))
        (test-equal "'topping 'fudge '(ice cream with fudge for dessert) equal? \
                     '(ice cream with topping for dessert)"
          (subst 'topping 'fudge '(ice cream with fudge for dessert))
          '(ice cream with topping for dessert))
      )

      (test-group "**subst2**"
        (test-equal "'a 'b 'c '() equal? '()"
          (subst2 'a 'b 'c '())
          '())
        (test-equal "'vanilla 'chocolate 'banana '(banana ice cream \
                     with chocolate topping) equal? '(vanilla ice cream \
                     with chocolate topping)"
          (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
          '(vanilla ice cream with chocolate topping))
        (test-equal "'a 'b 'c '(a c b d) equal? '(a a b d)"
          (subst2 'a 'b 'c '(a c b d))
          '(a a b d))
        (test-equal "'a 'b 'c '(a d b c) equal? '(a d a c)"
          (subst2 'a 'b 'c '(a d b c))
          '(a d a c)))

      (test-group "**multirember**"
        (test-equal "'a '() equal? '()"
          (multirember 'a '())
          '())
        (test-equal "'a '(a b c) equal? '(b c)"
          (multirember 'a '(a b c))
          '(b c))
        (test-equal "'a '(a b a c a d a) equal? '(b c d)"
          (multirember 'a '(a b a c a d a))
          '(b c d))
        (test-equal "'a '(a a a a a) equal? '()"
          (multirember 'a '(a a a a a))
          '())
        (test-equal "'cup '(coffee cup tea cup and hick cup) equal? '(coffee tea and hick)"
          (multirember 'cup '(coffee cup tea cup and hick cup))
          '(coffee tea and hick)))

      (test-group "**multiinsertR**"
        (test-equal
          "'a 'b '() equal? '()"
          (multiinsertR 'a 'b '())
          '())
        (test-equal
          "'a 'b '(a a c d e f g) equal? '()"
          (multiinsertR 'a 'b '(a a c d e f g))
          '(a a c d e f g))
        (test-equal
          "'a 'b '(b) equal? '(b a)"
          (multiinsertR 'a 'b '(b))
          '(b a))
        (test-equal
          "'a 'b '(b b b) equal? '(b a b a b a)"
          (multiinsertR 'a 'b '(b b b))
          '(b a b a b a)))

      (test-group "**multiinsertL**"
        (test-equal
          "'a 'b '()) equal? '()"
          (multiinsertL 'a 'b '())
          '())
        (test-equal
          "'a 'b '(a a c d e f g) equal? '(a a c d e f g)"
          (multiinsertL 'a 'b '(a a c d e f g))
          '(a a c d e f g))
        (test-equal
          "'a 'b '(b) equal? '(a b)"
          (multiinsertL 'a 'b '(b))
          '(a b))
        (test-equal
          "'a 'b '(b b b) equal? '(a b a b a b)"
          (multiinsertL 'a 'b '(b b b))
          '(a b a b a b)))

      (test-group "**multisubst**"
        (test-equal
          "'a 'b '() equal? '()"
          (multisubst 'a 'b '())
          '())
        (test-equal
          "'a 'b '(b) equal? '(a)"
          (multisubst 'a 'b '(b))
          '(a))
        (test-equal
          "'a 'b '(d d d) equal? '(d d d)"
          (multisubst 'a 'b '(d d d))
          '(d d d))
        (test-equal
          "'a 'b '(b b b b b b b) equal? '(a a a a a a a)"
          (multisubst 'a 'b '(b b b b b b b))
          '(a a a a a a a))
        (test-equal
          "'a 'b '(a b c d e f g b) equal? '(a a c d e f g a)"
          (multisubst 'a 'b '(a a c d e f g a))
          '(a a c d e f g a)))

      (test-group "**plus**"
        (test "0 0 eq? 0" (plus 0 0) 0)
        (test "10 0 eq? 10" (plus 10 0) 10)
        (test "0 10 eq? 10" (plus 0 10) 10)
        (test "10 10 eq? 20" (plus 10 10) 20)
        (test "3 11 eq? 20" (plus 3 11) 14))

      (test-group "**minus**"
        (test "0 0 eq? 0" (minus 0 0) 0)
        (test "10 0 eq? 10" (minus 10 0) 10)
        (test "10 10 eq? 0" (minus 10 10) 0)
        (test "10 9 eq? 1" (minus 10 9) 1)
        (test "11 3 eq? 8" (minus 11 3) 8))

      (test-group "**tup?**"
        (test "'() eq? #t" (tup? '()) #t)
        (test "'(1) eq? #t" (tup? '(1)) #t)
        (test "'(1 2 3) eq? #t" (tup? '(1 2 3)) #t)
        (test "'(a) eq? #f" (tup? '(a)) #f)
        (test "'(1 2 (3) 4) eq? #f" (tup? '(1 2 (3) 4)) #f))

      (test-group "**addtup**"
        (test "'() eq? 0" (addtup '()) 0)
        (test "'(1) eq? 1" (addtup '(1)) 1)
        (test "'(0 0 0 0 1) eq? 1" (addtup '(0 0 0 0 1)) 1)
        (test "'(1 2 3 4) eq? 1" (addtup '(1 2 3 4)) 10)
        (test "'(20 30 0 0 1 2) eq? 1" (addtup '(20 30 0 0 1 2)) 53))

      (test-group "**times**"
        (test "1 1 eq? 1" (times 1 1) 1)
        (test "1 13 eq? 13" (times 1 13) 13)
        (test "12 12 eq? 144" (times 12 12) 144)
        (test "6 7 eq? 144" (times 6 7) 42))

      (test-group "**tup+**"
        (test-equal
          "'(3 6 9 11 4) '(8 5 2 0 7) equal? '(11 11 11 11 11)"
          (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
          '(11 11 11 11 11))
        (test-equal
          "'() '() equal? '()"
          (tup+ '() '())
          '()))

      (newline)
      (newline)
      (display "*total-tests-run-global*: ")
      (display *total-tests-run-global*)
      (newline)
      (newline)
      (display "*total-tests-passed-global*: ")
      (display *total-tests-passed-global*)
      (newline)
      (newline)
      (display "*total-tests-failed-global*: ")
      (display *total-tests-failed-global*)
      (newline)
      (newline)))
)
