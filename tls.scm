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
          '(peaches bananas tangerines)))

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
