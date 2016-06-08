(define (print-test-result expression expect res)
  (if (equal? res expect)
      (for-each display (list "Success: " expression " -> " res "\n"))
      (for-each display (list "Error: " expression " -> " res
                              ", but expected " expect "\n"))))

(define (print-test-result-if-fail expression expect res)
  (if (not (equal? res expect))
      (for-each display (list "Error: " expression " -> " res
                              ", but expected " expect "\n"))))

(define-syntax test
  (syntax-rules ()
    ((test expression expect)
     (print-test-result 'expression expect expression))))

(define-syntax test-silent
  (syntax-rules ()
    ((test expression expect)
     (print-test-result-if-fail 'expression expect expression))))


(set! the-global-environment (setup-environment))
(define glob the-global-environment)
(define repl read-eval-print-loop)

;; 2a
(define (test-2a)
  (newline) (display "tester `2a`\n")
  (test (mc-eval '(1+ 2) glob) 3)
  (test (mc-eval '(1- 2) glob) 1))

;; 2b
(define (test-2b)
  (newline) (display "tester `2b`\n")
  (mc-eval '(define (p1) 1) glob)
  (install-primitive! 'p2 (lambda () 2))
  (test (mc-eval '(p2) glob) 2)
  (test (mc-eval '(p1) glob) 1)
  (install-primitive! 'square (lambda (x) (* x x)))
  (test (mc-eval '(square 3) glob) 9)
  (test (primitive-procedure? (mc-eval 'square glob)) #t))

;; 3a
(define (test-3a)
  (newline) (display "tester `3a`\n")
  (define true-or-value #t) ; tilpass verdien til om studenten har satt
  ; `and` og `or` til å returnere 'true (#t) eller den sanne verdien (#f)
  (test (mc-eval '(and) glob) 'true)
  (test (mc-eval '(or) glob) 'false)
  (test (mc-eval '(and 1) glob) (if true-or-value 'true 1))
  (test (mc-eval '(and false) glob) 'false)
  (test (mc-eval '(and 1 2) glob) (if true-or-value 'true 2))
  (test (mc-eval '(and false 2) glob) 'false)
  (test (mc-eval '(and 2 false) glob) 'false)
  (test (mc-eval '(and #f 2) glob) 'false)
  (test (mc-eval '(or 1) glob) (if true-or-value 'true 1))
  (test (mc-eval '(or false) glob) 'false)
  (test (mc-eval '(or 1 2) glob) (if true-or-value 'true 1))
  (test (mc-eval '(or false 2) glob) (if true-or-value 'true 2))
  (test (mc-eval '(or false false) glob) 'false)
  (test (mc-eval '(or #f false) glob) 'false))

;; 3b
(define (test-3b)
  (newline) (display "tester `3b`\n")
  (define if-exp '(if (= 1 1) then 2 else 3))
  (test (mc-eval if-exp glob) 2)
  (set! if-exp '(if (= 1 0) then 2 else 3))
  (test (mc-eval if-exp glob) 3)
  (set! if-exp '(if (= 1 0) then 2 elsif (= 1 1) then 3 else 4))
  (test (mc-eval if-exp glob) 3)
  (set! if-exp '(if (= 1 0) then 2 elsif (= 0 1) then 3
                    elsif (= 1 1) then 4 else 5))
  (test (mc-eval if-exp glob) 4)
  (set! if-exp '(if (= 1 0) then 2 elsif (= 0 1) then 3
                    elsif (= 1 -1) then 4 else 5))
  (test (mc-eval if-exp glob) 5))

;; 3c
(define (test-3c)
  (newline) (display "tester `3c`\n")
  (define let-exp '(let ((var1 1)
                         (var2 2)
                         (var3 3))
                     (+ var1 var2 var3)))
  (define lambda-exp '((lambda (var1 var2 var3)
                         (+ var1 var2 var3)) 1 2 3))
  ;; erstatt "let->lambda" med studentens prosedyrenavn
  (test (let->lambda let-exp) lambda-exp)
  ;(test (let->application let-exp) lambda-exp)
  (test (mc-eval let-exp glob) 6))

;; 3d
(define (test-3d)
  (newline) (display "tester `3d`\n")
  (define let-exp '(let var1 = 1 and
                     var2 = 2 and
                     var3 = 3 in
                     (+ var1 var2 var3)))
  (test (mc-eval let-exp glob) 6))

;; 3e
(define (test-3e)
  (newline) (display "tester `3e`\n")
  (mc-eval '(define bar 5) glob)
  
  ;; while-uttrykket må tilpasses til studentens syntax
  (define while-exp '(while (not (= bar 0))
                            (begin (display bar)
                                   (newline)
                                   (set! bar (- bar 1)))))
  (mc-eval while-exp glob))

(test-2a)
(test-2b)
(test-3a)
(test-3b)
(test-3c)
(test-3d)
(test-3e)
