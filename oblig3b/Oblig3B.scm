(load "evaluator.scm")
(newline)

(set! the-global-environment (setup-environment))
(define global the-global-environment)

;;;;Innlevering 3B

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((or? exp) (eval-or (cdr exp) env)) ;;Oppgave 3A
        ((and? exp) (eval-and (cdr exp) env)) ;;Oppgave 3A
        ((while? exp) (eval-while (cdr exp) env)) ;;Oppgave 3E
        ((let? exp) (mc-eval (eval-let exp) env)) ;;Oppgave 3C
        ((if2? exp) (eval-if2? exp env)))) ;;Oppgave 3B


(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;;((or? exp) #t) ;;Oppgave 3A
        ;;((and? exp) #t) ;;Oppgave 3A
        ;;((while? exp) #t) ;;Oppgave 3E
        ;;((if2? exp) #t) ;;Oppgave 3B
        ;;((let? exp) #t) ;;Oppgave 3C
        (else #f)))



;;Oppgave 1

;;(a)
;;(foo 2 square) returnerer 0, (foo 4 square) returnerer 16, og (cond((= cond 2) 0) (else (else 4) returnerer 2.
;;Grunnen til at (foo 2 square) returnerer 0, er fordi det sjekkes om cond parameteret = 2, hvis det er det
;;skal det returneres 0.
;;Grunnen til at (foo 4 square) returnerer 16, er fordi 4 ikke er to, og derfor utføres square på 4,
;;som blir 16.
;;Grunnen til at det siste utrykket returnerer 2, er fordi den spør om cond = 2, og i dette tilfellet er det da
;;den allerede definerte conden som blir evaluert. Dermed blir om spørsmålet 3 = 2? som er feil, og derfor
;;returneres det heller "else" som da er prosedyren else som deler tallet på to, dermed deles 4 på to, og vi
;;får tilbake 2.
;;Nå kommer svaret på hvorfor dette faktisk fungerer:
;;Scheme interpreteren skjønner hvilken av definisjonene av cond og else den skal bruke ut ifra hvor de står
;;plassert i koden. Selv om den innebygde conden og conden vi definerer ser like ut i koden er de lagret på
;;ulike steder i minnet og har totalt ulike verdier. Scheme tolker uttrykket ut ifra hvordan det er satt opp.
;;Hvis man da har en cond eller else som er satt opp slik som de innebygde special-formsa, tolker scheme uttrykket
;;ut ifra dette. Hvis det derimot er satt opp på en annen måte skjønner evaluatoren at det ikke er den innebyggde
;;special-formen som blir brukt, og finner da variablen vi har funnet og verdien eller procedyren som følger med.


;;Oppgave 2

;;(2a)
;;Vi har lagt til de to nederste listene. List '1+ og list '1-.

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        (list '1+
              (lambda (x) (+ x 1)))
        (list '1-
              (lambda (x) (- x 1)))
        ;;      her kan vi legge til flere primitiver.
        ))


;;(2b)

(define (install-primitive! title body)
  ;(set! primitive-procedures (append primitive-procedures (list title body))) ;;Usikker på om denne kodebiten
  ;trengs, men våres inntrykket er at det ikke gjør det.
  (define-variable! title (list 'primitive body) global))

(mc-eval (install-primitive! 'square (lambda (x) (* x x))) global)
(mc-eval '(square 4) global)
global


;;Oppgave 3

;;(3a)

;;Se special-form? og eval-special-form +

(define (or? exp)
  (tagged-list? exp 'or))

(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-or exp env)
  (let ((mid (car exp)))
    (cond ((null? exp) #f)
          ((true? (mc-eval (car exp) env)) (set! mid (car exp)) mid)
          ((not (null? (cdr exp))) (eval-or (cdr exp) env))
          (else (true? (mc-eval mid env))))))

(define (eval-and exp env)
  (if (null? exp)
      #t
      (if (true? (mc-eval (car exp) env))
          (if (null? (cdr exp))
              (car exp)
              (eval-and (cdr exp) env))
          #f)))


(mc-eval '(or (= 2 4)
              (= 2 2)) global)

(mc-eval '(or (= 2 3)
              (= 4 5)) global)

(mc-eval '(and (= 2 2)
               (= 3 4)) global)

(mc-eval '(and (= 4 4)
               (= 6 6)) global)




;;(3b)

;;Se special-form? og eval-special-form +

(define (eval-if2? exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent2 exp) env)
      (mc-eval (if-alternative2 (cddddr exp) env) env)))

(define (if-consequent2 exp) (cadddr exp))

(define (if-alternative2 exp env)          
  (if (not (null? (cdr exp)))
      (if (true? (mc-eval (cadr exp) env))
          (if (not (null? (cddr exp)))
              (cadddr exp)
              (display "feil1"))
          (if (tagged-list? (cddddr exp) 'else)
              (cadr (cddddr exp))
              (if-alternative2 (cddddr exp) env))) 
      (display "feil2"))) 

(define (if2? exp) (tagged-list? exp 'if2?))


(mc-eval '(if2? (= 2 3)
                'then "hey"
                'elseif (= 3 2)
                'then "yey"
                'elseif (= 3 3)
                'then "mhm"
                'else "...") global)



;;(3c)           

;;Se special-form? og eval-special-form +

(define (let? exp) (tagged-list? exp 'let))

(define (eval-let exp)
  (append
   (list (make-lambda (make-var (cadr exp)) (list (caddr exp))))
   (make-val (cadr exp))))

(define (make-var exp)
  (if (null? exp)
      '()
      (cons (caar exp) (make-var (cdr exp)))))

(define (make-val exp)
  (if (null? exp)
      '()
      (cons (cadar exp) (make-val (cdr exp)))))

(mc-eval '(let ((a 1)
                (b 2)
                (c 3))
            (+ a b c)) global)


;;(3d)

;;special-form? og eval-special-form er lik som i oppgaven over. +

;;Denne og forrige oppgave har mange av de samme prosedyrenavnene,
;;så kommenter ut oppgave c når oppgave d skal kjøres.

#|
(define (let? exp) (tagged-list? exp 'let))

(define (eval-let exp)
  (append (list (make-lambda (make-var (cdr exp)) (list (make-body exp)))) (make-val (cddr exp))))

(define (make-var exp)
  (if (tagged-list? exp 'in)
      '()
      (if (tagged-list? exp '=)
          (make-var (cddr exp))
          (if (tagged-list? exp 'and)
              (make-var (cdr exp))
              (if (null? exp)
                  '()
                  (cons (car exp) (make-var (cdr exp))))))))

(define (make-val exp)
  (if (tagged-list? exp 'in)
      '()
      (if (tagged-list? exp '=)
          (make-val (cdr exp))
          (if (tagged-list? exp 'and)
              (make-val (cddr exp))
              (if (null? exp)
                  '()
                  (cons (car exp) (make-val (cdr exp))))))))

(define (make-body exp)
  (if (tagged-list? exp 'in)
      (cadr exp)
      (make-body (cdr exp))))


(mc-eval '(let x = 3 and
                 y = 4 in
                 (* x y )) global)
|#



;;(3e)

;;Se special-form? og eval-special-form +

(define (eval-while pred proc)
 (if pred
    (begin
     proc
    (eval-while pred proc))
 #f))

(define (while? exp)
 (tagged-list? exp 'while))


(mc-eval '(install-primitive! 'while eval-while) global)







