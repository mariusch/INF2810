;; Oppg 1

;;Oppg a-d se egen fil

;; Oppg e
;; (define foo '(1 2 3))
;; (cons foo foo) --> ((1 2 3) 1 2 3)
;;
;;
;;         +-------+-------+
;;         |       |       |
;;         |   o   |   o   |
;;         |   |   |   |   |
;;         +---+---+---+---+
;;             |      /
;;             |     /
;;             |    /
;;             |   / 
;;             |  /
;;             V V
;;            +-------+-------+        +-------+-------+        +-------+-------+
;;            |       |       |        |       |       |        |       |    /  |
;;    foo---> |   o   |   o---+------> |   o   |   o---+------> |   o   |   /   |
;;            |   |   |       |        |   |   |       |        |   |   |  /    |
;;            +---+---+-------+        +---+---+-------+        +---+---+-------+
;;                |                        |                        |
;;                |                        |                        |
;;                |                        |                        |
;;                V                        V                        V
;;
;;                2                        2                        3

"Oppgave 1 F-I"
;;f)
(caddr '(1 2 3 4))

;;g)
(caadr '((1 2) (3 4)))

;;h)
(caaddr '((1) (2) (3) (4)))

;;i)
(list (list 1 2) (list 3 4))

(cons (cons 1 (cons 2 '()))
      (cons (cons 3 (cons 4 '()))'() ))


;; Oppg 2

"Oppgave 2 A"
;; a

(define (length2 liste)
  (define (lengde-iter liste teller)
    (if (null? liste)
        teller
        (lengde-iter (cdr liste)
              (+ 1 teller))))
  (lengde-iter liste 0))

(length2 '(1 2 3 4 7))

"Oppgave 2 B"
;; b)
(define (rev-list liste)
  (define (rev-iter a b)
    (if (null? a)
        b
         (rev-iter (cdr a)
               (cons (car a) b))))
(rev-iter liste '()))

(rev-list '("Marius." "heter" "Jeg"))
;;Denne metoden bruker halerekursjon. Det ble valgt fordi det var det første måten jeg tenkte på og
;; jeg hadde ikke effektivitet i tankene

"Oppgave 2 C"
;; c
;; Her har jeg skrevet en rekursiv versjon som vokser lineært med hensyn til tid (0(n) og minne (0(n).
(define (ditch bort liste)
  (cond ((null? liste) '())
      ((= bort (car liste)) (ditch bort (cdr liste)))
      (else (cons (car liste) (ditch bort (cdr liste))))))

(ditch 3 '(1 2 8 3 4 5 3))

"Oppg 2 D"
(define (nth pos liste)
  (define (iter liste teller)
    (if (null? liste)
        #f
    (if (equal? pos teller)
        (car liste)
        (iter (cdr liste)
              (+ 1 teller)))))
  (iter liste 0))

(nth 2 '(47 11 12 13))

;;e
"Oppg 2 E"
(define (where pos liste)
  (define (iter liste teller)
    (if (null? liste)
        #f
    (if (equal? (car liste) pos)
        teller
        (iter (cdr liste)
              (+ 1 teller)))))
  (iter liste 0))

(where 9 '(1 2 3 3 4 5 3 9))

;; f)
"oppg 2 F"
(define (map2 proc liste1 liste2)
  (cond
    ((null? liste1) '())
    ((null? liste2) '())
    (else
     (cons (proc (car liste1) (car liste2))
           (map2 proc (cdr liste1) (cdr liste2))))))
(map2 + '(1 2 3 4) '(3 4 5))

;; g)
"Oppg 2 I"
(map2 (lambda (a b) (/ (+ a b) 2)) '(1 2 3 4) '(3 4 5))
(map2 (lambda (a b) (and (even? a) (even? b))) '(1 2 3 4) '(3 4 5))

;; h
"Oppg 2 H"
(define (both? pred)
  (lambda (x y)
    (and (pred x)
       (pred y))))

((both? even?) 2 4)

;; i
"Oppg 2 I"

(define (self proc) (lambda (x) (proc x x)))

((self +) 5)
((self *) 3)
(self +)
((self list) "hello")


