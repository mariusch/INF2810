;;Innlevering 2B

;;Oppgave 1

;;(a)
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

;;(b)
;;se tegning


;;Oppgave 2

;;(a)
(define (make-stack items)
  (lambda (message . args)
    (cond ((eq? message 'push!)
           (set! items (append (reverse args) items)))
          ((eq? message 'pop!)
           (if (not (null? items))
               (set! items (cdr items))))
          ((eq? message 'stack)
           items))))

;;(b)
(define (stack stack-object)
  (stack-object 'stack))

(define (pop! stack-object)
  (stack-object 'pop!))

(define (push! stack-object . args)
  (apply stack-object 'push! args))


;;Oppgave 3

;;Kode som trengs videre i noen av oppgavene:
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
(set-car! (car bah) 42)


;;(a)
;;Se tegninger 

;;list-ref er en innebygd prosedyre som peker på symbolet til indeksen i lista.
;;Siden cdr til cdddr ('d) blir satt til å peke på cdr av lista (bar) lages det en syklisk liste.
;;d vil alltid peke tilbake på b.
;;Derfor vil d sin påfølgende indeks alltid være b.


;;(b)
;;Se tegninger

;;Grunnen til at bah blir evaluert slik den blir
;;er fordi car pekeren i bah settes til å peke på samme objekt i minnet som
;;cdr i lista.
;;((42 towel) 42 towel) her er car-pekeren i lista satt til å peke på resten av lista (akkurat som cdr).


;;(c)
(define (cycle? items)
  (define (iter t-items h-items)
    (cond ((null? h-items) #f)
          ((null? (cdr h-items)) #f)        
          ((eq? t-items h-items) #t)
          (else (iter (cdr t-items) (cddr h-items)))))
  (iter items (cdr items)))

;;Her har vi ikke tatt øyde for den tomme lista

;;(d)
;;Fordi per definisjon har alle lister en begrenset lengde. Dermed vil en syklisk liste returnere false.
;;Dette er fordi den ikke har en begrenset lengde (den er "uendelig").
;;List sjekker faktisk om objektet inneholder en tom liste '() og siden dette mangler i en syklisk liste
;;returneres #f

;;(e)
(define (last-pair items)
  (if (pair? (cdr items))
      (last-pair (cdr items))
      items))

(define (make-ring items)
  (define (left-rotate!)
    (set! items (cdr items)))
  (define (right-rotate!)
    (define (iter rest)
      (if (eq? (car items) (cadr rest))
          (set! items rest)
          (iter (cdr rest))))
    (iter items))
  (define (delete!)
    (right-rotate!)
    (set-cdr! items (cddr items))
    (left-rotate!))
  (define (insert! args)
    (right-rotate!)
    (set-cdr! items (cons args (cdr items)))
    (left-rotate!))
  (if (null? items)
      "this ist not ein list"
      (set-cdr! (last-pair items) items))       
  (lambda (message . args)                   
    (cond ((eq? message 'top)
           (car items))
          ((eq? message 'left-rotate!)
           (begin
             (left-rotate!)
             (car items)))
          ((eq? message 'right-rotate!)
           (begin
             (right-rotate!))
           (car items))
          ((eq? message 'delete!)
           (begin
             (delete!)
             (car items)))
          ((eq? message 'insert!)
           (apply insert! args)
           (car items)))))

(define (top ring)
  (ring 'top))
(define (left-rotate! ring)
  (ring 'left-rotate!))
(define (right-rotate! ring)
  (ring 'right-rotate!))
(define (delete! ring)
  (ring 'delete!))
(define (insert! ring . args)
  (ring 'insert! args))



;;(f)
;;right-rotate!: Denne er mener vi er O(n).
;;Den må iterere igjennom lista helt til den finner elementet til venstre for top.
;;Hvis top da er første element i lista må den da iterere gjennom hele lista. Dette er da worst case.

;;left-rotate!: Denne er er O(1) dette er fordi hjelpeprosedyren kun indeholder et set-cdr! kall.
;;Vi setter starten i lista til å være neste element i lista. 

;;delete!: Vi mener denne også er O(n). Vi må igjennom lista til vi finner elementet før top, for å så conse sammen
;;dette med elementet etter top. Deretter left-rotater vi en gang, for å få top på riktig sted.

;;insert!:Denne er også O(n). Den gjør det samme som delete, men isteden for å conse sammen elementene
;;før og etter top, legger den heller til args antall elementer før top, og roterer for å få plassert top riktig.


















