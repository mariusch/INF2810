(load "prekode3a.scm")


;;Oppgave 1 a og b

(define mem
  (let ((f-table (make-table)))
    (lambda (message proc)
    (cond ((eq? message 'memoize)
           (begin
             (let ((old-proc proc))
               (let ((new-proc (memo proc)))
               (insert! new-proc old-proc f-table)
             new-proc))))   
          ((eq? message 'unmemoize)
           (lookup proc f-table))
          (else (" you...Stupid"))))))

(define (memo proc)
  (let ((args-table (make-table)))
    (lambda args
      (or (lookup args args-table)
          (let ((result (apply proc args)))      
            (insert! args result args-table)
            result)))))


(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)
(set! fib (mem 'unmemoize fib))
(fib 3)
(newline)

(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)

;;(c)
;;Forskjellen er at i mem-fib blir ikke prosedyren satt med set!, men heller definert.
;;Av den grunn blir ikke selve prosedyren memoisert, istedenfor blir det laget en ny prosedyre som er den
;;memoiserte versjonen av prosedyren,
;;men overskriver ikke den eksisterende prosedyren. Derfor blir det også bare lagret en memoisert
;;prosedyre av prosedyrene for vært ulike kall, og ikke de rekursive resultatene av feks. fib.
;;Det viktige her er ikke forskjellen på define og set!, heller at mem-fib kaller rekursivt på fib,
;;der fib ikke nødvendigvis er memoisert.
;;Etter at fib 3 da blir kalt med mem-fib første gang husker den resultatet av fib med verdien 3, men ikke
;;noe som ligger lavere i det rekursive kallet. Fib 2 blir derfor ikke lagret i fib 3.

;;(d)
(define (greet . args)
  (define (help kall items)
    (cond ((null? items) #f)
          ((equal? kall (car items)) (cadr items))
          (else (help kall (cddr items)))))
  (let ((time (or (help 'time args) "day")))
    (let ((title (or (help 'title args) "friend")))
    (display (string-append "good " time " " title "\n")))))

(newline)
(greet)
(greet 'time "evening")
(greet 'title "sir" 'time "morning")
(greet 'time "afternoon" 'title "dear")
(newline)
 


;;Oppgave 2 Strømmer

;;(a)
(define (list-to-stream items)
  (if (null? items)
      the-empty-stream
      (cons-stream (car items) (list-to-stream (cdr items)))))

(define (stream-to-list stream . args)
  (define (iter stream count)
    (if (= 0 count)
        '()
        (cons (stream-car stream) (iter (stream-cdr stream) (- count 1)))))
  (cond ((= 0 (length args))
         (if (null? stream)
             '()
             (cons (stream-car stream) (stream-to-list (stream-cdr stream)))))
        ((= 1 (length args)) (iter stream (car args)))
        (else "nope")))

(list-to-stream '(1 2 3 4 5))
(stream-to-list (stream-interval 10 20))
(show-stream nats 15)
(stream-to-list nats 10)


;;(b)
(define (stream-map proc . argstreams)
  (if (any? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (any? pred)
  (cond ((null? pred) #t)
        ((null? (car pred)) #t)
        ((null? (cdr pred)) #f)
        (else (any? (cdr pred)))))

(newline)
(stream-map + (stream-interval 2 3) (stream-interval 10 20) (stream-interval 10 20))


;;(c)
;;Problemet er at strømmer kan være uendelige, og memq vil løpe gjennom hele strømmen
;;for å finne duplikater. Det vil dermed ta uendelig tid, med andre ord vil ikke kallet på memq terminere.
;;Når vi jobber med strømmer må vi passe på å aldri trenge å løpe gjennom hele sekvensen.

;;(d)
(newline)

(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (stream-filter (lambda (x) (not (eq? x (stream-car stream))))
                                  (remove-duplicates (stream-cdr stream))))))


(show-stream (remove-duplicates (list-to-stream '(1 2 3 2 4 5 1 2 3))))
;(define ones (cons-stream 1 ones))
;((show-stream (remove-duplicates ones)))


;;(e)
(newline)

(define x (stream-map show (stream-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

;;Her ser vi at det benyttes memoisering ved utsatt evaluering. Når et kall
;;utsettes (ved å kalle på delay) vil det pakkes en prosedyre rundt kallet,
;;og denne prosedyren memoiseres. På den måten largres det som skal kalles på,
;;men kallet vil ikke utføres.

;;Dette forteller to viktige ting om utsatt evaluering.
;;Det ene er at ved utsatt evaluering kan vi ikke benytte oss av destruktive operasjoner
;;(og fremdeles ha kontroll over programmet), fordi vi har ikke oversikt over
;;når disse kalles. Det andre er at ved utsatt evaluering risikerer man å evaluere samme
;; uttrykk flere ganger. Dette er fordi uttrykket kan sendes videre til
;;flere prosedyrer uten å evalueres først, og uten memoisering ville uttrykket bli
;;beregnet en gang for hver prosedyre det sendes til.

;;(f)
(newline)

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams nats factorials)))

(stream-ref factorials 5)
(show-stream factorials)


