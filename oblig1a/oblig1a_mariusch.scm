;;Oppg 1

;;A
;; Utrykket (* (+ 4 2) 5) evaluerer til 30, fordi 4+2 = 6 og 6*5 = 30

;;B
;;Utrykket (* (+ 4 2) (5)) vil ikke kjøres, fordi vi prøver å kalle 5 som en prosedyre. Noe 5 ikke er.

;;C
;;Utrykket (* (4 + 2) 5) vil heller ikke kjøre, fordi syntaxen er feil. "+" kan komme i argumentplass,
;;men 4 er ingen prosedyre

;;D Utrykket evaluerer til 21. Vi definerer at bar er halvpraten av 42, også skriver vi ut bar.

;;E
;;Gitt at bar fortsatt er definert som i oppg C, vil utrykket evaluere til 10. Siden bar er 21-11=10.

;;F
;;Utrykket (/ (* bar 3 4 1) bar) evaluerer til 12, siden 21 * 3 * 4 * 1 = 252, delt på 21 er 12.



;;Oppg 2

;;A
;; Utrykk 1
;;Utryket evaluerer til "piff!" fordi den går igjennom og stopper etter første som ikke er #f. 1 er ikke det
;;samme som 2, mens stringen "piff!" er bare seg selv. Og det er ikke feil, så da stopper den der. Den kommer altså
;;ikke til feilen som står til slutt, fordi den stopper etter første sanne. Hadde den ikke vært en special form
;;ville den gitt error.

;; Utrykk 2
;;Dette utrykket er en annen special form som ser om alle argumentene er #t eller #f. I dette tilfellet er
;; 1 ikke det samme som 2, og blir false, mens "piff!" bare er seg selv og blir #t. Dermed stopper den og returnerer
;; #f. Dermed kommer den ikke til erroren i siste argument.

;;Utrykk 3
;; IF er også en special form, det kan vi se fordi den ikke evaluerer vanlig. "Testen" til IF-en er sann, dermed
;; skrives "poff" ut. Dersom det hadde vært false ville den gått til det udefinerte og lagd en error.

;;B
(define (sign-cond tall)
(cond ((negative? tall) -1)
      ((positive? tall) 1)
      (else 0)))

(sign-cond 144482)
(sign-cond 0)
(sign-cond -144482)

(define (sign-if tall)
(if (positive? tall)
    1
    (if (equal? tall 0)
        0
        -1)))

(sign-if 144482)
(sign-if 0)
(sign-if -144482)

;;C
(define (sign-c x)
  (or (and (positive? x) 1)
      (and (negative? x) -1)
      0))

"2C"
(sign-c -3535)

;;Oppg 3

;;A
(define (add1 tall)
  (+ 1 tall))

(add1 3)

(define (sub1 tall)
  (- tall 1))

(sub1 2)

(add1 (sub1 0))

"3B"
;;B
(define (pluss tall1 tall2)
  (if (zero? tall1)
  tall2
  (pluss (sub1 tall1) (add1 tall2))))

(pluss 3 4)


;;Oppgaven over en en rekkursiv prosedyre, som gir opphav til en iterativ prosess. Det er fordi vi ikke trenger å
;;vente på returverdien til det rekursive kallet.


"3C"
(define (pluss-iter tall1 tall2)
  (hjelp-iter tall1 tall2))

(define (hjelp-iter a b)
  (if (= a 0)
      b
      (hjelp-iter (sub1 a) (add1 b))))

(pluss-iter 1 2)


;;Oppgave 3d

(define (power-close-to b n)
     (define (power-iter e)
     (if (> (expt b e) n)
        e
       (power-iter (+ 1 e))))
     (power-iter 1))
"3D" 
(power-close-to 2 8)
;;Her har jeg prøvd å forenkle ved bruk av lokale variabler og utnyttet at når det blir blokkstruktur, er de
;;fremdeles innenfor scopet til den indre prosedyren. Så da slipper vi å sende de med som parameter, fordi de
;;alt er tilgjengelig i scopet.

;;Oppgave 3E
;; Nei, det er ikke mulig å forenkle fib-iter, fordi parameterene endres i hvert rekursive kall.