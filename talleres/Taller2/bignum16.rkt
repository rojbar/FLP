#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARTICIPANTES                                 ;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rojbar                                        ;;
;; Matomaral                                     ;;
;; thiagoc789                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DEFINICIONES CLIENTE PUNTO 1
(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))

(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))
    ))
    
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))
;; GRAMATICA
;; <bignum> ::= (empty) | (number <bignum>)

;;zero: -> empty
;;Proposito: zero es una funcion que no recibe ningun argumento y retorna una lista vacia  
(define zero (lambda () empty ) )

;;is-zero?: integer -> boolean
;;Proposito: is-zero? es una funcion que recibe un numero cualquiera y determina si es igual a zero
(define is-zero? (lambda (n) (null? n)))

;;successor: integer -> integer
;;Proposito: successor es una funcion que recibe como un argumento un numero entero no negativo y retorna el sucesor de ese numero
(define successor
  (lambda (n)
    (cond
      [(is-zero? n) (cons 1 empty)]
      [(< (car n) 15) (cons (+ (car n) 1) (cdr n))]
      [else (cons 0 (successor (cdr n)))]
    )               
  )
)

;;predecessor: integer -> integer
;;predecessor: es una funcion que recibe como un argumento un numero entero no negativo y retorna el predecesor de ese numero
;;el predecesor de zero no esta definido
(define predecessor
  (lambda (n)
    (cond
    [(is-zero? n) (eopl:error "no hay predecesor de cero" )]
    [(is-zero? (cdr n))
     (if (equal? (car n) 1) empty (cons (- (car n) 1) empty) )
    ]
    [(> (car n) 0) (cons (- (car n) 1) (cdr n))]
    [else (cons 15 (predecessor (cdr n)))]
   )
  )
)

;; SUMAS
(suma '(9) '(3))
(suma '(15 1) '(15))
;; RESTAS
(resta '(0 1) '(2))
(resta '(3 3 4 5) '(0 2 3)) 
;; MULTIPLICACION
(multiplicacion '(3) '(9))
(multiplicacion '(0 1) '(0 1))
;; FACTORIAL
(factorial '(3))
(factorial '(7))
;; POTENCIA
(potencia '(2) '(6))
(potencia '(2) '(3))
