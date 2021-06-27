#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARTICIPANTES                                 ;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rojbar                                        ;;
;; Matomaral                                     ;;
;; thiagoc789                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;GRAMATICA USADA PARA LA SUMA ANIDADA
;suma-anidada ::= <valor> (<numero>)
;             ::= <suma> (suma-anidada suma-anidada)



;;TIPO DE DATO SUMA
(define-datatype suma-anidada suma-anidada?
  (valor (numero number?))
  (suma (izq suma-anidada?)(der suma-anidada?))
)

;;(suma (valor 5) (valor 6)) -> #(struct:suma  #(struct:valor 5) #(struct:valor 6))



;; SUMA USANDO EL TIPO DE DATO SUMA-ANIDADA
;; sumaAnidada: (suma-anidada) -> integer
;; sumaAnidada es una funcion que recibe una suma-anidada y retorna el valor de sumar sus valores
(define sumaAnidada
  (lambda (valores)
    (cases suma-anidada valores
      (valor (numero) numero)
      (suma (izq der)(+ (sumaAnidada izq)(sumaAnidada der))))))

;;(sumaAnidada (suma (valor 5) (valor 6))) -> 11 


;; PARSER
;; parse-exp: sintaxis-concreta -> sintaxis-abstracta
;; parse-exp toma una suma-anidada escrita en sintaxis concreta y la pasa a sintaxis abstracta
(define parse-exp
  (lambda (dato)
    (cond
      [(eqv? (car dato) 'valor) (valor (cadr dato))]
      [else (suma
             (parse-exp (cadr dato))
             (parse-exp (caddr dato))
             )
      ]
     )
   )
)

;;(parse-exp '(suma (valor 5) (valor 6))) ;----> (struct:suma #(struct:valor 5) #(struct:valor 6))


;; UNPARSER
;; unparse-exp: sintaxis-abstracta -> sintaxis-concreta
;; unparse-exp:  toma una sumaAnidada escrita en sintaxis abstracta y la pasa a sintaxis concreta
(define unparse-exp
  (lambda (exp)
    (cases suma-anidada exp
      (valor (numero)(list 'valor numero))
      (suma (izq der)
                  (list 'suma (unparse-exp izq)(unparse-exp der)))
      )))

;(define a
  ;(parse-exp '(suma (valor 5) (suma (valor 6)(valor 8)))))

;(unparse-exp a) -> (suma (valor 5) (valor 6))















