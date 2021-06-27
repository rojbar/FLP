#lang racket
;; tes-case: funcion (argumentos)* resultado -> string
;; proposito:
;; test-case es una regla de sintactica que evalua un caso prueba para el nombre de una funcion (funcion !no es string),
;; los argumentos a evaluar esa funcion (argumentos)* y el resultado esperado de esa funcion (resultado). Retorna un String indicando
;; si fue un exito el caso prueba ("SUCCESS"), si fallo por error ("ERROR: excepcion") y si falla por resultado no igual ("INCORRECT ANSWER").
;;

(provide test-case)

(define-syntax test-case
  (syntax-rules ()
    [(test-case funcion ... resultado)
     (let ()
       (with-handlers ([exn:fail? (lambda (v) (string-append "ERROR: " (exn-message v)))])
         (begin
           (define resultado-evaluar (funcion ...))
           (cond
              [(equal? resultado-evaluar resultado) "SUCCESS"]
              [else (string-append "INCORRECT ANSWER IN: (" (~s funcion ...) ") EXPECTED: " (~s resultado) " GIVEN: " (~s resultado-evaluar)) ]
           )
         )
       )
     ) 
    ]
  )
)








