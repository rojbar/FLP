#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARTICIPANTES                                 ;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rojbar                                        ;;
;; Matomaral                                     ;;
;; thiagoc789                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GRAMATICA
;; <NodeInSequence> ::= (<number> Listof(<number>) Listof(<number>))

;;number-sequence:  number -> secuencia-bidireccional
;;number-sequence es una funcion que retorna un numero en forma de secuencia-bidireccional
(define number-sequence
  (lambda (numero)
    (list numero '() '())
    )
  )

;;current-element:   secuencia-bidireccional ->  number
;;current-element es una funcion que retorna el numero en el que se encuentra la lista
(define current-element
  (lambda (secuencia-bidireccional)
    (car secuencia-bidireccional)
    )
  )

;; flatten: list -> list
;; Proposito:
;; Recibe como argumento una lista L y devuelve la lista eliminando los parentesis internos 
(define (flatten L)
  (cond ((null? L) '())
        ((pair? L)
         (append (flatten (car L)) (flatten (cdr L))))
        (else (list L))))


;; move-to-left: secuencia-bidireccional -> secuencia-bidireccional
;; move-to-left se mueve a la izquierda en la secuencia bidireccional
(define move-to-left
  (lambda (secuencia-bidireccional)
    (cond
      [(null? (car (cdr secuencia-bidireccional)))(eopl:error 'move-to-left "ya estoy en el extremo izquierdo")]
      [else (list (caadr secuencia-bidireccional)(cdr(cadr secuencia-bidireccional))(flatten(list (car secuencia-bidireccional)(cddr secuencia-bidireccional))))]
      )
    
    )
  )

;; move-to-right secuencia-bidireccional -> secuencia-bidireccional
;; move-to-right se mueve a la derecha en la secuencia bidireccional
(define move-to-right
  (lambda (secuencia-bidireccional)
    (cond
      [(null? (car (cdr(cdr secuencia-bidireccional)))) (eopl:error 'move-to-right "ya estoy en el extremo derecho")]
      [else (list (caaddr secuencia-bidireccional)(flatten(list(car secuencia-bidireccional)(cadr secuencia-bidireccional)))(cdr(caddr secuencia-bidireccional)))]
      )
   
    )
  )


;; insert-to-left number secuencia-bidireccional -> secuencia-bidireccional
;; insert-to-left inserta el number en la secuencia bidireccional a la izquierda
(define insert-to-left
  (lambda (numero secuencia-bidireccional)
    (list (car secuencia-bidireccional)(flatten(list numero (cadr secuencia-bidireccional)))(flatten(cddr secuencia-bidireccional)))
    )
  )

;; insert-to-right number secuencia-bidireccional -> secuencia-bidireccional
;; insert-to-right inserta el number en la secuencia bidireccional a la derecha
(define insert-to-right
  (lambda (numero secuencia-bidireccional)
    (list (car secuencia-bidireccional)(flatten(list (cadr secuencia-bidireccional)))(flatten(list numero(cddr secuencia-bidireccional))))
    )
  )

;;at-left-end?: secuencia-bidireccional -> boolean
;;at-left-end?: especifica si la secuencia bidireccional se encuentra a la izquierda
(define at-left-end?
  (lambda (secuencia-bidireccional)
    (cond
      [(null? (cadr secuencia-bidireccional))#t]
      [else #f]
      )
    )
  )

;;at-right-end?: secuencia-bidireccional -> boolean
;;at-right-end? especifica si la secuencia bidireccional se encuentra a la derecha
(define at-right-end?
  (lambda (secuencia-bidireccional)
    (cond
      [(null? (caddr secuencia-bidireccional))#t]
      [else #f]
      )
    )
  )
