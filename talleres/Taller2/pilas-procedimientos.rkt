#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARTICIPANTES                                 ;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rojbar                                        ;;
;; Matomaral                                     ;;
;; thiagoc789                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAMATICA
;; <stack>  ::= (empty-stack)
;;          ::= (stack-value <scheme-value> <stack>)

(define stack
  (lambda(val sta)
    (lambda(signal)
      (cond
        [(= signal 1) 'stack-value]
        [(= signal 2) val]
        [(= signal 3) sta]
        ))))

;;empty-stack: -> stack
;;empty-stack retorna una stack-vacia
(define empty-stack
  (lambda()
    (lambda (signal)
    (cond
      [(= signal 1) 'empty-stack]
      [else "Stack vacio"]))))

;;stack?: stack -> boolean
;;stack? determina si es un stack no vacio
(define stack?
  (lambda(proc)
    (eqv? (proc 1) 'stack-value)))

;;empty-stack?: stack -> boolean
;;empty-stack? determina si un stack esta vacio
(define empty-stack?
  (lambda(proc)
    (eqv?(proc 1)'empty-stack)))

;;top: stack -> element
;;top retorna el primer elemento del stack
(define top
  (lambda(proc)
    (proc 2)))

;;pop: stack -> stack
;;pop quita el primer elemento de la lista y retorna el resto del stack
(define pop
  (lambda(proc)
    (proc 3)))

;;push: element stack -> stack
;;push retorna una stack formada por el elemento y el stack pasado, lo añade en la primera posicion
(define push
  (lambda(n s)
    (stack n s)))








