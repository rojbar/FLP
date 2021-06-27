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

;;empty-stack: -> stack
;;empty-stack retorna una stack-vacia
(define empty-stack
  (lambda() (list 'empty-stack)))

;;push: element stack -> stack
;;push retorna una stack formada por el elemento y el stack pasado, lo añade en la primera posicion
(define push
  (lambda(elm stack)
    (list 'stack-value elm stack)))

;;pop: stack -> stack
;;pop quita el primer elemento de la lista y retorna el resto del stack
(define pop
  (lambda (stack)
    (if (empty-stack? stack)
        (eopl:error "No puedes quitar un elemento de un stack vacio")
        (if(eqv? (car stack) 'stack-value)
           (caddr stack)
           (eopl:error "Mal Formed Stack")
        ))))

;;top: stack -> element
;;top retorna el primer elemento del stack
(define top
  (lambda (stack)
    (if (empty-stack? stack)
        (eopl:error "No puedes hallar un elemento de un stack vacio")
        (if(eqv? (car stack) 'stack-value)
           (cadr stack)
           (eopl:error "Mal Formed Stack")
        ))))

;;empty-stack?: stack -> boolean
;;empty-stack? determina si un stack esta vacio
(define empty-stack?
  (lambda(stack)
    (and(eqv? (car stack) 'empty-stack)
        (null? (cdr stack)))))


