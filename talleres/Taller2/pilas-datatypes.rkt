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
(define-datatype stack stack?
  (empty-stack)
  (stack-value (bound-var scheme-value?)
               (bound-stack stack?)))

;;scheme-value?: element -> boolean
;;scheme-value? retorna verdadero o falso si el elemento es un bvalor de scheme
(define scheme-value?
  (lambda (x)
    #t))

;;push: element stack -> stack
;;push retorna una stack formada por el elemento y el stack pasado, lo añade en la primera posicion
(define push
  (lambda(n s)
    (cases stack s
      (empty-stack () (stack-value n (empty-stack)))
      (stack-value (bound-var bound-stack) (stack-value n s)))))

;;pop: stack -> stack
;;pop quita el primer elemento de la lista y retorna el resto del stack
(define pop
  (lambda(s)
    (cases stack s
      (empty-stack ()
                   (eopl:error "No puedes retirar un elemento de una pila vacia")) 
      (stack-value (bound-var bound-stack) bound-stack))))

;;top: stack -> element
;;top retorna el primer elemento del stack
(define top
  (lambda(s)
    (cases stack s
       (empty-stack () (eopl:error "No puedes retirar un elemento de una pila vacia"))
       (stack-value (bound-var bound-stack) bound-var))))

;;empty-stack?: stack -> boolean
;;empty-stack? determina si un stack esta vacio
(define empty-stack?
  (lambda(s)
    (cases stack s
      (empty-stack () #t)
      (stack-value (bound-var bound-stack)#f))))
