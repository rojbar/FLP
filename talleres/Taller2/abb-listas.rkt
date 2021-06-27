#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARTICIPANTES                                 ;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rojbar                                        ;;
;; Matomaral                                     ;;
;; thiagoc789                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GRAMATICA
;;<arbol-binario> := (arbol-vacio) empty
;;                := (nodo) <numero> <arbol-binario> <arbol-binario>

;; REPRESENTACION POR LISTAS

;; Constructores

;; arbol-vacio: -> empty
;; arbol-vacio retorna una lista vacia y no recibe argumentos
(define arbol-vacio (lambda () empty))

;; nodo: (number arbol-binario arbol-binario)-> list
;; nodo recibe un numero y dos arboles binarios formando una lista
(define nodo (lambda (numero arbol-binario1 arbol-binario2) (list numero arbol-binario1 arbol-binario2)))

;; Predicados

;; arbol-vacio?: arbol-binario -> boolean
;; arbol-vacio? recibe un arbol-binario y dice si es un arbol-vacio
(define arbol-vacio?(lambda (arbol) (equal? arbol (arbol-vacio))))

;; arbol-hoja?: arbol-binario -> boolean
;; arbol-hoja? recibe un arbol-binario y dice si es un arbol-hoja? (un arbol-hoja es aquel un nodo con arbol-vacio en los hijos izquierdo y derecho)
(define arbol-hoja? (lambda (arbol) (and (not (arbol-vacio? arbol)) (equal? (hijo-izq->arbol arbol) (arbol-vacio)) (equal? (hijo-der->arbol arbol) (arbol-vacio)))))

;; arbol-nodo?: arbol-nodo -> boolean
;; arbol-nodo? recibe un arbol-binario y dice si es un arbol-nodo (un arbol-nodo es aquel nodo con almenos un arbol-vacio en los hijos izquierdo y derecho)
(define arbol-nodo? (lambda (arbol) (and (not (arbol-vacio? arbol)) (not (arbol-hoja? arbol)))))

;; Extractores

;; añadir condiciones por ejemplo en nodo->val no debe poder retornar valor si es un arbol vacio
;; nodo->val: arbol-binario -> number
;; nodo->val recibe un arbol-binario y retorna el numero del nodo
(define nodo->val (lambda (arbol) (car arbol))) 

;; hijo-izq->arbol: arbol-binario -> arbol-binario
;; hijo-izq->arbol retorna el arbol-izquierdo del arbol-binario pasado
(define hijo-izq->arbol (lambda (arbol) (car (cdr arbol))))

;; hijo-der->arbol: arbol-binario -> arbol-binario
;; hijo-der->arbol retorna el arbol-derecho del arbol-binario pasado
(define hijo-der->arbol (lambda (arbol) (car (cddr arbol))))

;; validador-orden:
;; validador-orden
(define validador-orden
  (lambda (arbol)
    (cond
      [(arbol-vacio? arbol) (eopl:error 'validador-orden "es un arbol vacio")]
      [else (listaOrdenada? (arbol->lista arbol))]
    )
    
  )
)
;; listaOrdenada?: list -> boolean
;; listaOrdenada? determina si  una lista esta ordenada de menor a mayor
(define (listaOrdenada? lista)
  (cond
    [(null? lista) #t]
    [(null? (cdr lista)) #t]
    [(< (car lista) (cadr lista)) (listaOrdenada? (cdr lista))]
    [else #f]
  )
)
;; arbol->lista: arbol -> list
;; arbol->lista convierte un arbol es una lista
(define (arbol->lista arbol)
  (cond
    [(arbol-vacio? arbol)  empty]
    [else (append (arbol->lista (hijo-izq->arbol arbol)) (cons (nodo->val arbol)empty) (arbol->lista (hijo-der->arbol arbol)))]
  )
 )
;; insertar-elemento: arbol-binario number -> arbol-binario
;; insertar-elemento es una funcion que recibe un arbol-binario y un numero y retorna un nuevo arbol insertando el numero en la posicion correcta
(define insertar-elemento
  (lambda (arbol elemento)
    (cond
      [(arbol-vacio? arbol) (nodo elemento (arbol-vacio) (arbol-vacio))]
      [(equal? (nodo->val arbol) elemento) arbol]
      [(> elemento (nodo->val arbol)) (nodo (nodo->val arbol) (hijo-izq->arbol arbol) (insertar-elemento (hijo-der->arbol arbol) elemento))]
      [(< elemento (nodo->val arbol)) (nodo (nodo->val arbol) (insertar-elemento (hijo-izq->arbol arbol) elemento) (hijo-der->arbol arbol))]
    )
  )
)
