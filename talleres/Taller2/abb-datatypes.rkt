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

;; REPRESENTACION POR DATATYPE
(define-datatype arbol-binario arbol-binario?
  (arbol-vacio)
  (nodo (numero number?) (arbol-izq arbol-binario?) (arbol-der arbol-binario?))
)

;;EXTRACTORES

;; nodo->val: arbol-binario -> number
;; nodo->val recibe un arbol-binario y retorna el numero del nodo
(define nodo->val
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio () (eopl:error 'nodo->val "arbol-vacio no posee nodo con valor"))
      (nodo (numero arbol-izq arbol-der) numero)
    )
  )
)

;; hijo-izq->arbol: arbol-binario -> arbol-binario
;; hijo-izq->arbol retorna el arbol-izquierdo del arbol-binario pasado
(define hijo-izq->arbol
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio () arbol-vacio)
      (nodo (numero arbol-izq arbol-der) arbol-izq)
    )
  )
)

;; hijo-der->arbol: arbol-binario -> arbol-binario
;; hijo-der->arbol retorna el arbol-derecho del arbol-binario pasado
(define hijo-der->arbol
  (lambda (arbol)
    (cases arbol-binario arbol
      (arbol-vacio () arbol-vacio)
      (nodo (numero arbol-izq arbol-der) arbol-der)
    )
  )
)
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

;; UNPARSER
;; unparse: sintaxis-abstracta -> sintaxis-concreta
;; unparse:  toma un arbol-binario escrito en sintaxis abstracta y la pasa a sintaxis concreto
(define unparse
  (lambda (entrada)
    (cases arbol-binario entrada
      (arbol-vacio () '())
      (nodo (numero arbol-izq arbol-der) (list numero (unparse arbol-izq) (unparse arbol-der)))
    )
  )
)

;; PARSER
;; parse: sintaxis-concreta -> sintaxis-abstracta
;; parse toma una suma-anidada escrita en sintaxis concreta y la pasa a sintaxis abstracta
(define parse
  (lambda (entrada)
    (cond
      [(null? entrada) (arbol-vacio)]
      [(number? (car entrada)) (nodo (car entrada) (parse (car (cdr entrada))) (parse (car (cddr entrada))))]
    )
  )
)
