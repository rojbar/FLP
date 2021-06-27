#lang eopl
#| 
 rojbar
 Matomara
 thiagoc789
|#                                
#|
  Especificación Léxica
|#
(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("//" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit))) symbol)
    (integer
     (digit (arbno digit)) number)
    (integer
     ("-" digit (arbno digit)) number)
    (float
     (digit (arbno digit)"."digit (arbno digit)) number)
    (float
     ("-" digit (arbno digit)"."digit (arbno digit)) number)
    (character
     ("'" (or letter digit) "'") string)
    (string
     ("`" (arbno (or letter digit whitespace)) "`") string) 
   )
)
;;Especificación Sintáctica (gramática)
(define grammar-simple-interpreter
  '(
    (program ((arbno class-decl) expression) starts-program)
    ;; OBJETOS
    ;; DECLARACION DE CLASE
     (class-decl                         
      ("class" identifier 
        "extends" identifier                   
         (arbno "field" identifier)
         (arbno method-decl)
         )
      a-class-decl)
     ;; METODOS
        (method-decl
         ("method" identifier 
                   "("  (separated-list identifier ",") ")" ; method ids
                   expression 
         )
      a-method-decl)

    ;; CREACION DE OBJETOS
            (expression 
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)
     ;; LLAMADO A OBJETOS
    (expression
      ("send" expression identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)
    ;; LLAMADO SUPER
    (expression                                
      ("super" identifier    "("  (separated-list expression ",") ")")
      super-call-exp)

     
    ;; IDENTIFICADOR
    (expression (identifier) identifier-exp)
    ;; PASO POR REFERENCIA
    (expression ("&" identifier) reference-exp)
    ;; DATOS
    (expression (num) number-exp)
    (expression (string) string-exp)
    (expression (expr-bool) expr-bool-exp)
    (expression (character) character-exp)
    (expression ("x16(" (arbno expression)")") hexadecimal-exp)
    (expression ("print("expression")") print-exp)
    ;; INSTANCIAS SAT
    (expression ("FNC" integer "(" (separated-list clausula-or "and") ")") instancia-sat-exp)
    (clausula-or ("("  (separated-list integer "or") ")") clausula-or-exp)
    (expression ("$"identifier".solve()") solve-instancia-sat-exp)
    ;; DATOS PREDEFINIDOS
    (expression ("list(" (separated-list expression ",") ")") list-exp)
    (expression ("vector("(separated-list expression ",") ")") vector-exp)
    (expression ("dictionary("(separated-list identifier "->" expression ";") ")")dictionary-exp)
    ;; ASIGNACION DE VARIABLES
    (expression ("set" identifier "=" expression) set-exp)
    ;; PRIMITIVAS
    (expression ("[" primitive (separated-list expression ",") "]") primitive-exp)
    ;; DEFINICION DE DATOS 
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) var-exp)
    (expression ("cons" (separated-list identifier "=" expression ",") "in" expression) cons-exp)
    ;; ESTRUCTURAS DE CONTROL
    (expression ("begin" expression ";" (separated-list expression ";")"end") begin-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("while" expression "do" expression) while-exp)
    (expression ("for" "(" identifier "=" expression ";" to expression ")" "do" expression) for-exp)
    ;; PROCEDIMIENTOS
    (expression ("procedure" "("(separated-list identifier ",") ")" "{" expression "}") procedure-exp)
    (expression ("invoke" expression "("  (separated-list expression ",") ")") procedure-call-exp)
    (expression ("recursive" (separated-list identifier "(" (separated-list identifier ",") ")" "=" expression ";" ) "in" expression) recursive-exp)
    ;; DEFINICION DE NUMEROS 
    (num (integer) integer->number)
    (num (float)   float->number)
    ;; DEFINIICION DE TO|DOWNTO
    (to ("to") to-for)
    (to ("downto") down-for)
    ;; EXPRESIONES BOOLEANAS
    (expr-bool (pred-prim "(" expression "," expression ")") pred-expr-bool)
    (expr-bool (binary-boolean-primitive "(" expression "," expression")") binary-expr-bool)
    (expr-bool (unary-boolean-primitive "(" expression")") unary-expr-bool)
    (expr-bool (boolean) boolean-expr-bool)
    ;; VALORES BASE BOOLEANOS
    (boolean ("true") true->boolean)
    (boolean ("false")false->boolean)
    ;; PRIMITIVAS PARA BOOLEANOS
    (binary-boolean-primitive ("&&") and-boolean-primitive)
    (binary-boolean-primitive ("||")  or-boolean-primitive)
    (unary-boolean-primitive ("not") not-boolean-primitive)
    (pred-prim ("<") <-prim)
    (pred-prim (">") >-prim)
    (pred-prim (">=") >=-prim)
    (pred-prim ("<=") <=-prim)
    (pred-prim ("==") ==-prim)
    (pred-prim ("<>") <>-prim)
    ;; PRIMITIVAS PARA NUMEROS
    (primitive ("+") plus-number-prim)
    (primitive ("-") minus-number-prim)
    (primitive ("*") multi-number-prim)
    (primitive ("/") div-number-prim)
    (primitive ("%") mod-number-prim)
    (primitive ("add1") add1-number-prim)
    (primitive ("sub1") sub1-number-prim)
    ;; PRIMITIVAS PARA STRINGS
    (primitive ("concat") concat-string-prim)
    (primitive ("length") length-string-prim)
    ;; PRIMITIVAS PARA LISTAS
    (primitive ("create-list") create-list-prim)
    (primitive ("append") append-list-prim)
    (primitive ("empty?") empty?-list-prim)
    (primitive ("list?") list?-list-prim)
    (primitive ("head") head-list-prim)
    (primitive ("tail") tail-list-prim)
    (primitive ("empty") empty-list-prim)
    ;; PRIMITIVAS PARA VECTORES                                                                  
    (primitive ("ref-vector") ref-vector-prim)
    (primitive ("set-vector") set-vector-prim)
    (primitive ("vector?") vector?-vector-prim)
    (primitive ("create-vector") create-vector-prim) 
    ;; PRIMITIVAS PARA DICTIONARYS                                                                  
    (primitive ("dictionary?") dictionary?-dictionary-prim)
    (primitive ("dictionary-set") set-dictionary-prim)
    (primitive ("dictionary-ref") ref-dictionary-prim)
    (primitive ("create-dictionary") create-dictionary-prim)   
  )
)
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)
(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)
  )
)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter)
)
;; INTERPRETADOR                                                                  
(define interpreter
  (sllgen:make-rep-loop "-->" (lambda (pgm) (eval-program  pgm)) (sllgen:make-stream-parser scanner-spec-simple-interpreter grammar-simple-interpreter))
)
;; EVAL-PROGRAM
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (starts-program (c-decls body)
        (elaborate-class-decls! c-decls)
        (eval-expression body (init-env))
      )
    )
  )
)
;; AMBIENTE INICIAL
(define init-env
  (lambda ()
    (extend-env '() '() 
     (empty-env)
    )
  )
)
;; EVAL-EXPRESSION
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (method-app-exp             (obj-exp method-name rands)             (let ((args (evaluacion-expresiones-listas rands env)) (obj (eval-expression obj-exp env))) (find-method-and-apply method-name (object->class-name obj) obj args)))
      (new-object-exp             (class-name args)                       (let ((args (evaluacion-expresiones-listas args env))
                            (obj (new-object class-name)))
                        (find-method-and-apply
                         'initialize class-name obj args)
                        obj))
       
      (super-call-exp             (method-name rands)                     (let ((args (evaluacion-expresiones-listas rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))
      
      (identifier-exp             (id)                                    (apply-env env id))                                                                                      ;; Implementada
      (number-exp                 (numb)                                  (evaluacion-expresiones-numeros numb))                                                           ;; Implementada
      (expr-bool-exp              (expres-bol)                            (evaluacion-expresiones-booleanas expres-bol env))                                               ;; Implementada
      (string-exp                 (str)                                   (evaluacion-expresiones-cadenas str))                                                            ;; Implementada
      (character-exp              (char)                                  (evaluacion-expresiones-caracteres char))                                                        ;; Implementada
      (hexadecimal-exp            (hex)                                   (evaluacion-expresiones-hexadecimales hex env))                                                  ;; Implementada
      (list-exp                   (expr-list)                             (evaluacion-expresiones-listas expr-list env))                                                    ;; Implementada
      (vector-exp                 (expr-vec)                              (evaluacion-expresiones-vectores expr-vec env))                                                   ;; Implementada
      (dictionary-exp             (ids exps)                              (evaluacion-expresiones-diccinarios ids exps env))                                                ;; Implementada
      (print-exp                  (ex)                                    (display  (eval-expression ex env)))                                                                         ;; Implementada
      (set-exp                    (id expr)                               (evaluacion-expresiones-set id expr env))                                                                  ;; Implementada
      (primitive-exp              (prim list-expres)                      (evaluacion-expresiones-primitivas prim list-expres env))                                   ;; Implementada
      (reference-exp              (ref)                                   
                                                                         (cases reference (apply-env-ref env ref)
                                                                          (a-ref (pos vals mut) 
                                                                                (if (target? (vector-ref vals pos) )
                                                                                    (vector-ref vals pos)
                                                                                    (indirect-target (apply-env-ref env ref) )    
                                                                                ) 
                                                                          )
                                                                         )     
      )                                                               ;; Implementada
      (var-exp                    (vars vals body)                        (evaluacion-expresiones-var vars vals body env))                                                    ;; Implementada
      (cons-exp                   (vars vals body)                        (evaluacion-expresiones-cons vars vals body env))                                                   ;; Implementada
      (begin-exp                  (expr exp-lists)                        (evaluacion-expresiones-begin expr exp-lists env))                                                ;; Implementada
      (while-exp                  (bool-exp body)                         (evaluacion-expresiones-while bool-exp body env))                                                  ;; Implementada
      (procedure-exp              (ids body)                              (evaluacion-expresiones-procedure ids body env))                                                    ;; Implementada
      (if-exp                     (bool-exp true-expr false-expr)         (evaluacion-expresiones-if bool-exp true-expr false-expr env))                        ;; Implementada
      (procedure-call-exp         (expr args)                             (evaluacion-expresiones-call-procedure expr args env))                                        ;; Implementada
      (instancia-sat-exp          (first-int clauses)                     (evaluacion-expresiones-sat first-int clauses ) )                                      ;; Implementada
      (solve-instancia-sat-exp    (id)                                    (evaluacion-expresiones-solve-sat id env))                                                      ;; Implementada
      (for-exp                    (id init-value goto final-value body)   (evaluacion-expresiones-for id init-value goto final-value body env))          ;; Implementada      
      (recursive-exp              (proc-names idss bodies letrec-body)    (evaluacion-expresiones-recursive proc-names idss bodies letrec-body env)) ;; Implementada
    )
  )
)
;; IMPLEMENTACION RECURSIVE
(define evaluacion-expresiones-recursive
  (lambda ( proc-names idss bodies letrec-body env)
    (eval-expression letrec-body (extend-env-recursively proc-names idss bodies env))
  )
)
;; función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record (map (lambda (id) (mutable id))proc-names) vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env))
            )
            (iota len) idss bodies
          )
          env
      )
     )
   )
  )
)
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;; DEFINICION DE TIPOS DE DATOS PROPIOS
;; TIPO DE DATO VALOR DE VERDAD
;; VALOR TRUE
(define true-value 'true)
;; VALOR FALSE
(define false-value 'false)
;; ES VERDAD?
(define isTrue?
  (lambda (x)
    (equal? x true-value)
  )
)
;; TIPO DE DATO INSTANCIA SAT
;; INSTANCIA SAT
(define-datatype sat sat?
  (instancia-sat (n integer?) (lista list?))
)
;; TIPO DE DATO ARBOL-BINARIO-SAT
(define-datatype arbol-sat arbol-sat?
  (nodo (value number?) (arbol-izquierda arbol-sat?) (arbol-derecha arbol-sat?))
  (arbol-vacio)
)
;; IMPLEMENTACION FOR
;; evaluacion-expresiones-for: identifier, expression, (to), expression, expression, enviroment -> integer
;; evaluacion-expresiones-for es una funcion que implementa el comportamiento de una estructura de control for, para eso extiende el ambiente actual
;; con el id que se le pasa y el first-expression, y llama la función auxiliar for recursivo.
(define evaluacion-expresiones-for
  (lambda (id init-value goto final-value body env)
    (for-recursivo id final-value body goto (extend-env (list (mutable id)) (list (eval-expression init-value env)) env))
  )
)
;; for-recursivo: id, expression, expression, (to), enviroment -> integer
;; for-recursivo: evalua el valor del id comparado con el final-value, de ser verdad la condición decidad por cada caso
;; de (to) ejecuta el body y despues se vuelve a llamar, de no ser verdad retorna 1
(define for-recursivo
  (lambda (id final-value body goto env)
    (cases to goto
      (to-for () (if (< (apply-env env id ) (eval-expression final-value env)) (begin (eval-expression body env) (for-recursivo id final-value body goto env)) 1)   )
      (down-for () (if (> (apply-env env id ) (eval-expression final-value env)) (begin (eval-expression body env) (for-recursivo id final-value body goto env)) 1) )
    )   
  )
)
;; IMPLEMENTACION INSTANCIA SAT
;; evaluacion-expresiones-sat: int,(list-of clausulas-or)-> instancia-sat
;; evaluacion-expresiones-sat retorna un objeto de tipo sat, el cual encapsula el primer entero que representa la cantidad de variables, y las clausulas-or
(define evaluacion-expresiones-sat
  (lambda (first-int clauses )
    (instancia-sat first-int (pasar-clausulas-or clauses))
  )
)
;; pasar-clausulas-or: (list-of clausulas-or) -> (list-of (list-of number))
;; pasar-clausulas-or pasa una lista formada por objetos de tipo clausula-or a una lista formada por listas donde cada lista es una clausula-or distinta
(define pasar-clausulas-or
  (lambda (clausulas)
    (cond
      ((null? clausulas) empty)
      (else
       (cons (cases clausula-or (car clausulas) (clausula-or-exp (nums) nums)) (pasar-clausulas-or (cdr clausulas)))            
      )
    )
  )
)
;; IMPLEMENTACIÓN SOLVE INSTANCIA SAT EXP
;; evaluacion-expresiones-solve-sat; identifier, enviroment -> (or (list-of boolean) string)
;; evaluacion-expresiones-solve-sat retorna la combinación de valores booleanos que dan solución al problema o una string en caso de que no exista solución,
;; en caso de no ser una instancia-sat a lo que apunta el id se debe retornar un error
(define evaluacion-expresiones-solve-sat
  (lambda (id env)
      (if (sat? (apply-env env id))
          (cases sat (apply-env env id)
            (instancia-sat (n clausulas-or)
                           (begin
                             (define combinaciones (generar-todas-las-combinaciones n))
                             (define resultado (evaluar-combinaciones-hasta-correcta combinaciones clausulas-or 0))
                             (if (string? resultado) (list resultado) (map (lambda (id) (if (equal? id 1) true-value false-value)) resultado) )
                           )            
            )
          )
          (eopl:error 'evaluacion-expresiones-solve-sat "Not a SAT instance ~s")
      )
  )
)
;; evaluar-combinaciones-hasta-correcta: vector, list -> (string or (list-of number))
;; evaluar-combinaciones-hasta-correcta retornar una string en caso de que no exista combinación que tenga solución o una lista de numeros (0 o 1)
;; que representa una solución al problema
(define evaluar-combinaciones-hasta-correcta
  (lambda (combinaciones clausulas-or pos)
    (if (equal? pos (vector-length combinaciones))
        "No tiene solucion"
        (if (evaluar-combinacion (list->vector (vector-ref combinaciones pos)) clausulas-or )
            (vector-ref combinaciones pos)
            (evaluar-combinaciones-hasta-correcta combinaciones clausulas-or (+ pos 1))
        )
    )
  )
)
;; evaluar-combinacion: vector x list -> boolean
;; evaluar-combinacion retorna true si todas las clausulas-or en la lista son verdad al ser evaluadas para la combinación dada
(define evaluar-combinacion
  (lambda (combinacion clausulas-or)
    (if (null? clausulas-or)
        #t
        (and (evaluar-clausula combinacion (car clausulas-or)) (evaluar-combinacion combinacion (cdr clausulas-or)))
    )
  )
)
;; evaluar-clausula: vector x list -> boolean
;; evaluar-clausula  retorna true si al menos un valor dentro de la clausula or es verdad
(define evaluar-clausula
  (lambda  (combinacion clausula)
    (if (null? clausula)
        #f
        (or (traducir-valor-de-verdad combinacion (car clausula)) (evaluar-clausula combinacion (cdr clausula)))
    )
  )
)
;; traducir-valor-de-verdad: vector,int -> boolean
;; traducir-valor-de-verdad recibe un vector que puede tener numeros enteros distintos a 0 y retorna el valor de verdad asignado a ese numero en el vector,
;; en caso de que el numero sea negativo se retorna el opuesto al valor de verdad.
(define traducir-valor-de-verdad
  (lambda (vec pos)
    (if (< pos 0)
        (not (obtener-valor-de-verdad vec (- (* pos -1) 1 )))
        (obtener-valor-de-verdad vec (- pos 1))
    )
  )
)
;; obtener-valor-de-verdad: vector,int -> boolean
;; obtener-valor-de-verdad retorna true si el valor a que se hace referencia en el vector es 1, false si es 0
(define obtener-valor-de-verdad
  (lambda (vec pos)
    (begin
      (define val (vector-ref vec pos))
      (if (equal? val 1) #t #f)
    )
  )
)
;; aux-generar-arbol: integer, n -> arbol-sat
;; aux-generar-arbol retorna un arbol-sat de n niveles, donde val es el valor de la raiz, y todos los arboles izquierdos tienen 1
;; y todos los arboles-derechos tienen 0
(define aux-generar-arbol
  (lambda (val n)
    (cond
      ((equal? n 0) (arbol-vacio))
      (else (nodo val (aux-generar-arbol 1 (- n 1)) (aux-generar-arbol 0 (- n 1))))
     )
   )
)
;; generar-arbol-con-combinaciones: integer -> arbol-sat
;; generar-arbol-con-combinaciones retorna un arbol-sat de n niveles, donde la raiz es -1 y todos los arboles izquierdos tienen 1
;; y todos los arboles-derechos tienen 0 
(define generar-arbol-con-combinaciones
  (lambda (n)
    (cond
      ((equal? n 0) (arbol-vacio))
      (else (nodo -1 (aux-generar-arbol 1 n) (aux-generar-arbol 0 n)) )
      )
  )
)
;; MEJORAR CODIGO
;; generar-todas-las-combinaciones: int -> vector
;; generar-todas-las-combinaciones retorna un vector formado por listas, el vector posee cada una de las combinaciones de valores booleanos
(define generar-todas-las-combinaciones
  (lambda (first-int)
    (begin
      (define combinaciones (generar-arbol-con-combinaciones first-int) )
      (define vector-combinaciones (make-vector (expt 2 first-int)) )
      (define posicion-vector 0 )
      (define vector-push
        (lambda (val)
          (begin
            (vector-set! vector-combinaciones posicion-vector val)
            (set! posicion-vector (+ posicion-vector 1))
            )
          )
       )
      (define buscar-combinaciones
        (lambda (arbol-binario list)
          (cases arbol-sat arbol-binario
            (nodo (val izq der)
                  (if (equal? val -1)
                      (begin (buscar-combinaciones izq list) (buscar-combinaciones der list))
                      (if (equal? izq (arbol-vacio)) (vector-push (cons val list)) (begin (buscar-combinaciones izq (cons val list)) (buscar-combinaciones der (cons val list))))  )
                      
                  )
            (default #f )
            ) 
        )
       )
      (buscar-combinaciones combinaciones '())
       vector-combinaciones
    )
  )
)

;; IMPLEMENTACION IF
;; evaluacion-expresiones-if: expression, expression, expression, enviroment -> (eval-expression expression)
;; evaluacion-expresiones-if simula la estructura de control if, para ello retorna la evaluación de la expresión true-expr en caso de que sea true bool-exp
;; retorna la evaluación de la expresión false-expr en caso de que no
(define evaluacion-expresiones-if
  (lambda (bool-exp true-expr false-expr env)
    (if (isTrue? (eval-expression bool-exp env)) (eval-expression  true-expr env) (eval-expression false-expr env))
  )
)
;; IMPLEMENTACION VAR
;; evaluacion-expresiones-var: (list-of variable), (list-of expression), expression, enviroment -> (eval-expression body)
;; evaluacion-expresiones-var retorna la evaluación del cuerpo en el ambiente extendido de la lista de variables (mutables para este caso) y lista de valores
;; que son evaluados junto a env
(define evaluacion-expresiones-var
  (lambda (vars vals body env)
    (eval-expression body (extend-env (map (lambda (var) (mutable var)) vars )  (evaluacion-expresiones-listas vals env) env) )
  )
)
;; IMPLEMENTACION CONS
;; evaluacion-expresiones-cons: (list-of variable), (list-of expression), expression, enviroment -> (eval-expression body)
;; evaluacion-expresiones-cons igual que var, pero las vairables son inmutables
(define evaluacion-expresiones-cons
  (lambda (vars vals body env)
    (eval-expression body (extend-env (map (lambda (var) (inmutable var))vars)  (evaluacion-expresiones-listas vals env) env))
  )
)
;; IMPLEMENTACION  SET
;; evaluacion-expresiones-set: id, expr, env -> 1
;; evaluacion-expresiones-set implementa expresiones de asignación, solo las variables mutables pueden ser asginadas, así como el paso
;; por referencia de variables mutables, de intentar modificar una variable inmutable arroja error
(define evaluacion-expresiones-set
  (lambda (id expr env)
     (begin
       (if  (es-mutable? id env)
            (cases reference (apply-env-ref env id)
              (a-ref (pos vals mut)  
                  (if (target? (vector-ref vals pos))
                    (cases target (vector-ref vals pos) 
                      (indirect-target (ref)  (setref! ref (eval-expression expr env)))
                    )
                  (setref! (apply-env-ref env id) (eval-expression expr env))
                )
              )
            )      
            (eopl:error 'set-exp "No se puede modificar constante ~s" id)
       )
       1
     )
  )
)
;; IMPLEMENTACION BEGIN
;; evaluacion-expresiones-begin: expression,(list-of expression), env -> (eval-expression last-expression)
;; evaluacion-expresiones-begin retorna la ultima expresión evaluada en el ambiente
(define evaluacion-expresiones-begin
  (lambda (primera-exp lista-de-expresiones env)
     (if (null? lista-de-expresiones) (eval-expression primera-exp env)
         (begin (eval-expression primera-exp env) (evaluacion-expresiones-begin (car lista-de-expresiones) (cdr lista-de-expresiones) env)))
  )
)
;; IMPLEMENTACION PRIMITIVAS
;; evaluacion-expresiones-primitivas: primitive list-expres env -> (valor expresado)
;; evaluacion-expresiones-primitivas retorna el resultado de evaluar una primitiva de una lista de expresiones evaluadas
(define evaluacion-expresiones-primitivas
  (lambda (prim list-expres env)
    (let ([exprs  (evaluacion-expresiones-listas list-expres env)] )
    (cases primitive prim
      (plus-number-prim     ()                 
                                (if (and (list? (car exprs)) (list? (cadr exprs))) 
                                    (suma-hex (car exprs) (cadr exprs))
                                    (+ (car exprs) (cadr exprs))  
                                )
      ) 
      (minus-number-prim    ()    
                                (if (and (list? (car exprs)) (list? (cadr exprs)))  
                                    (resta-hex (car exprs) (cadr exprs))
                                    (-   (car exprs) (cadr exprs))
                                )
      ) 
      (multi-number-prim    ()   
                                (if (and (list? (car exprs)) (list? (cadr exprs)))  
                                    (multiplicacion-hex (car exprs) (cadr exprs))
                                    (* (car exprs) (cadr exprs))
                                )
      )   
      (div-number-prim      ()    (/ (car exprs) (cadr exprs))) 
      (mod-number-prim      ()    (modulo (car exprs) (cadr exprs)))
      (add1-number-prim     ()    
                                (if (list? (car exprs))
                                  (successor (car exprs))
                                  (+ (car exprs) 1)
                                )
      )              
      (sub1-number-prim     ()    
                                (if (list? (car exprs))
                                  (predecessor (car exprs))
                                  (- (car exprs) 1)
                                )
      )             
      (concat-string-prim   ()    (string-append (car exprs)(cadr exprs)))
      (length-string-prim   ()    (string-length (car exprs)))
      
      (create-list-prim     ()    (cons (car exprs) (cadr exprs)) ) ;; revisar
      (append-list-prim     ()    (append (car exprs) (cadr exprs)) )
      (empty?-list-prim     ()    (if (equal? (car exprs) empty) true-value false-value))
      (list?-list-prim      ()    (if (list? (car exprs)) true-value false-value ))
      (head-list-prim       ()    (caar exprs))
      (tail-list-prim       ()    (cdr(car exprs)))
      (empty-list-prim      ()    empty)
      
      (ref-vector-prim      ()    (vector-ref (car exprs) (cadr exprs)))
      (set-vector-prim      ()    (begin (vector-set! (car exprs) (cadr exprs) (caddr exprs)) 1))
      (vector?-vector-prim  ()    (if (vector? (car exprs)) true-value false-value))
      (create-vector-prim   ()    (make-vector (car exprs) 0))
      
      (dictionary?-dictionary-prim  () (if (diccionario? (car exprs)) true-value false-value))
      (set-dictionary-prim          () (if (diccionario? (car exprs)) (cases diccionario (car exprs) (diccionario-base ( ids vals) (begin (set-dictionary (cadr exprs) (caddr exprs) ids vals) 1) ) )  (eopl:error "No es un diccionario")  ) )
      (ref-dictionary-prim          () (if (diccionario? (car exprs)) (cases diccionario (car exprs) (diccionario-base (ids vals)  (get-dictionary (cadr exprs) ids vals )   ) ) (eopl:error "no es un diccionario") ) )
      (create-dictionary-prim       () (diccionario-base (car exprs) (list->vector (cadr exprs)) )) 
    )
   )
  )
)
;; IMPLEMENTACION WHILE
;; evaluacion-expresiones-while: expression, expression, enviroment -> 1
;; evaluacion-expresiones-while evalua el body en el ambiente mientras se cumpla el valor de verdad de la primera expresion
(define evaluacion-expresiones-while
        (lambda (bool-exp body env)
          (if (isTrue? (eval-expression bool-exp env)) (begin (eval-expression body env) (evaluacion-expresiones-while bool-exp body env) ) 1)
        )
)
;; IMPLEMENTACION HEXADECIMAL
;; evaluacion-expresiones-hexadecimales: expression, enviroment -> (eval-expression expr)
;; evaluacion-expresiones-hexadecimales
(define evaluacion-expresiones-hexadecimales
  (lambda (expr env)
     (evaluacion-expresiones-listas expr env)
  )
)
(define suma-hex
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma-hex (predecessor x) y)))))

(define resta-hex
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta-hex  x (predecessor y))))))

(define multiplicacion-hex
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma-hex (multiplicacion-hex (predecessor x) y) y))
    ))
    



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

;; IMPLEMENTACION DICCIONARIOS
;; evaluacion-expresiones-diccinarios: (list-of symbols),(list-of expression),enviroment -> list
;; evaluacion-expresiones-diccinarios retorna una lista formada por dos listas, la primera es la lista de simbolos, la segunda la lista de valores expresados
;; asociados a cada simbolo
(define evaluacion-expresiones-diccinarios
  (lambda (ids exps env)
    (diccionario-base (map (lambda (id) (symbol->string id) ) ids)  (list->vector (evaluacion-expresiones-listas exps env)) )
  )
)
(define values? 
  (lambda (any)
    #t
  )
)
(define-datatype diccionario diccionario?
  (diccionario-base (ids (list-of string?)) (vals vector? ))
  
)
(define pos-simbolo 
  (lambda (simbolo lista pos)
    (cond
      [(null? lista) -1]
      [(equal? simbolo (car lista)) pos]
      [else (pos-simbolo simbolo (cdr lista) (+ pos 1))]
    )
  )

)
(define set-dictionary 
  (lambda (id val lista vec)
    (begin
      (define pos (pos-simbolo id lista 0))
      (if (equal? -1 pos) (eopl:error "invalid key") (vector-set! vec pos val))
    )
  
  )
)
(define get-dictionary
  (lambda (id  lista vec)
    (begin
      (define pos (pos-simbolo id lista 0))
      (if (equal? -1 pos) (eopl:error "invalid key") (vector-ref vec pos))
    )
  )
)
;; IMPLEMENTACION VECTORES
;; evaluacion-expresiones-vectores: (list-of expression), enviroment -> vector
;; evaluacion-expresiones-vectores retorna un vector de valores expresados
(define evaluacion-expresiones-vectores
  (lambda (expr-vec env)
    (let ([v (make-vector (length expr-vec))] [i 0])
      (begin
        (for-each (lambda (arg) (begin (vector-set! v i (eval-expression (list-ref expr-vec i) env)) (set! i (+ i 1)))) expr-vec )
       v
      )
    )
  )
)



;; IMPLEMENTACION LISTAS
(define evaluacion-expresiones-listas
 (lambda (exprs env)
  (cond
   ((null? exprs) empty)
   (else
      (cons (eval-expression (car exprs) env) (evaluacion-expresiones-listas (cdr exprs) env))
   )
  )
 )
)

(define aplicar-indirect-target
  (lambda (val)
    (cases target val (indirect-target (ref)  (primitive-deref ref)))
  )
)

;; IMPLEMENTACION CADENAS
;; evaluacion-expresiones-cadenas: string -> string
;; evaluacion-expresiones-cadenas recibe una cadena que siempre tiene  la forma "'valores'", este metodos se encarga de retornar la cadena de la forma
;; "valores"
(define evaluacion-expresiones-cadenas
  (lambda (str)
    (substring str 1 (- (string-length str) 1))
  )
)
;; IMPLEMENTACION CARACTERES
;; evaluacion-expresiones-caracteres: string -> symbol
;; evaluacion-expresiones-caracteres recibe una cadena formada por un
;; caracter que siempre tiene  la forma "'v'", este metodos se encarga de retornar la cadena de la forma
;; 'valor
(define evaluacion-expresiones-caracteres
  (lambda (str)
    (string->symbol (substring str 1 (- (string-length str) 1)))
  )
)
;;IMPLEMENTACION NUMEROS
;; evaluacion-expresiones-numeros: num -> number
;; evaluacion-expresiones-numeros recibe un (num (tipo de dato definido)) y retorna el numero que representa de tipo racket
(define evaluacion-expresiones-numeros
  (lambda (numb)
    (cases num numb
     (integer->number (numb) numb)
     (float->number (numb) numb)
    )
  )
)
;; IMPLEMENTACION BOOLEANOS
(define evaluacion-expresiones-booleanas
  (lambda (expr env)
    (cases  expr-bool expr
                       (pred-expr-bool (pred first-expr second-expr)
                                       (cases pred-prim pred
                                         (<-prim  () (if (<  (eval-expression first-expr env) (eval-expression second-expr env))           true-value false-value))
                                         (>-prim  () (if (>  (eval-expression first-expr env) (eval-expression second-expr env))           true-value false-value))
                                         (>=-prim () (if (>= (eval-expression first-expr env) (eval-expression second-expr env))           true-value false-value))
                                         (<=-prim () (if (<= (eval-expression first-expr env) (eval-expression second-expr env))           true-value false-value))
                                         (==-prim () (if (equal? (eval-expression first-expr env) (eval-expression second-expr env))       true-value false-value))
                                         (<>-prim () (if (not (equal? (eval-expression first-expr env) (eval-expression second-expr env))) true-value false-value))
                                       )
                       )
                       (binary-expr-bool (pred first-expr second-expr)
                                       (cases binary-boolean-primitive pred
                                         (and-boolean-primitive ()  (if (and (isTrue? (eval-expression first-expr env)) (isTrue? (eval-expression second-expr env))) true-value false-value))
                                         (or-boolean-primitive  ()  (if (or  (isTrue? (eval-expression first-expr env)) (isTrue? (eval-expression second-expr env))) true-value false-value)) 
                                       )
                       )
                       (unary-expr-bool (unary-prim  bool-exp)
                                       (cases unary-boolean-primitive unary-prim
                                          (not-boolean-primitive  () (if (isTrue? (eval-expression bool-exp env)) false-value true-value))
                                       )
                       )
                       (boolean-expr-bool (bool)
                                       (cases  boolean bool 
                                          (true->boolean  ()  true-value)
                                          (false->boolean ()  false-value)
                                       )
                       )
   )
  )
)

;; INTERFAZ PROCEDIMIENTOS
;; TIPO DE DATO procval
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;; (apply-procedure): evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
;; procval x list-valores-expresados
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env  (definir-mutabilidad ids args) args env)
                                )))))
(define definir-mutabilidad
  (lambda (ids args)
   (cond
     ((null? ids) empty)
     (else   (if (target? (car args))
                 (cases target (car args)
                              (indirect-target (ref) (cases reference ref (a-ref (pos vec mut)
                                                                                 (if (equal? mut 'M)
                                                                                     (cons (mutable (car ids)) (definir-mutabilidad (cdr ids) (cdr args)) )
                                                                                     (cons (inmutable (car ids)) (definir-mutabilidad (cdr ids) (cdr args))  )) )))
                              )
                  (cons (mutable (car ids)) (definir-mutabilidad (cdr ids) (cdr args)))
       )
     )
   )
  )
)

 
 
;; IMPLEMENTACION PROCEDURE
(define evaluacion-expresiones-procedure
  (lambda (ids body env)
    (closure ids body env)
  )
)
;; IMPLEMENTACION CALL-PROCEDURE
(define evaluacion-expresiones-call-procedure
  (lambda (expr args env)
    (let (
        (proc (eval-expression expr env))
          (argumentos  (evaluacion-expresiones-listas args env))
         )  
         (if (procval? proc)
                     (apply-procedure proc argumentos)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))
      )
  )
)



;; Enviroments:


;; PASO POR VALOR Y REFERENCIA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crea una referencia, pos es la posicion de la referencia en el vector
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (mutable symbol?)
        )
)


;;deref retorna el valor de la referencia en el vector
(define de-ref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vals mut) 
        (if (target? (vector-ref vals pos))
           (cases target (vector-ref vals pos)
              (indirect-target (refi) (primitive-deref refi))
           )
          (primitive-deref ref)
        )
      )
    )       
  )
)
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec mut)
             (vector-ref vec pos)
      )
    )
  )
)
;;setref! cambia el valor de la referencia por el valor dado
(define setref!
  (lambda (ref val)
    (if (target? ref)
        (cases target ref
          (indirect-target (refi) (primitive-setref! refi val))
        )
        (primitive-setref! ref val)
    )
  )
)
(define primitive-setref!
     (lambda (ref val)
       (cases reference ref
         (a-ref (pos vec mut) (vector-set! vec pos val))
       )
     )
    )

;; TARGET

(define-datatype target target?
  (indirect-target (ref  ref-to-direct-target? ))

)
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec mut)
                  ( if  (not (reference?  (vector-ref vec pos) ) )  #t #f ))))))
;; VARIABLES
(define-datatype variable variable?
  (mutable (id symbol?))
  (inmutable (id symbol?))
)
;; ENCONTRAR EL SIMBOLO EN UNA LISTA DE VARIABLES
(define encontrar-simbolo-en-vars
  (lambda (sym vars)
    (busqueda-symb-in-vars sym vars 0)
  )
)
;; AUX SEARCH SIMBOLO EN LISTA DE VARIABLES
(define busqueda-symb-in-vars
  (lambda (sym vars pos)
    (cond
      ( (null? vars) #f)
      ( (equal? sym (retornar-simbolo (car vars))) pos )
      ( else (busqueda-symb-in-vars sym (cdr vars) (+ pos 1)) )
    )
  )
)
;; ES-MUTABLE?    1. buscar en el ambiente la variable que tenga esa simbolo y retornarla
(define es-mutable?
  (lambda (sym env)
      (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-simbolo-en-vars sym vars)))
                             (if (number? pos)
                                 (cases variable (list-ref vars pos)
                                   (mutable (id) #t)
                                   (inmutable (id) #f)
                                 )
                                 (es-mutable? sym env)
                             )
        )
      )
  )
 )
)
;; RETORNA SI LA VARIABLE ES MUTABLE O INMUTABLE
(define encontrar-valor-mutable
 (lambda (sym vars)
   (busqueda-mut-in-vars sym vars 0)
 )
)

;; AUX SEARCH MUT EN LISTA DE VARIABLES
(define busqueda-mut-in-vars
  (lambda (sym vars pos)
    (cond
      ( (null? vars) #f)
      ( (equal? sym (retornar-simbolo (car vars))) (cases variable (car vars) (mutable (id) 'M) (inmutable (id) 'I) )  )
      ( else (busqueda-mut-in-vars sym (cdr vars) (+ pos 1)) )
    )
  )
)

;; RETORNAR EL SIMBOLO DE LA VARIABLE
(define retornar-simbolo
  (lambda (var)
    (cases variable var
      (mutable (id) id)
      (inmutable (id) id)
    )
  )
)

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (vars (list-of variable?))
                       (vec vector?)
                       (env environment?))
  )

(define scheme-value? (lambda (v) #t))

(define empty-env  
  (lambda ()
    (empty-env-record)))       


(define extend-env
  (lambda (vars vals env)
    (extended-env-record vars (list->vector vals) env)))


;función que busca un símbolo en un ambiente  
(define apply-env
  (lambda (env sym)
    (de-ref (apply-env-ref env sym))
  )
)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-simbolo-en-vars sym vars)) (mut (encontrar-valor-mutable sym vars)) )
                             (if (and (number? pos) (symbol? mut) )
                                 (a-ref pos vals mut)
                                 (apply-env-ref env sym)))))))


;; UTLIDADES OBJETOS
;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

;; UTILIDADES PARTES
;;;;;;;;;;;;;;;; selectors of all sorts ;;;;;;;;;;;;;;;;

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))
;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expression body
        (extend-env
          (map (lambda (id) (mutable id))(cons '%super (cons 'self ids)))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (map (lambda (id) (mutable id)) (part->field-ids (car parts)))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))
;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; find a method in a list of method-decls, else return #f

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))

;; DATATYPES OBJETOS
(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))


;; evaluar
(define aux
   (lambda (x)
     x))



(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))))))


      
;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of class-decls.

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

(interpreter)



