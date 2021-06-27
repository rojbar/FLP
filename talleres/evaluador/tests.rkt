#lang racket
;; Importamos el evaluador
;; No cambiar a menos que el archivo no se encuentre o este en otra carpeta
(require "evaluador.rkt")


;; En sus definiciones deben añadir antes de las definiciones (provide (all-defined-out))
;; Importan sus definiciones
;; (require "<path>")

;; Guardamos nuestros tests
(define (test-cases)
    (begin
      ;;Escribir aqui los casos bases
      ;;Se pueden importar de los archivos de texto contenidos en la carpeta "casos-base"
      ;;quitar void una vez se han traido los casos base
      void
    )
)

;; Ejecutamos
(test-cases)