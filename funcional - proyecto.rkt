#lang racket

(define tablero '((0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)
                  (0 0 0 0 0 0 0 0)))

(define (indice lista fila columna)
  (cond ((or (null? lista) (< fila 1) (< columna 1) (> fila (len lista)) (> columna (len (car lista))))
               999)
              (else
               (indice_fila lista fila columna))
              )
        )

(define (indice_fila lista fila columna)
  (cond ((equal? fila 1)
         (indice_columna (car lista) columna))
        (else
         (indice_fila (cdr lista) (- fila 1) columna))
        )
  )

(define (indice_columna lista columna)
  (cond ((equal? columna 1)
         (car lista))
        (else
         (indice_columna (cdr lista) (- columna 1)))
        )
  )

(define (len lista)
  (cond ((null? lista)
         0)
        (else
         (+ 1 (len (cdr lista))))
        )
  )

(define (candidatos lista)
  (cond ((null? lista)
         lista)
        (else
         (candidatos_aux lista '() 1 1))
        )
  )

(define (candidatos_aux matriz candidatos fila columna)
  (cond ((> columna (len (car matriz)))
         candidatos)
        ((> fila (len matriz))
         (candidatos_aux matriz candidatos 1 (+ columna 1)))
        ((equal? (indice matriz fila columna) 0)
         (candidatos_aux matriz (append candidatos (list (list fila columna))) 1 (+ columna 1)))
        (else
         (candidatos_aux matriz candidatos (+ fila 1) columna))
        )
  )

(define (agregar lista num fila columna)
   (cond ((or (null? lista) (< fila 1) (< columna 1) (> fila (len lista)) (> columna (len (car lista))))
               '(999))
              (else
               (agregar_aux lista num fila columna '() 1))
              )
        )

(define (agregar_aux lista num fila columna new_lista fila_cont)
  (cond ((null? lista)
         new_lista)
        ((equal? fila fila_cont)
         (agregar_aux (cdr lista) num fila columna (append new_lista (list (agregar_exacto (car lista) num columna '() 1))) (+ fila_cont 1)))
        (else
         (agregar_aux (cdr lista) num fila columna (append new_lista (list (car lista))) (+ fila_cont 1)))
        )
  )

(define (agregar_exacto lista num columna new_lista columna_cont)
  (cond ((null? lista)
         new_lista)
        ((equal? columna columna_cont)
         (agregar_exacto (cdr lista) num columna (append new_lista (list num)) (+ columna_cont 1)))
        (else
         (agregar_exacto (cdr lista) num columna (append new_lista (list (car lista))) (+ columna_cont 1)))
        )
  )