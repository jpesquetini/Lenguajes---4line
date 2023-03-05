#lang racket

(define tablero '((1 1 2 0 0 2 1 0)
                  (0 2 0 0 0 0 2 0)
                  (0 1 0 0 0 0 0 0)
                  (0 2 0 0 0 0 0 0)
                  (0 1 0 0 0 0 0 0)
                  (0 2 0 0 0 0 0 0)
                  (0 1 0 0 0 0 0 0)
                  (0 1 0 0 0 0 0 0)))

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
  
  
  