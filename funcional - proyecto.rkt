#lang racket

(define tablero '((11 12 13 14 15 16 17 18)
                  (21 22 23 24 25 26 27 28)
                  (31 32 33 34 35 36 37 38)
                  (41 42 43 44 45 46 47 48)
                  (51 52 53 54 55 56 57 58)
                  (61 62 63 64 65 66 67 68)
                  (71 72 73 74 75 76 77 78)
                  (81 82 83 84 85 86 87 88)))

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
  