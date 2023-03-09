
#lang racket
;;Columnas   1 2 3 4 5 6 7 8 9 
(define tablero '((1 0 0 0 0 0 0 0 1);;1
                  (0 1 0 0 0 0 0 1 0);;2
                  (0 0 1 0 0 0 1 0 0);;3
                  (0 0 0 0 0 0 0 0 0);;4
                  (0 0 0 0 0 0 0 0 0);;5
                  (0 0 0 0 0 0 0 0 0);;6
                  (0 0 0 0 0 0 0 0 0);;7
                  (0 0 0 0 0 0 0 0 0)));;8
                 ;; (0 0 0 0 0 0 0 0 0)));;9


;;Función Solución
(define (solucion lista player)
  (cond ((equal? #t (sol_horizontal? lista player))#t)
        ((equal? #t (sol_vertical? lista player))#t)
        ((equal? #t (sol_diagonal? lista player))#t)
        (else #f)))

;;Solución Horizontal
(define (sol_horizontal? lista player)
  (cond ((null? lista)#f)
        ((equal? #t (sol_horizontal_aux (car lista) 0 player))#t);;Winner winner chicken dinner
        (else (sol_horizontal? (cdr lista) player))))
        
(define (sol_horizontal_aux lista contador player)
  (cond ((and (null? lista)(not(= contador 4)))#f)
        ((= contador 4)#t)
        ((= (car lista) player) (sol_horizontal_aux (cdr lista) (+ 1 contador) player))
        (else(sol_horizontal_aux (cdr lista) 0 player))))



;;Solución Vertical
(define (sol_vertical? lista player)
  (sol_vertical_aux lista 0 player 1 1 (len lista) (len (car lista))))
                                       
(define (sol_vertical_aux lista contador player indice_fila indice_columna alto ancho) ;;Alto= cantidad de filas/ Ancho= cantidad de columnas
  (cond ((= contador 4)#t)
        ((> indice_fila alto)(sol_vertical_aux lista 0 player 1 (+ 1 indice_columna) alto ancho))
        ((> indice_columna ancho) #f)
        ((= player (indice lista indice_fila indice_columna))
         (sol_vertical_aux lista (+ 1 contador) player (+ 1 indice_fila) indice_columna alto ancho))
        (else (sol_vertical_aux lista contador player (+ 1 indice_fila) indice_columna alto ancho))))





;;Solución Diagonal
(define (sol_diagonal? lista player)
  (cond ((equal? #t (sol_diagonal_LR lista 0 player (len lista) 1 (len lista) (len (car lista)) (len lista) 1))#t)
        ((equal? #t (sol_diagonal_RL lista 0 player (len lista) (len(car lista)) (len lista) (len (car lista)) (len lista) (len (car lista)))) #t)
        (else #f)))
                     
(define (sol_diagonal_RL lista contador player indice_fila indice_columna alto ancho reset_fila reset_columna) ;;Alto= cantidad de filas/ Ancho= cantidad de columnas
  (cond ((= contador 4)#t)

        ((and(= indice_fila 1) (= indice_columna 1))#f);;No hay ganador
         
        ((and (> indice_fila alto)(< 1 reset_fila));;mitad inferior de la matriz
         (sol_diagonal_RL lista contador player (- reset_fila 1) reset_columna alto ancho (- reset_fila 1) reset_columna))
        
        ((and (< indice_columna 1)(> reset_columna 1));;mitad superior de la matriz
         (sol_diagonal_RL lista contador player reset_fila (- reset_columna 1) alto ancho reset_fila (- reset_columna 1)))
        
        ((= player (indice lista indice_fila indice_columna))
         (sol_diagonal_RL lista (+ 1 contador) player (+ indice_fila 1) (- indice_columna 1) alto ancho reset_fila reset_columna))

       
        
        (else (sol_diagonal_RL lista 0 player (+ indice_fila 1) (- indice_columna 1) alto ancho reset_fila reset_columna))))


(define (sol_diagonal_LR lista contador player indice_fila indice_columna alto ancho reset_fila reset_columna) ;;Alto= cantidad de filas/ Ancho= cantidad de columnas
  (cond ((= contador 4)#t)
        
        ((and(= indice_fila 1) (= indice_columna ancho))#f);;No hay ganador
        
        ((and (> indice_fila alto)(> reset_fila 1));;mitad inferior de la matriz
         (sol_diagonal_LR lista contador player (- reset_fila 1) reset_columna alto ancho (- reset_fila 1) reset_columna))
        
        ((and (> indice_columna ancho)(< reset_columna ancho));;mitad superior de la matriz
         (sol_diagonal_LR lista contador player reset_fila (+ reset_columna 1) alto ancho reset_fila (+ reset_columna 1)))
        
        ((= player (indice lista indice_fila indice_columna))
         (sol_diagonal_LR lista (+ 1 contador) player (+ 1 indice_fila) (+ 1 indice_columna) alto ancho reset_fila reset_columna))

       
        
        (else (sol_diagonal_LR lista 0 player (+ 1 indice_fila) (+ 1 indice_columna) alto ancho reset_fila reset_columna))))



;;extraido del git________________________________________________________________________________________________
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




