#lang racket
(require "funciones.rkt")
(require racket/include)
(provide (all-defined-out))

(define (goloso matrix symbols)
  (solution (objetivo (viable_candidates matrix symbols) matrix symbols) )
)

;Funcion de Viabilidad
;función que determina los indices de la matrix de los candidates a solución que son viables
; matrix: representa el gato
; symbols los symbols usados para marcar la posicion escogida por los usuarios ej. '(X 0)
(define (viable_candidates matrix symbols)
  (viable_candidates_aux matrix symbols '() 0 0)
)
; función auxiliar para determinar los candidates viables
; n fila
; m columna
(define (viable_candidates_aux matrix symbols positions m n)
  (cond((null? matrix) positions)
       ( (null? (car matrix))
         (viable_candidates_aux (cdr matrix) symbols positions 0 (+ n 1)))
       
       ( (and (different? (caar matrix) (car symbols)) (different? (caar matrix) (cadr symbols)))
         (viable_candidates_aux (append (list (cdar matrix)) (cdr matrix)) symbols (append positions (list (list n m))) (+ m 1) n))
       (else        
          (viable_candidates_aux (append (list (cdar matrix)) (cdr matrix)) symbols positions (+ m 1) n))

   )
)


(define (solution evaluated_candidates)
  (solution_aux (car evaluated_candidates) (cdr evaluated_candidates))
 )

(define (solution_aux best_candidate candidates)
  (cond ( (null? candidates)
          (car best_candidate)   )
        ((< (cadr best_candidate) (cadar candidates))
          (solution_aux (car candidates) (cdr candidates))   )
        (else
          (solution_aux best_candidate (cdr candidates))    )
   )
)


;Funcion Objetivo
; devuelve una lista donde cada par se evalua, cada elemento tiene la siguiente estructura ai= ((fila columna) nota)
(define (objetivo candidates matrix symbols)
  (objetivo_aux candidates '() matrix  symbols)
)

(define (objetivo_aux candidates rated_candidates matrix symbols)
  (cond ( (null? candidates)
          rated_candidates)
        (else
          (objetivo_aux (cdr candidates) (append rated_candidates (list (list (car candidates) (rate (car candidates) matrix symbols)))) matrix symbols)
          )
  )
)

;función que evalúa un candidato dentro de la matrix del juego
;@param coordinates : coordinates (i j) a rate
(define(rate coordinates matrix symbols)
  (cond ( (hypothetical_win? coordinates matrix (car symbols)) 1000);computer wins
        
        ( (hypothetical_win? coordinates matrix (cadr symbols)) 500);player wins

        ( (or (and (exists_in_col? coordinates matrix (cadr symbols)) (not (exists_in_col? coordinates matrix (car symbols))))
              (and (exists_in_row? coordinates matrix (cadr symbols)) (not (exists_in_row? coordinates matrix (car symbols))))
              (exists_in_diagonal? coordinates matrix (cadr symbols))
              ) 100)

        (else 50)
   )
  )

(define (hypothetical_win? coordinates matrix symbol)
 (win? symbol (put matrix symbol (car coordinates) (cadr coordinates)) )
  )

(define  (exists_in_col? coordinates matrix symbol)
  (exists_in_col_aux? (cadr coordinates) (car matrix) (cdr matrix) symbol)
  )

(define (exists_in_col_aux? j row matrix symbol)
  (cond ( (and (null? matrix) (null? row)) #f)          
        ( (equal? symbol (get-value-row row j)) #t)
        ( (null? matrix) (exists_in_col_aux? j '() '() symbol) )
        ( else (exists_in_col_aux? j (car matrix) (cdr matrix) symbol))
        )
  )
        

(define  (exists_in_row? coordinates matrix symbol)
  (exists_in_row_aux? matrix 0 (car coordinates) symbol)
  )

(define (exists_in_row_aux? matrix n i symbol)
  (cond ((= n i)
         (contains? symbol (car matrix))
         )
        (else
         (exists_in_row_aux? (cdr matrix) (+ n 1) i symbol))
     )
  )
(define (exists_in_row2? row symbol)
  (cond ((null? row) #f)
        ( (equal? (car row) symbol) #t)
        (else
         (exists_in_row2? (cdr row) symbol)
         )
        )
  )

(define  (exists_in_diagonal? coordinates matrix symbol)
  #f
  )
                                                                                                                                                                                                      

(define (win? symbol matrix)
  (cond ( (or (not (null? (check-rows matrix 0 symbol)))
              (not (null? (check-columns matrix 0 symbol)))
              (not (null? (check-diagonals matrix 0 symbol)))
              (not (null? (check-diagonals-inverse matrix (- (length matrix) 1) symbol)))
              ) #t)
        (else #f)
        )
  )


(define (different? X Y)
  (not (equal? X Y))
)

(define (contains? item list)
  (cond ((null? list) #f)
        ( (equal? item (car list)) #t)
        (else
         (contains? item (cdr list))
         )
        )
  )

          