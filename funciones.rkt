#lang racket
(require racket/include)
(provide (all-defined-out))
#|------------------------------------------------------------------------------------------------To create matrix------------------------------------------------------------------------------------------------|#

#|
Create row
@param columns - row size
@return columns - size row
|#
(define (create-row columns)
  (cond
    ((zero? columns) '() )
    (else (append '(0) (create-row (- columns 1))))
  )
)

#|
Create matrix
@param rows - rows number
@param columns - columns number
@return matrix of rows x columns
|#
(define (create-matrix rows columns)
  (cond
    ((zero? rows) '() )
    (else (append (list (create-row columns)) (create-matrix (- rows 1) columns)))
  )
)

#|-----------------------------------------------------------------------------------------------To change matrix-----------------------------------------------------------------------------------------------|#

#|
Change row
@param value - value to change
@param row - row to change
@param j - position in the row
@return row with the value changed
|#
(define (change-row value row j)
  (cond
    ((null? row) '())
    ((zero? j) (append (list value) (change-row value (cdr row) (- j 1))))
    (else  (append (list (car row)) (change-row value (cdr row) (- j 1))))
  )
)

#|
Put value in (i, j) positon in matrix
@param matrix - matrix to change
@param value - value to change
@param i - position in rows
@param j - position in columns
@return matrix with value changed
|#
(define (put matrix value i j)
  (cond
    ((null? matrix) '())
    ((zero? i) (append (list (change-row value (car matrix) j)) (put (cdr matrix) value (- i 1) j)))
    (else (append (list (car  matrix)) (put (cdr matrix) value (- i 1) j)))
  )
)

#|----------------------------------------------------------------------------------------------To get matrix value-----------------------------------------------------------------------------------------------|#

#|
Get value of the row
@param value - value to get
@param row - row to get
@param j - position in row
@return value
|#
(define (get-value-row row j)
  (cond
    ((zero? j) (car row))
    (else (get-value-row (cdr row) (- j 1)))
  )
)

#|
Get value of the (i, j) position in matrix
@param matrix - matrix to get value
@param i - position in rows
@param j - position in columns
@return value
|#
(define (get-value matrix i j)
  (cond
    ((zero? i) (get-value-row (car matrix) j))
    (else (get-value (cdr matrix) (- i 1) j))
  )
)

#|------------------------------------------------------------------------------------------------To check winner------------------------------------------------------------------------------------------------|#

#|
Check if there is a winner row
@param row - row to check
@param symbol - symbol to check in all positions
@return boolean
|#
(define (winner-row? row symbol)
  (cond
    ((null? row) #t)
    ((equal? symbol (car row)) (winner-row? (cdr row) symbol))
    (else #f)
  )
)

#|
Check if there is a winner row
@param matrix - matrix to check
@param pos - actual position
@param symbol - symbol to check in all positions
@return winner row position or () if it doesn't exist
|#
(define (check-rows matrix pos symbol)
  (cond
    ((null? matrix) '())
    ((winner-row? (car matrix) symbol) pos)
    (else (check-rows (cdr matrix) (+ pos 1) symbol))
  )
)

#|
Check if there is a winner column
@param column - column to check
@param symbol - symbol to check in all positions
@param row - position in row
@param column - actual position in column
@return boolean
|#
(define (winner-column? matrix symbol row column)
  (cond
    ((equal? row (length matrix)) #t)
    ((equal? symbol (get-value matrix row column)) (winner-column? matrix symbol (+ row 1) column))
    (else #f)
  )
)

#|
Check if there is a winner column
@param matrix - matrix to check
@param pos - actual position
@param symbol - symbol to check in all positions
@return winner column position or () if it doesn't exist
|#
(define (check-columns matrix pos symbol)
  (cond
    ((equal? pos (length (car matrix))) '())
    ((winner-column? matrix symbol 0 pos) pos)
    (else (check-columns matrix (+ pos 1) symbol))
  )
)

#|
Winner diagonal
|#
(define (winner-diagonal matrix row column symbol)
  (cond
    ((or (equal? (length matrix) row) (equal? (length (car matrix)) column)) #t)
    ((equal? (get-value matrix row column) symbol) (winner-diagonal matrix (+ row 1) (+ column 1) symbol))
    (else #f)
  )
)

#|
Check if there is a winner diagonal
@param matrix - matrix to check
@param diagonal - actual position
@param symbol - symbol to check in all positions
@return winner diagonal position or () if it doesn't exist
|#
(define (check-diagonals matrix diagonal symbol)
  (cond
    ((zero? (+ diagonal 1)) '())
    ((winner-diagonal matrix diagonal 0 symbol) diagonal)
    (else (check-diagonals matrix (- diagonal 1) symbol))
  )
)

#|
Winner diagonal inverse
|#
(define (winner-diagonal-inverse matrix row column symbol)
  (cond
    ((or (equal? row -1) (equal? (length (car matrix)) column)) #t)
    ((equal? (get-value matrix row column) symbol) (winner-diagonal-inverse matrix (- row 1) (+ column 1) symbol))
    (else #f)
  )
)

#|
Check if there is a winner diagonal inverse
@param matrix - matrix to check
@param diagonal - actual position
@param symbol - symbol to check in all positions
@return winner diagonal position or () if it doesn't exist
|#
(define (check-diagonals-inverse matrix diagonal symbol)
  (cond
    ((equal? diagonal (- (length (car matrix)) 2)) '())
    ((winner-diagonal-inverse matrix diagonal 0 symbol) diagonal)
    (else (check-diagonals-inverse matrix (- diagonal 1) symbol))
  )
)