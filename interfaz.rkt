#lang racket/gui
(require table-panel)
(require "goloso.rkt")
(require "funciones.rkt")
;frame principal
(define (TTT mT nT)
  (set! m mT)
  (set! n nT)
  (set! listaPrincipal (poblarMatriz m n listaPrincipal 0))
  (inicio m n)
  (send frame show #t))

(define intro (new frame%
                   [label "Gato"]
                   [width 300]
                   [height 100]))
;texto
(define msg (new message% [parent intro]
                          [label "Buenas por favor digite el tamano del gato:"]))
;texto
(define mText (new text-field%
                     [label "m:"]
                     [parent intro]))
;texto
(define nText (new text-field%
                     [label "n:"]
                     [parent intro]))

;variable global de la matri con la que se crean los botones
(define listaPrincipal '())
(define matrizPrincipal '())
(define m 0)
(define n 0)
(define buttons #[])
(define posicionUsuario 0)
(define posicionMaquina 0)
(define posicionButton '())


;poblador de matriz
(define (poblarMatriz m n matriz contador)
  (cond ((equal? (* m n) (length matriz))
         (set! matrizPrincipal matriz)
         matrizPrincipal)
        (else (poblarMatriz m n (append matriz (list contador)) (+ contador 1)))))


;frame principal
(define frame
  (instantiate frame%
    ("Gato")))



;boton para iniciar juego
(new button% [parent intro]
             [label "Inicio"]
             [callback (lambda (button event)
                         (send intro show #f)
                         (set! m (string->number (send mText get-value)))
                         (set! n (string->number (send nText get-value)))
                         (set! listaPrincipal (poblarMatriz m n listaPrincipal 0))
                         (inicio m n)
                         (send frame show #t))])


;panel principal del juego
(define panelPrincipal
  (instantiate table-panel%
    (frame)
    (alignment '(center center))
    (dimensions '(2 2))))


;creacion de botones y los eventos de cada uno
(define (inicio m n)
  (let ((panelGato (instantiate table-panel%
                 (panelPrincipal)
                 (style '(border))
                 (dimensions (list m n))
                 (column-stretchability (if (memq 1 '(0 1)) #t #f))
                 (row-stretchability (if (memq 1 '(0 2)) #t #f)))))
    ;---------------
    (let ((temp (for/vector ([j listaPrincipal])
                     (new button%
                          [parent panelGato]
                          [label (format "~a" j)]
                          [callback (lambda (button event)
                                             (send button enable #f)
                                             ;-----aqui se agrega a la interfaz en la parte del usuario
                                             ;aqui se cambia por un cero en la interfaz
                                             (send button set-label "O")
                                             ;aqui se agrega a la matriz
                                             (set! posicionUsuario (numToCoor j 0 n))
                                             (set! matrizPrincipal (put matrizPrincipal 'O (car posicionUsuario) (cadr posicionUsuario)))
                                             (cond ((win? 'O matrizPrincipal)
                                                    (send gano show #t)))
                                             ;aqui inicia el algoritmo goloso
                                             (cond ((null? (viable_candidates matrizPrincipal '(X O)))
                                                    (send empate show #t))
                                                   (else
                                                    (set! posicionMaquina (goloso matrizPrincipal '(X O)))
                                                    
                                                    (set! temp (append temp (list posicionMaquina)))
                                                    (set! matrizPrincipal (put matrizPrincipal 'X (car posicionMaquina) (cadr posicionMaquina)))
                                                    (set! posicionButton (vector-ref buttons (coorToNum posicionMaquina n)))
                                                    (send posicionButton set-label "X")
                                                    (send posicionButton enable #f)
                                                    (cond ((win? 'X matrizPrincipal)
                                                           (send perdio show #t)))
                                                    (cond ((null? (viable_candidates matrizPrincipal '(X O)))
                                                           (send empate show #t)))
                                                    ))
                                      )]))))
      (set! buttons (vector-append buttons temp))
      (set! matrizPrincipal (listaMatriz n m listaPrincipal '() '())))))

(define temp '())


;coordenada a numero
(define (numToCoor pos i columnas)
  (cond ((< pos columnas)
         (list i pos))
        (else
         (numToCoor (- pos columnas) (+ i 1) columnas))))

(define (coorToNum coordenada columnas)
  (+ (* (car coordenada) columnas) (cadr coordenada))) 
          
          

;funcion recursiva para pasar de una lista a una matriz
(define (listaMatriz mTemp nTemp lista temp resultado)
  (cond ((equal? nTemp 0)
         resultado)
        ((equal? mTemp 0)
         (listaMatriz n (- nTemp 1) lista '() (append resultado (list temp))))
        (else
         (listaMatriz (- mTemp 1) nTemp (cdr lista) (append temp (list (car lista))) resultado))))


;frame gano el juego
(define gano (new frame%
                   [label "Gato"]
                   [width 300]
                   [height 100]))
(define msgGano (new message% [parent gano]
                          [label "Ganaste el juego!"]))
;frame perdio el juego
(define perdio (new frame%
                   [label "Gato"]
                   [width 300]
                   [height 100]))
(define msgPerdio (new message% [parent perdio]
                          [label "Perdiste el juego!"]))
;frame nadie gano el juego
(define empate (new frame%
                   [label "Gato"]
                   [width 300]
                   [height 100]))
(define msgEmpate (new message% [parent empate]
                          [label "Se empato el juego!"]))

(send intro show #t)