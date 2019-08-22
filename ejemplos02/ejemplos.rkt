#lang plai

(print-only-errors)

#| Ejemplos de la Nota de Laboratorio 2 |#

;; Función que suma dos vectores representados por pares.
;; suma: (pairof real) (pairof real) -> (pairof real)
(define (suma u v)
   (cons (+ (car u) (car v)) (+ (cdr u) (cdr v))))

;; Función que realiza el producto punto de dos vectores representados 
;; por pares.
;; producto-punto: (pairof real) (pairofreal) -> real
(define (producto-punto u v)
   (+ (* (car u) (car v)) (* (cdr u) (cdr v))))

;; Función que realiza el producto punto de un vector representado por 
;; un par y un escalar.
;; producto-escalar: (pairof real) real -> (pairof real)
(define (productoEscalar u k) 
   (cons (* (car u) k) (* (cdr u) k)))

;; Función que obtiene la longitud de una lista.
;; longitud: (listof a) -> number
(define (longitud l) 
   (if (empty? l)
       0 
       (+ 1 (longitud (cdr l)))))

;; Función que quita los primeros n elementos de una lista.
;; quita: number (listof a) -> (listof a)
(define (quita n l)
   (if (zero? n) 
       l 
       (quita (sub1 n) (cdr l))))

;; Función que toma los primeros n elementos de una lista.
;; toma: number (listof a) -> (listof a)
(define (toma n l) 
   (if (zero? n) 
       '() 
       (cons (car l) (toma (sub1 n) (cdr l)))))

;; Función que indica si un elemento pertenece a una lista.
;; contiene: a (lisotf a) -> boolean
(define (contiene? e l) 
   (or (equal? (car l) e) (contiene? e (cdr l))))

;; Función que suma los dígitos de un número entero positivo.
;; suma-digitos: number -> number
(define (suma-digitos n)
   (match n
      [0 0]
      [n (+ (modulo n 10) (quotient n 10))]))

;; Función que suma los elementos de una lista.
;; suma-lista: (listof number) -> number
(define (suma-lista l)
   (match l
      ['() 0]
      [(cons x xs) (+ x (suma-lista xs))]))

;; Función que aplica una función a cada elemento de una lista.
;; mapeo: procedure (listof any) -> (listof any)
(define (mapeo f l)
   (match l
      ['() '()]
      [(cons x xs) (cons (f x) (mapeo f xs))]))

;; Función que obtiene el nombre del mes representado por el número 
;; recibido como parámetro.
;; nombre-mes: number -> string
(define (nombre-mes n)
   (cond
      [(equal? n 1)  "Enero"     ]
      [(equal? n 2)  "Febrero"   ]
      [(equal? n 3)  "Marzo"     ]
      [(equal? n 4)  "Abril"     ]
      [(equal? n 5)  "Mayo"      ]
      [(equal? n 6)  "Junio"     ]
      [(equal? n 7)  "Julio"     ]
      [(equal? n 8)  "Agosto"    ]
      [(equal? n 9)  "Septiembre"]
      [(equal? n 10) "Octubre"   ]
      [(equal? n 11) "Noviembre" ]
      [(equal? n 12) "Diciembre" ]
      [else (error 'nombre-mes "Mes inválido.")]))

;; Función que transforma una lista de números enteros en una lista 
;; de cadenas que representan el mes correspondiente.
;; meses: (listof number) -> (listof string)
(define (meses l)
   (map nombre-mes l))

;; Función que aplica filtra los elementos de una lista dada una 
;; condición.
;; filtra: (any -> boolean) (listof any) -> (listof any)
(define (filtra f l)
   (match l
      ['() '()]
      [(cons x xs)
         (cond
            [(f x) (cons x (filtra f xs))]
            [else (filtra f xs)])]))

;; Función que indica si una lista de tamaño 3 es una terna pitagórica.
;; terna-pitagorica?: list -> boolean
(define (terna-pitagorica? l)
   (match l
      ['() #f]
      [(list u v w) (equal? (+ (expt u 2) (expt v 2)) (expt w 2))]))

;; Función que filtra las tuplas de tamaño 3 que cumplen con la 
;; propiedad de ser una terna pitagórica.
;; ternasPitagoricas: (listof list) -> boolean
(define (ternas-pitagoricas xs)
   (filter terna-pitagorica? xs))

;; Función que aplica una función a los elementos de una lista, de 
;; forma encadenada a la derecha.
;; foldr: procedure any (listof any) -> any
(define (mi-foldr f v l)
   (match l
      ['() v]
      [(cons x xs) (f x (mi-foldr f v xs))]))

;; Función que aplica una función a los elementos de una lista, de 
;; forma encadenada a la izquierda.
;; foldl: procedure any (listof any) -> any
(define (mi-foldl f v l)
   (match l
      ['() v]
      [(cons x xs) (mi-foldl f (f x v) xs)]))

;; Función que suma los elementos de una lista.
;; suma-listar: (listof number) -> number
(define (suma-listar xs) 
   (foldr + 0 xs))

;; Función que suma los elementos de una lista.
;; suma-listal: (listof number) -> number
(define (suma-listal xs) 
   (foldl + 0 xs))

;; Función que obtiene el promedio de una lista de números.
;; promedio-lista: (listof number) -> number
(define (promedio-lista lst)
   (let ([suma (lambda (l) 
            (foldr + 0 l))]
         [longitud (lambda (l)
            (match l
               ['() 0]
               [(cons x xs) (+ 1 (longitud xs))]))])
      (/ (suma lst) (longitud lst))))
