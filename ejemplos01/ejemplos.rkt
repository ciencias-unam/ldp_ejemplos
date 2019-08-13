#lang plai

(print-only-errors)

#| Ejemplos de la Nota de Laboratorio 1 |#

;; Función que calcula el promedio de tres números.
;; promedio-3: number number number → number
(define (promedio-3 x y z)
   (/ (+ x y z) 3))

(test (promedio-3 1 3 8) 4)
(test (promedio-3 -1 0 7) 2)
(test (promedio-3 -3 0 3) 0)

;; Función que calcula la suma de los pesos correspondiente a monedas
;; de 50 centavos, 1 peso, 2 pesos, 5 pesos y 10 pesos
;; respectivamente.
;; suma-monedas: number number number number number → number
(define (suma-monedas a b c d e)
   (+ (* a 0.5) (* b 1) (* c 2) (* d 5) (* e 10)))

(test (suma-monedas 0 0 0 0 1) 10)
(test (suma-monedas 0 0 8 0 3) 46)
(test (suma-monedas 1 1 1 1 1) 18.50)

;; Función que calcula el volumen de la esfera de radio r.
;; volumen-esfera: number → number
(define (volumen-esfera r)
   (* 4/3 pi (expt r 3)))

(test (volumen-esfera 10) 4188.7902)

;; Función que calcula el área de un círculo dado su diámetro.
;; area-circulo: number → number
(define (area-circulo d)
   (* pi (/ d 2) (/ d 2)))

(test (area-circulo 10) 78.53)
(test (area-circulo 4) 12.56)
(test (area-circulo 16) 201.06)

;; Función que calcula el área de un círculo dado su diámetro.
;; area-circulo2: number → number
(define (area-circulo2 d)
   (let ([r (/ d 2)])
      (* pi r r)))

(test (area-circulo2 10) 78.53)
(test (area-circulo2 4)  12.56)
(test (area-circulo2 16) 201.06)

;; Función que calcula el valor absoluto de un número entero.
;; valor-absoluto: number → number
(define (valor-absoluto x)
   (if (< x 0)
       (* x -1)
       x))

(test (valor-absoluto 1729) 1729)
(test (valor-absoluto -265) 265)

;; Función que obtiene el nombre del mes representado por el número 
;; recibido como parámetro.
;; nombre-mes: number → string
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

(test (nombre-mes 8)  "Agosto")
(test (nombre-mes 10) "Octubre")
(test (nombre-mes 11) "Noviembre")
(test/exn (nombre-mes 25) "Mes inválido.")
