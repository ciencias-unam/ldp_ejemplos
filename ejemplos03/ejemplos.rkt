#lang plai

#| Ejemplos de la Nota de Laboratorio 3 |#

;; Función que permite definir estructuras genéricas.
(define (any? a) #t)

(define-type Arbol
	[hoja (elem any?)]
	[nodo (elem any?) (izq Arbol?) (der Arbol?)])

;; Función que obtiene el número de hojas de un árbol binario.
;; numero-hojas: Arbol → number
(define (numero-hojas a)
	(if (hoja? a)
		 1
		 (+ 1 (numero-hojas (nodo-izq a)) (numero-hojas (nodo-der a)))))

;; Predicado que indica si un elemento está contenido en un árbol.
;; contiene?: a Arbol → boolean
(define (contiene? e a)
	(cond
		[(hoja? a) (equal? e (hoja-elem a))]
		[(nodo? a) 
			(or (equal? e (nodo-elem a)) 
             (contiene? e (nodo-izq a)) 
             (contiene? e (nodo-der a)))]))

;; Función que obtiene el número de hojas de un árbol binario.
;; numero-hojas1: Arbol → number
(define (numero-hojas1 a)
	(type-case Arbol a
		[hoja (x) 1]
		[nodo (x i d) (+ 1 (numero-hojas1 i) (numero-hojas1 d))]))

;; Función que obtiene el número de hojas de un árbol binario.
;; numero-hojas2: Arbol → number
(define (numero-hojas2 a)
	(match a
		[(hoja _) 1]
		[(nodo _ i d) (+ 1 (numero-hojas2 i) (numero-hojas2 d))]))

;; Predicado que indica si un elemento está contenido en un árbol.
;; contiene1?: a Arbol → boolean
(define (contiene1? e a)
	(type-case Arbol a
		[hoja (x) (equal? e x)]
		[nodo (x i d) 
			(or (equal? e x) (contiene1? e i) (contiene1? e d))]))

;; Predicado que indica si un elemento está contenido en un árbol.
;; contiene2?: a Arbol → boolean
(define (contiene2? e a)
	(match a
		[(hoja x) (equal? e x)]
		[(nodo x i d) 
			(or (equal? e x) (contiene1? e i) (contiene1? e d))]))

;; Función que aplana un árbol binario.
;; aplana1: Arbol → (listof any?)
(define (aplana1 a)
	(type-case Arbol a
		[hoja (x) (list x)]
		[nodo (x i d) (append (list x) (aplana1 i) (aplana1 d))]))

;; Función que aplana un árbol binario.
;; aplana2: Arbol → (listof any?)
(define (aplana2 a)
	(match a
		[(hoja x) (list x)]
		[(nodo x i d) (append (list x) (aplana2 i) (aplana2 d))]))

;; Aplica una función a cada elemento de un árbol.
;; map-arbol1: procedure Arbol → Arbol
(define (map-arbol1 f a)
	(type-case Arbol a
		[hoja (x) (hoja (f x))]
		[nodo (x i d) (nodo (f x) (map-arbol1 f i) (map-arbol1 f d))]))

;; Aplica una función a cada elemento de un árbol.
;; map-arbol2: procedure Arbol → Arbol
(define (map-arbol2 f a)
	(match a
		[(hoja x) (hoja (f x))]
		[(nodo x i d) (nodo (f x) (map-arbol1 f i) (map-arbol1 f d))]))
