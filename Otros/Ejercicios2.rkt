#lang racket



;Ejercicio 7
;; cartesian-product : L1 x L2 -> L
;; Proposito:
;; L1 x L2 -> L : Procedimiento que recibe dos listas de símbolos sin repeticiones L1 y L2
;; y retorna una lista de pares que representan el producto cartesiano entre L1 y L2.
;; <lista> := ()
;;         | (<valor> <lista>)
;; <par>   := (<valor1> <valor2>)
;; <producto-cartesiano> := (<par> <producto-cartesiano>) | ()


(define cartesian-product
  (lambda (L1 L2)
    (define (pairs x ys)
      (if (null? ys)
          '()
          (cons (list x (car ys)) (pairs x (cdr ys)))))
    (if (null? L1)
        '()
        (append-pairs (pairs (car L1) L2)
                     (cartesian-product (cdr L1) L2)))))

;; append-pairs: concatena dos listas sin usar append
(define (append-pairs l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append-pairs (cdr l1) l2))))



(cartesian-product '(a b c) '(x y))
;((a x) (a y) (b x) (b y) (c x) (c y))
(cartesian-product '(p q r) '(5 6 7))
;((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))


;Ejercicio 8
;; mapping : (Num -> Num) x L1 x L2 -> L
;; Proposito:
;; F x L1 x L2 -> L : Procedimiento que recibe una función unaria F y dos listas de números L1 y L2 de igual tamaño.
;; Retorna una lista de pares (a b) tal que a es de L1, b es de L2 y F(a) = b.
;; <lista> := ()
;;         | (<valor> <lista>)
;; <par>   := (<valor1> <valor2>)
;; <mapping> := (<par> <mapping>) | ()


(define mapping
    (lambda (F L1 L2)
      (if (or (null? L1) (null? L2)) '()
            (if (equal? (F (car L1)) (car L2))
                (cons (list (car L1) (car L2))
                      (mapping F (cdr L1) (cdr L2)))
                (mapping F (cdr L1) (cdr L2)))
    )))


 (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
 ;((1 2) (2 4) (3 6))
 (mapping (lambda (d) (* d 3)) '(1 2 2) '(2 4 6))
 ;((2 6))
 (mapping (lambda (d) (* d 2)) '(1 2 3) '(3 9 12))
 ;()


;Ejercicio 9

;; inversions-helper : Num x L -> N
;; Proposito:
;; x x L2 -> N : Función auxiliar que cuenta cuántos elementos en L2 son menores que x.
;; <lista> := ()
;;         | (<valor> <lista>)
;; <numero> := 0 | 1 | 2 | ...
(define inversions-helper
  (lambda (x L2)
    (if (null? L2)
        0
        (+ (if (> x (car L2)) 1 0)
           (inversions-helper x (cdr L2))))))
           
;(inversions-helper 3 '(2 1 4))
;2
;(inversions-helper 2 '(3 1 4))
;1


;; inversions : L -> N
;; Proposito:
;; L -> N : Procedimiento que recibe una lista L y determina el número de inversiones de la lista L.
;; Una inversión es un par de posiciones (i, j) tal que i < j y L[i] > L[j].
;; <lista> := ()
;;         | (<valor> <lista>)
;;         := <numero>

(define inversions
  (lambda (L)
    (if (or (null? L) (null? (cdr L)))
        0
        (+ (inversions-helper (car L) (cdr L))
           (inversions (cdr L))))
           ))


(inversions '(2 3 8 6 1))
;5
(inversions '(1 2 3 4))
;0
(inversions '(3 2 1))
;3


;Ejercicio 10
;; up : L -> L
;; Proposito:
;; L -> L : Procedimiento que recibe una lista L y remueve un par de paréntesis (un nivel de lista)
;; a cada elemento del nivel más alto de la lista. Si un elemento no es una lista, se incluye sin modificación.
;; <lista> := ()
;;         | (<valor> <lista>)


(define up
  (lambda (L)
    (define (flatten-first lst rest)
      (if (null? lst)
          rest
          (cons (car lst) (flatten-first (cdr lst) rest))))
    (if (null? L) '()
        (if (list? (car L))
            (flatten-first (car L) (up (cdr L)))
            (cons (car L) (up (cdr L))))
            )
            ))


(up '((1 2) (3 4)))
;(1 2 3 4)
(up '((x (y)) z))
;(x (y) z)


;Ejercicio 11
;; zip : (A x B -> C) x L1 x L2 -> L
;; Proposito:
;; F x L1 x L2 -> L : Procedimiento que recibe una función binaria F y dos listas L1 y L2 de igual tamaño.
;; Retorna una lista donde cada elemento es el resultado de aplicar F a los elementos correspondientes de L1 y L2.
;; <lista> := ()
;;         | (<valor> <lista>)

(define zip
  (lambda (F L1 L2)
    (if (or (null? L1) (null? L2)) '()
        (cons (F (car L1) (car L2))
              (zip F (cdr L1) (cdr L2))))))


(zip + '(1 4) '(6 2))
;(7 6)
(zip * '(11 5 6) '(10 9 8))
;(110 45 48)




;Ejercicio 12
;; filter-acum : Num x Num x (A x Num -> A) x A x (Num -> Bool) -> A
;; Proposito:
;; a x b x F x acum x lter -> A : Procedimiento que aplica la función binaria F acumulando el resultado en acum
;; para todos los elementos en el intervalo [a, b] que cumplen el predicado lter. Retorna el valor final de acum.
;; <intervalo> := [<numero> <numero>]
;; <acum> := <valor>
;; <filtro> := (Num -> Bool)

(define filter-acum
  (lambda (a b F acum lter)
    (if (> a b) acum
        (if (lter a)
            (filter-acum (+ a 1) b F (F acum a) lter)
            (filter-acum (+ a 1) b F acum lter)))
))



(filter-acum 1 10 + 0 odd?)
;25
(filter-acum 1 10 + 0 even?)
;30