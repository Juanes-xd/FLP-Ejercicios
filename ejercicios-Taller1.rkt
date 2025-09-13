#| 
Taller 1
Juan David Olaya - 2410206 - 3743
Juan Esteban Ortiz - 2410227 - 3743
Jean Pierre Cardenas - 2510003 - 3743
|#



#lang racket


;Ejercicio1

#| 
multiplo5?
Proposito: Retorna true si n es múltiplo de 5
n -> bool: procedimiento que determina si n es múltiplo de 5
<number> := 0
         := n - 5, si n >= 5
         := false, si n < 5
|#
(define (multiplo5? n)
  (if (zero? n)
      #t
      (if (>= (- n 5) 0)
          (multiplo5? (- n 5))
          #f)))

#|
invert
Proposito:
L x P -> L': procedimiento que invierte los pares (x,y) de la lista L si x e y cumplen el predicado P
<lista> := '()
        :=  ((<num> <num>) <lista>)
|#
(define invert (lambda (l p)
                 (if (null? l) '()
                     (let ((x (car (car l)))
                           (y (cdr (car l))))
                       (if (and (p x) (p (car y)))
                           (cons (list(car y) x) (invert (cdr l) p))
                           (invert (cdr l) p))))
                 ))
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((5 9) (10 90) (82 7) ) multiplo5? )
(invert '((6 9) (10 90) (82 7) ) odd? )

;Ejercicio2
#| 
down
Proposito: 
L -> L' : procedimiento que toma una lista L y retorna una lista L' donde cada elemento de L está envuelto en un par adicional
<lista> := '()
        :=  ((<string>) <lista>)
|#
(define down (lambda (l)
               (if (null? l) l
                   (cons (list (car l)) (down (cdr l))))))

(down '(1 2 3)) 
(down '((una) (buena) (idea))) 
(down '(un (objeto (mas)) complicado)) 

;Ejercicio3
#|
mayor5?
Proposito: Retorna true si n es mayor que 5
n -> bool: procedimiento que determina si n es mayor que 5
<number> := true, si n > 5
         := false, si n <= 5
|#
(define mayor5? (lambda (n) (> n 5)))

#| 
list-set
Proposito:
L n x P -> L': procedimiento que reemplaza el elemento en la posición n de L por x si cumple el predicado P
<lista> := '()
        :=  (<num> <lista>)
|#
(define list-set (lambda (L n x P)
                   (if (null? L) '()
                       (if (= n 0)
                           (if (P (car L))
                               (cons x (cdr L))
                               L)
                           (cons (car L) (list-set (cdr L) (- n 1) x P))))))

(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)
(list-set '(5 8 7 6) 3 '(1 5 10) mayor5? )
(list-set '(5 8 7 6) 0 '(1 5 10) mayor5? )

;Ejercicio4
#| 
filter-in
Proposito: 
L P -> L': procedimiento que filtra los elementos de L que cumplen el predicado P
<lista> := '()
        :=  (<char> <lista>)
        := (cons(<lista>) <lista>)
|#
(define filter-in(lambda (P L)
                   (if (null? L) '()
                       (if (P (car L)) (cons (car L) (filter-in P (cdr L)))
                           (filter-in P (cdr L))
                           )
                       )
                   ))

(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))


;Ejercicio5
#| 
list-index
Proposito: 
L P -> n: procedimiento que retorna la posición del primer elemento de L que cumple el predicado P
<lista> := '()
        :=  (<char> <lista>)
        := (cons(<lista>) <lista>)
|#
(define list-index (lambda (P L)
                     (if (null? L ) #f
                         (let ((x 0))
                           (if (P (car L)) x
                               (and (list-index P (cdr L))(+ x 1))
                               )))))

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))



;Ejercicio6
#| 
swapper
Proposito:
E1 E2 L -> L': procedimiento que intercambia todas las apariciones de E1 por E2 y viceversa en la lista L
<lista> := '()
        :=  (<char> <lista>)
        := (cons(<lista>) <lista>)
|#
(define swapper (lambda (E1 E2 L)
                  (if (null? L) '()
                      (if (equal? E1 (car L))
                          (cons E2 (swapper E1 E2 (cdr L)))
                          (if (equal? E2 (car L))
                              (cons E1 (swapper E1 E2 (cdr L)))
                              (cons (car L) (swapper E1 E2 (cdr L))))))))

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))


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
    (if (null? L1) '()
        (cons (cons (car L1) (car L2))
              (cartesian-product (cdr L1) L2))))
  )



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
           
(inversions-helper 3 '(2 1 4))
;2
(inversions-helper 2 '(3 1 4))
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



;Ejercicio13

#| 
operate
Proposito: Aplica sucesivamente operaciones binarias a una lista de operandos
lrators lrands -> result: procedimiento que toma una lista de funciones binarias y una lista de números y retorna el resultado de aplicar las operaciones sucesivamente
<operadores> := '()
             := (op <operadores>)
<operandos> := (<num>)
           := (<num> <operandos>)
|#
(define (operate lrators lrands)
  (cond
    [(null? lrators) (first lrands)]
    [else
     (let ([first-op (first lrators)]
           [firstOperand (first lrands)]
           [secondOperand (second lrands)]
           [restOperators (rest lrators)]
           [restOperands (rest (rest lrands))])
       (operate restOperators 
                (cons (first-op firstOperand secondOperand) restOperands)))]))

;Ejemplos del enunciado
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))




;Ejercicio14

#| 
path
Proposito: Encuentra la ruta desde la raíz hasta un número n en un BST
n bst -> ruta: procedimiento que retorna una lista con la ruta ('left' o 'right') desde la raíz hasta n
<arbol> := '()
        := (<num> <arbol> <arbol>)
<ruta> := '()
       := ("left" <ruta>)
       := ("right" <ruta>)
|#
(define (path n bst)
  (cond
    [(null? bst) '()]
    [(= n (first bst)) '()]
    [(< n (first bst))
     (let ([leftSubtree (second bst)])
       (cond
         [(null? leftSubtree) '()]
         [(member n (flatten leftSubtree))
          (cons "left" (path n leftSubtree))]
         [else '()]))]
    [(> n (first bst))
     (let ([rightSubtree (third bst)])
       (cond
         [(null? rightSubtree) '()]
         [(member n (flatten rightSubtree))
          (cons "right" (path n rightSubtree))]
         [else '()]))]))

;Ejemplos del enunciado
(define tree1 '(14 (7 () (12 () ()))
                   (26 (20 (17 () ()) ())
                       (31 () ()))))

(path 17 tree1)




;Ejercicio15

#| 
count-odd-and-even
Proposito: Cuenta números pares e impares en un árbol binario
tree -> conteo: procedimiento que retorna una lista con dos elementos (cantidad-pares cantidad-impares)
<arbol> := '()
        := (<num> <arbol> <arbol>)
<conteo> := (<num> <num>)
|#
(define (count-odd-and-even tree)
  (cond
    [(null? tree) '(0 0)]
    [else
     (let ([currentValue (first tree)]
           [subTreeLeft (second tree)]
           [subTreeRight (third tree)])
       (let ([countLeft (count-odd-and-even subTreeLeft)]
             [countRight (count-odd-and-even subTreeRight)])
         (if (even? currentValue)
             (list (+ 1 (first countLeft) (first countRight))
                   (+ (second countLeft) (second countRight)))
             (list (+ (first countLeft) (first countRight))
                   (+ 1 (second countLeft) (second countRight))))))]))

;Ejemplos del enunciado
(count-odd-and-even '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))




;Ejercicio16

#| 
simpson-rule
Proposito: Calcula la integral de una función f entre a y b usando la regla de Simpson
f a b n -> aproximacion: procedimiento que aproxima la integral mediante la regla de Simpson
<funcion> := (lambda (x) <expresion>)
<numero> := <real>
|#
(define (simpson-rule f a b n)
  (let ([h (/ (- b a) n)])
    (define (calcSum k sum)
      (cond
        [(> k n) sum]
        [(= k 0)
         (calcSum (+ k 1) (+ sum (f (+ a (* k h)))))]
        [(= k n)
         (calcSum (+ k 1) (+ sum (f (+ a (* k h)))))]
        [(odd? k)
         (calcSum (+ k 1) (+ sum (* 4 (f (+ a (* k h))))))]
        [else
         (calcSum (+ k 1) (+ sum (* 2 (f (+ a (* k h))))))]))
    (* (/ h 3) (calcSum 0 0))))

;Ejemplos del enunciado
(simpson-rule (lambda (x) (* x (* x x))) 1 5 8)
(simpson-rule (lambda (x) x) 1 5 12)




;Ejercicio17

#| 
prod-scalar-matriz
Proposito: Multiplica una matriz por un vector elemento a elemento
mat vec -> matriz-resultado: procedimiento que multiplica cada fila de la matriz por el vector elemento a elemento
<matriz> := '()
         := (<fila> <matriz>)
<fila> := (<num> <fila>)
       := '()
<vector> := (<num> <vector>)
         := '()
|#
(define (prod-scalar-matriz mat vec)
  (cond
    [(null? mat) '()]
    [else
     (cons 
      (cond
        [(or (null? (first mat)) (null? vec)) '()]
        [else
         (define (multiplyRow row v)
           (cond
             [(or (null? row) (null? v)) '()]
             [else (cons (* (first row) (first v))
                         (multiplyRow (rest row) (rest v)))]))
         (multiplyRow (first mat) vec)])
      (prod-scalar-matriz (rest mat) vec))]))

;Ejemplos del enunciado
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))




;Ejercicio18

#| 
pascal
Proposito: Retorna la fila N del triángulo de Pascal
N -> fila: procedimiento que genera la fila N del triángulo de Pascal
<numero> := 1 | 2 | 3 | ...
<fila> := (<num> <fila>)
       := '()
|#
(define (pascal N)
  (cond
    [(= N 1) '(1)]
    [else
     (let ([previousRow (pascal (- N 1))])
       (let ([rowZeroLeft (cons 0 previousRow)]
             [rowZeroRight (append previousRow '(0))])
         (define (sumLists list1 list2)
           (cond
             [(or (null? list1) (null? list2)) '()]
             [else (cons (+ (first list1) (first list2))
                         (sumLists (rest list1) (rest list2)))]))
         (sumLists rowZeroLeft rowZeroRight)))]))

;Ejemplos del enunciado
(pascal 5)
(pascal 1)