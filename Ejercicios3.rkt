#lang racket

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