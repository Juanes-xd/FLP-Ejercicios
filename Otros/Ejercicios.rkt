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



