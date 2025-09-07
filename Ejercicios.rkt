#lang racket





;ejer1
; Definimos una función que retorna true si n es múltiplo de 5
; para ello tenemos el caso base de que si n es 0, retornamos true
; pero un numero restado 5 que de cero significa que es múltiplo de 5
; entonces hacemos una llamado recursivo para numeros mas grandes que 5
(define (multiplo5? n)
  (if (zero? n)
      #t
      (if (>= (- n 5) 0)
          (multiplo5? (- n 5))
          #f)))

; definimos la funcion invert que
(define invert (lambda (l p)
                 (if (null? l) '()
                     (let ((x (car (car l)))
                           (y (cdr (car l))))
                       (if (and (p x) (p (car y)))
                           (list (list(car y) x) (invert (cdr l) p))
                           (invert (cdr l) p))))
                 ))
;(invert '((3 2) (4 2) (1 5) (2 8)) even?)
;(invert '((5 9) (10 90) (82 7) ) multiplo5? )
;(invert '((6 9) (10 90) (82 7) ) odd? )

;ejer2
;; La función down toma una lista l y retorna una lista donde cada elemento está envuelto en un nivel extra de paréntesis

(define down (lambda (l)
               (if (null? l) l
                   (list (list (car l)) (down (cdr l))))))

;(down '(1 2 3)) ; => '((1) (2) (3))
;(down '((una) (buena) (idea))) ; => '(((una)) ((buena)) ((idea)))
;(down '(un (objeto (mas)) complicado)) ; => '((un) ((objeto (mas))) (complicado))

;ejer3

(define mayor5? (lambda (n) (> n 5)))

(define list-set (lambda (L n x P)
                   (if (null? L) '()
                       (if (= n 0)
                           (if (P (car L))
                               (cons x (cdr L))
                               L)
                           (cons (car L) (list-set (cdr L) (- n 1) x P))))))

;(list-set '(5 8 7 6) 2 '(1 2) odd?)
;(list-set '(5 8 7 6) 2 '(1 2) even?)
;(list-set '(5 8 7 6) 3 '(1 5 10) mayor5? )
;(list-set '(5 8 7 6) 0 '(1 5 10) mayor5? )

; buscar la posicion n y guardar el valor de ese numero en la lista
; en una variable a la cual se le pregunta el predicado
; si el predicado se cumple, hacer una funcion que meta ese elemento x
; en la lista en la posicion n

;ejer4

(define filter-in(lambda (P L)
                   (if (null? L) '()
                       (if (P (car L)) (cons (car L) (filter-in P (cdr L)))
                           (filter-in P (cdr L))
                           )
                       )
                   ))

#| (filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
 |#

;ejer5

(define list-index (lambda (P L)
                     (if (null? L ) #f
                         (let ((x 0))
                           (if (P (car L)) x
                               (and (list-index P (cdr L))(+ x 1))
                               )))))

#| (list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))
 |#


;Ejer6
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



