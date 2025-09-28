#| 
Taller 1
Juan David Olaya - 2410206 - 3743
Juan Esteban Ortiz - 2410227 - 3743
Jean Pierre Cardenas - 2510003 - 3743
|#


#lang eopl

;Ejercicios 2.2


#|
Exp
Proposito:
Define el tipo de dato para representar expresiones aritméticas en notación polaca como árbol de sintaxis abstracta.
<Exp> := (const-exp n)
      |  (add-exp Exp Exp)
      |  (diff-exp Exp Exp)
      |  (mult-exp Exp Exp)
      |  (div-exp Exp Exp)
<n> := número
|#
(define-datatype Exp Exp?
  (const-exp (n number?))
  (add-exp   (e1 Exp?) (e2 Exp?))
  (diff-exp  (e1 Exp?) (e2 Exp?))
  (mult-exp  (e1 Exp?) (e2 Exp?))
  (div-exp   (e1 Exp?) (e2 Exp?)))



#|
PrefixList
Proposito:
Define el tipo de dato para envolver la expresión raíz del árbol de sintaxis abstracta de una expresión polaca.
<PrefixList> := (prefix-list Exp)
|#
(define-datatype PrefixList PrefixList?
  (prefix-list (exp Exp?)))


;; ====== PARSE ======

#|
parse-binop
Proposito:
lst x make-exp -> (exp, lista): procedimiento auxiliar que parsea una operación binaria en notación polaca, construyendo el subárbol correspondiente y devolviendo el resto de la lista.
<lista> := '()
        := (<op> <exp> <exp> <lista>)
<exp>  := <const-exp> | <add-exp> | <diff-exp> | <mult-exp> | <div-exp>
|#
(define (parse-binop lst make-exp)
  (let* ([res1 (parse-exp (cdr lst))]
         [e1   (car res1)]
         [rest1 (cadr res1)]
         [res2 (parse-exp rest1)]
         [e2   (car res2)]
         [rest2 (cadr res2)])
    (list (make-exp e1 e2) rest2)))


#|
parse-exp
Proposito:
lst -> (exp, lista): procedimiento que parsea una lista en notación polaca y devuelve el subárbol correspondiente y el resto de la lista.
<lista> := '()
        := (<num> <lista>)
        := (<op> <exp> <exp> <lista>)
<op> := + | - | * | /
|#
(define (parse-exp lst)
  (cond
    [(null? lst) (eopl:error 'parse-exp "Lista vacía")]
    [(number? (car lst)) (list (const-exp (car lst)) (cdr lst))]
    [(eq? (car lst) '+) (parse-binop lst add-exp)]
    [(eq? (car lst) '-) (parse-binop lst diff-exp)]
    [(eq? (car lst) '*) (parse-binop lst mult-exp)]
    [(eq? (car lst) '/) (parse-binop lst div-exp)]
    [else (eopl:error 'parse-exp "Oparador incorrecto" (car lst))]))


#|
PARSEBNF
Proposito:
lst -> prefix-list: procedimiento que recibe una lista en notación polaca y retorna el árbol de sintaxis abstracta (AST) como un data-type prefix-list.
<lista> := '()
  := (<exp>)
|#
(define (PARSEBNF lst)
  (let* ([res (parse-exp lst)]
   [ast (car res)]
   [rest (cadr res)])
    (if (null? rest)
  (prefix-list ast)
  (eopl:error 'PARSEBNF "Error" rest))))

;Ejercicio 2.2
;; ====== UNPARSE ======

#|
unparse-exp
Proposito:
exp -> lista: procedimiento que recibe un árbol de sintaxis abstracta (Exp) y retorna la lista en notación polaca correspondiente.
<exp> := <const-exp> | <add-exp> | <diff-exp> | <mult-exp> | <div-exp>
<lista> := (<num>) | (<op> <lista> <lista>)
|#
(define (unparse-exp exp)
  (cases Exp exp
    (const-exp (n) (list n))
    (add-exp (e1 e2)  (cons '+ (append (unparse-exp e1) (unparse-exp e2))))
    (diff-exp (e1 e2) (cons '- (append (unparse-exp e1) (unparse-exp e2))))
    (mult-exp (e1 e2) (cons '* (append (unparse-exp e1) (unparse-exp e2))))
    (div-exp (e1 e2)  (cons '/ (append (unparse-exp e1) (unparse-exp e2))))))


#|
UNPARSEBNF
Proposito:
tree -> lista: procedimiento que recibe un árbol de tipo prefix-list y retorna la lista en notación polaca correspondiente.
<prefix-list> := (prefix-list <exp>)
<lista> := (<num>) | (<op> <lista> <lista>)
|#
(define (UNPARSEBNF tree)
  (cases PrefixList tree
    (prefix-list (exp) (unparse-exp exp))))

;; ====== EVALUATE-EXP ======


#|
evaluate-exp
Proposito:
entrada -> num: procedimiento que evalúa una expresión en notación polaca, ya sea como lista, AST o nodo, y retorna el resultado numérico.
<entrada> := lista | prefix-list | Exp
<num> := resultado de la evaluación
|#
(define (evaluate-exp entrada)
  (cond
    [(list? entrada) (let ([ast (PARSEBNF entrada)]) (evaluate-ast ast))]
    [(PrefixList? entrada) (evaluate-ast entrada)]
    [(Exp? entrada) (evaluate-exp-node entrada)]
    [else (eopl:error 'evaluate-exp "Tipo no reconocido" entrada)]))


#|
evaluate-ast
Proposito:
ast -> num: procedimiento que evalúa un árbol de tipo prefix-list y retorna el resultado numérico.
<prefix-list> := (prefix-list <exp>)
<num> := resultado de la evaluación
|#
(define (evaluate-ast ast)
  (cases PrefixList ast
    (prefix-list (exp) (evaluate-exp-node exp))))


#|
evaluate-exp-node
Proposito:
exp -> num: procedimiento que evalúa un nodo del árbol de sintaxis abstracta (Exp) y retorna el resultado numérico.
<exp> := <const-exp> | <add-exp> | <diff-exp> | <mult-exp> | <div-exp>
<num> := resultado de la evaluación
|#
(define (evaluate-exp-node exp)
  (cases Exp exp
    (const-exp (n) n)
    (add-exp (e1 e2) (+ (evaluate-exp-node e1) (evaluate-exp-node e2)))
    (diff-exp (e1 e2) (- (evaluate-exp-node e1) (evaluate-exp-node e2)))
    (mult-exp (e1 e2) (* (evaluate-exp-node e1) (evaluate-exp-node e2)))
    (div-exp (e1 e2) (/ (evaluate-exp-node e1) (evaluate-exp-node e2)))))
