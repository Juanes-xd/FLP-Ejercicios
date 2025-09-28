#lang eopl

(define-datatype Exp Exp?
  (const-exp (n number?))
  (add-exp   (e1 Exp?) (e2 Exp?))
  (diff-exp  (e1 Exp?) (e2 Exp?))
  (mult-exp  (e1 Exp?) (e2 Exp?))
  (div-exp   (e1 Exp?) (e2 Exp?)))


(define-datatype PrefixList PrefixList?
  (prefix-list (exp Exp?)))

(define (parse-binop lst make-exp)
  (let* ([res1 (parse-exp (cdr lst))]
         [e1   (car res1)]
         [rest1 (cadr res1)]
         [res2 (parse-exp rest1)]
         [e2   (car res2)]
         [rest2 (cadr res2)])
    (list (make-exp e1 e2) rest2)))

(define (parse-exp lst)
  (cond
    [(null? lst) (eopl:error 'parse-exp "Lista vacía")]
    [(number? (car lst)) (list (const-exp (car lst)) (cdr lst))]
    [(eq? (car lst) '+) (parse-binop lst add-exp)]
    [(eq? (car lst) '-) (parse-binop lst diff-exp)]
    [(eq? (car lst) '*) (parse-binop lst mult-exp)]
    [(eq? (car lst) '/) (parse-binop lst div-exp)]
    [else (eopl:error 'parse-exp "Oparador incorrecto" (car lst))]))

(define (PARSEBNF lst)
  (let* ([res (parse-exp lst)]
         [ast (car res)]
         [rest (cadr res)])
    (if (null? rest)
        (prefix-list ast)
        (eopl:error 'PARSEBNF "Error" rest))))

;; ====== UNPARSE ======
(define (unparse-exp exp)
  (cases Exp exp
    (const-exp (n) (list n))
    (add-exp (e1 e2)  (cons '+ (append (unparse-exp e1) (unparse-exp e2))))
    (diff-exp (e1 e2) (cons '- (append (unparse-exp e1) (unparse-exp e2))))
    (mult-exp (e1 e2) (cons '* (append (unparse-exp e1) (unparse-exp e2))))
    (div-exp (e1 e2)  (cons '/ (append (unparse-exp e1) (unparse-exp e2))))))

(define (UNPARSEBNF tree)
  (cases PrefixList tree
    (prefix-list (exp) (unparse-exp exp))))

;; ====== EVALUATE-EXP ======

(define (evaluate-exp entrada)
  (cond
    [(list? entrada) (let ([ast (PARSEBNF entrada)]) (evaluate-ast ast))]
    [(PrefixList? entrada) (evaluate-ast entrada)]
    [(Exp? entrada) (evaluate-exp-node entrada)]
    [else (eopl:error 'evaluate-exp "Tipo no reconocido" entrada)]))

(define (evaluate-ast ast)
  (cases PrefixList ast
    (prefix-list (exp) (evaluate-exp-node exp))))

(define (evaluate-exp-node exp)
  (cases Exp exp
    (const-exp (n) n)
    (add-exp (e1 e2) (+ (evaluate-exp-node e1) (evaluate-exp-node e2)))
    (diff-exp (e1 e2) (- (evaluate-exp-node e1) (evaluate-exp-node e2)))
    (mult-exp (e1 e2) (* (evaluate-exp-node e1) (evaluate-exp-node e2)))
    (div-exp (e1 e2) (/ (evaluate-exp-node e1) (evaluate-exp-node e2)))))
