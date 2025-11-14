#lang eopl

;******************************************************************************************
; EJEMPLO: Programa con Formas Geométricas
; 
; Este programa demuestra la jerarquía de clases:
;     Forma (clase base con métodos perímetro y área)
;       ├─ Rectángulo (con campos lado1, lado2)
;       └─ Círculo (con campo radio)
;
;******************************************************************************************

;; Especificación Léxica
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;; Gramática
(define the-grammar
  '((program ((arbno class-decl) expression) a-program)

    ;; Expresiones
    (expression (number) lit-exp)
    (expression (identifier) var-exp)   
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") app-exp)

    ;; Primitivas
    (primitive ("+") add-prim)
    (primitive ("-") subtract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)

    ;; Clases
    (class-decl ("class" identifier "extends" identifier 
                  (arbno "field" identifier)
                  (arbno method-decl))
                a-class-decl)

    ;; Métodos
    (method-decl ("method" identifier "(" (separated-list identifier ",") ")" expression)
                 a-method-decl)

    ;; Expresiones OOP
    (expression ("new" identifier "(" (separated-list expression ",") ")") new-object-exp)
    (expression ("send" expression identifier "(" (separated-list expression ",") ")") method-app-exp)
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program pgm))
    (sllgen:make-stream-parser the-lexical-spec the-grammar)))

;******************************************************************************************
; EVALUADOR
;******************************************************************************************

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (class-decls body)
                 (let ((class-env (add-classes-to-env class-decls (empty-class-env))))
                   (eval-expression body (empty-env) class-env))))))

(define eval-expression
  (lambda (exp env class-env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env class-env)))
                     (apply-primitive prim args)))
      (if-exp (test true false)
              (if (true-value? (eval-expression test env class-env))
                  (eval-expression true env class-env)
                  (eval-expression false env class-env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env class-env)))
                 (eval-expression body (extend-env ids args env) class-env)))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env class-env))
                     (args (eval-rands rands env class-env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression "No es un procedimiento"))))
      (new-object-exp (class-name rands)
                      (let ((args (eval-rands rands env class-env)))
                        (let ((class (lookup-class class-name class-env)))
                          (new-instance class args class-name))))
      (method-app-exp (obj method-name rands)
                      (let ((args (eval-rands rands env class-env))
                            (obj-val (eval-expression obj env class-env)))
                        (apply-method obj-val method-name args class-env))))))

(define eval-rands
  (lambda (rands env class-env)
    (map (lambda (rand) (eval-expression rand env class-env)) rands)))

(define true-value? (lambda (x) (not (zero? x))))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args))))))

;******************************************************************************************
; PROCEDIMIENTOS
;******************************************************************************************

(define-datatype procval procval?
  (closure (ids (list-of symbol?)) (body expression?) (env environment?)))

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env) (empty-class-env))))))

;***************************** AMBIENTES *****************************

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?)) (vals (list-of scheme-value?)) (env environment?)))

(define scheme-value? (lambda (v) #t))

(define empty-env (lambda () (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "Variable no definida: ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym)))))))

;***************************** CLASES *****************************

(define-datatype class-environment class-environment?
  (empty-class-env-record)
  (extended-class-env-record (name symbol?) (class-decl class-decl?) (env class-environment?)))

(define empty-class-env (lambda () (empty-class-env-record)))

(define add-classes-to-env
  (lambda (class-decls env)
    (if (null? class-decls)
        env
        (add-classes-to-env (cdr class-decls)
                            (extended-class-env-record 
                              (a-class-decl->name (car class-decls))
                              (car class-decls)
                              env)))))

(define lookup-class
  (lambda (name env)
    (cases class-environment env
      (empty-class-env-record () (eopl:error 'lookup-class "Clase no existe: ~s" name))
      (extended-class-env-record (class-name class-decl parent-env)
                                 (if (eq? name class-name)
                                     class-decl
                                     (lookup-class name parent-env))))))

(define a-class-decl->name
  (lambda (class-decl)
    (cases class-decl class-decl
      (a-class-decl (name parent-name fields methods) name))))

;***************************** OBJETOS *****************************

(define-datatype object object?
  (an-object (class-name symbol?) (fields (list-of scheme-value?))))

(define new-instance
  (lambda (class-decl args class-name)
    (an-object class-name args)))

(define apply-method
  (lambda (obj method-name args class-env)
    (cases object obj
      (an-object (class-name field-values)
                 (let ((class-decl (lookup-class class-name class-env)))
                   (cases class-decl class-decl
                     (a-class-decl (name parent-name fields methods)
                                   (let ((method (find-method method-name methods)))
                                     (if method
                                         (cases method-decl method
                                           (a-method-decl (meth-name ids body)
                                                          (let ((env (extend-env fields field-values (empty-env))))
                                                            (eval-expression body 
                                                                           (extend-env ids args env) 
                                                                           class-env))))
                                         (eopl:error 'apply-method "Método no existe: ~s" method-name)))))))))

(define find-method
  (lambda (method-name methods)
    (cond
      ((null? methods) #f)
      ((eq? (a-method-decl->name (car methods)) method-name) (car methods))
      (else (find-method method-name (cdr methods))))))

(define a-method-decl->name
  (lambda (method-decl)
    (cases method-decl method-decl
      (a-method-decl (name ids body) name))))

;***************************** AUXILIARES *****************************

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

;***************************** PROGRAMA PRINCIPAL *****************************

; Define las clases y luego crea instancias

(define programa-formas
  (scan&parse "
    class Forma extends Object
      field ubicacion-x
      field ubicacion-y
      method get-x () ubicacion-x
      method get-y () ubicacion-y
      method set-x (nuevo-x) (set ubicacion-x = nuevo-x)
      method set-y (nuevo-y) (set ubicacion-y = nuevo-y)
    
    class Rectangulo extends Forma
      field lado1
      field lado2
      method get-lado1 () lado1
      method get-lado2 () lado2
      method area () *(lado1, lado2)
      method perimetro () *(+(lado1, lado2), 2)
    
    class Circulo extends Forma
      field radio
      method get-radio () radio
      method area () *(3.1416, *(radio, radio))
      method perimetro () *(*(2, 3.1416), radio)
    
    let resultado = new Rectangulo (3, 4, 20, 30) in
      resultado
  "))

(interpretador)
