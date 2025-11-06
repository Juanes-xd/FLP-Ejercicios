#lang eopl
;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos y 
;;;;; procedimientos recursivos

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>   
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {identifier = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp proc-names idss bodies bodyletrec>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)  
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (imaginary-number
   (digit (arbno digit) "i") symbol)
  (imaginary-number
   ("-" digit (arbno digit) "i") symbol)
  (string
   ("\"" (arbno (not #\")) "\"") string)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (imaginary-number) imag-exp)
    (expression (string) string-exp)
    (expression (identifier) var-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    
    ; características adicionales
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
                letrec-exp)
    (expression ("switch" "(" expression ")" "{" 
                (arbno "case" expression ":" "{" expression "}")
                "default" ":" "{" expression "}"
                "}") switch-exp)
    ;;;;;;

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("complex") complex-prim)
    (primitive ("sum-complex") sum-complex-prim)
    (primitive ("sub-complex") sub-complex-prim)
    (primitive ("mult-complex") mult-complex-prim)
    (primitive ("div-complex") div-complex-prim)))


;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?)))
;  (letrec-exp
;   (proc-names (list-of symbol?))
;   (idss (list-of (list-of symbol?)))
;   (bodies (list-of expression?))
;   (body-letrec expression?)))

;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) 
      (let ((result (eval-program pgm)))
        (if (complex-number? result)
            (display (show-complex result))
            result))) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env
     '(x y z f)
     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) '())) '())))
                      (empty-env)))
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (imag-exp (datum) 
                (let ((num-str (symbol->string datum)))
                  (let ((num-part (string->number (substring num-str 0 (- (string-length num-str) 1)))))
                    num-part)))
      (string-exp (str) str)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (switch-exp (switch-expr case-vals case-bodies default-body)
                  (eval-switch switch-expr case-vals case-bodies default-body env)))))

; Función auxiliar para evaluar switch
(define eval-switch
  (lambda (switch-expr case-vals case-bodies default-body env)
    (let ((switch-val (eval-expression switch-expr env)))
      (let loop ((vals case-vals) (bodies case-bodies))
        (cond
          ((null? vals) (eval-expression default-body env))
          ((equal? switch-val (eval-expression (car vals) env))
           (eval-expression (car bodies) env))
          (else (loop (cdr vals) (cdr bodies))))))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero o complex-number
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (complex-prim () (complex (car args) (cadr args)))
      (sum-complex-prim () 
                        (let ((z1 (car args)) (z2 (cadr args)))
                          (cases complex-number z1
                            (complex (a b)
                                     (cases complex-number z2
                                       (complex (c d)
                                                (complex (+ a c) (+ b d))))))))
      (sub-complex-prim () 
                        (let ((z1 (car args)) (z2 (cadr args)))
                          (cases complex-number z1
                            (complex (a b)
                                     (cases complex-number z2
                                       (complex (c d)
                                                (complex (- a c) (- b d))))))))
      (mult-complex-prim () 
                         (let ((z1 (car args)) (z2 (cadr args)))
                           (cases complex-number z1
                             (complex (a b)
                                      (cases complex-number z2
                                        (complex (c d)
                                                 (complex (- (* a c) (* b d)) 
                                                         (+ (* a d) (* b c)))))))))
      (div-complex-prim () 
                        (let ((z1 (car args)) (z2 (cadr args)))
                          (cases complex-number z1
                            (complex (a b)
                                     (cases complex-number z2
                                       (complex (c d)
                                                (let ((denom (+ (* c c) (* d d))))
                                                  (complex (/ (+ (* a c) (* b d)) denom)
                                                          (/ (- (* b c) (* a d)) denom))))))))))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Números Complejos
(define-datatype complex-number complex-number?
  (complex
   (real number?)
   (imag number?)))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))


;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))


;****************************************************************************************
;Funciones Auxiliares

; Función para mostrar números complejos de forma legible
(define show-complex
  (lambda (z)
    (cases complex-number z
      (complex (real imag)
               (let ((real-str (if (integer? real) 
                                   (number->string (inexact->exact real))
                                   (number->string real)))
                     (imag-str (if (integer? imag) 
                                   (number->string (inexact->exact imag))
                                   (number->string imag))))
                 (cond
                   ((= imag 0) real-str)
                   ((= real 0) 
                    (cond
                      ((= imag 1) "i")
                      ((= imag -1) "-i")
                      (else (string-append imag-str "i"))))
                   ((= imag 1) (string-append real-str "+i"))
                   ((= imag -1) (string-append real-str "-i"))
                   ((> imag 0) (string-append real-str "+" imag-str "i"))
                   (else (string-append real-str imag-str "i"))))))))

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

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

;******************************************************************************************
;Pruebas

(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x)")
(just-scan "add1(   x   )%cccc")
(just-scan "add1(  +(5, x)   )%cccc")
(just-scan "add1(  +(5, %ccccc x) ")
(scan&parse "add1(x)")
(scan&parse "add1(   x   )%cccc")
(scan&parse "add1(  +(5, x)   )%cccc")
(scan&parse "add1(  +(5, %cccc
x)) ")
(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
(scan&parse "let
x = -(y,1)
in
let
x = +(x,2)
in
add1(x)")

(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
(define exp-numero (lit-exp 8))
(define exp-ident (var-exp 'c))
(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
(define programa (a-program exp-app))
(define una-expresion-dificil (primapp-exp (mult-prim)
                                           (list (primapp-exp (incr-prim)
                                                              (list (var-exp 'v)
                                                                    (var-exp 'y)))
                                                 (var-exp 'x)
                                                 (lit-exp 200))))
(define un-programa-dificil
    (a-program una-expresion-dificil))

;******************************************************************************************
;Ejemplos de uso de números complejos

;(interpretador)

; Ejemplo 1: Crear números complejos y sumarlos
(define ejemplo1-program
  (scan&parse "let
                 a = complex(3, 5i)
                 b = complex(6, -2i)
               in
                 sum-complex(a, b)"))

; Ejemplo 2: Multiplicación de números complejos
(define ejemplo2-program
  (scan&parse "let
                 a = complex(3, 5i)
                 b = complex(6, -2i)
               in
                 mult-complex(a, b)"))

;******************************************************************************************
;Ejemplos de uso del SWITCH
;(interpretador)

(define switch-ejemplo1
  (scan&parse "let
                 x = 5
                 y = 2
                 f = proc(a b) +(a,b)
               in
                 switch ( -(x,y) ) {
                   case 1: {
                     (f x -3)
                   }
                   case 2: {
                     (f y 9)
                   }
                   case 3: {
                     (f x y)
                   }
                   default: {
                     \"casoPorDefecto\"
                   }
                 }"))

; Ejemplo con X=5, Y=4 -> switch evalúa -(5,4) = 1 -> ejecuta caso 1 -> (f 5 -3) = 2
(define switch-ejemplo2
  (scan&parse "let
                 x = 5
                 y = 4
                 f = proc(a b) +(a,b)
               in
                 switch ( -(x,y) ) {
                   case 1: {
                     (f x -3)
                   }
                   case 2: {
                     (f y 9)
                   }
                   case 3: {
                     (f x y)
                   }
                   default: {
                     \"casoPorDefecto\"
                   }
                 }"))

; Ejemplo con X=5, Y=20 -> switch evalúa -(5,20) = -15 -> ejecuta default
(define switch-ejemplo3
  (scan&parse "let
                 x = 5
                 y = 20
                 f = proc(a b) +(a,b)
               in
                 switch ( -(x,y) ) {
                   case 1: {
                     (f x -3)
                   }
                   case 2: {
                     (f y 9)
                   }
                   case 3: {
                     (f x y)
                   }
                   default: {
                     \"casoPorDefecto\"
                   }
                 }"))




