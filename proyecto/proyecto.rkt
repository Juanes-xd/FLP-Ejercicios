#| 
Proyecto Final
Juan David Olaya - 2410206 - 3743
Juan Esteban Ortiz - 2410227 - 3743
Jean Pierre Cardenas - 2510003 - 3743

Github: https://github.com/Juanes-xd/Proyecto-FLP
|#

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
;;                      <variable-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= var {identifier = <expression>}* in <expression>
;;                      <var-exp (ids rands body)>
;;                  ::= const {identifier = <expression>}* in <expression>
;;                      <const-exp (ids rands body)>
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
   (letter (arbno (or letter digit "?" "-" "_"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (imaginary-number
   (digit (arbno digit) "i") symbol)
  (imaginary-number
   ("-" digit (arbno digit) "i") symbol)
  (string
   ("\"" (arbno (not #\")) "\"") string)
  (boolean
   ("true") symbol)
  (boolean
   ("false") symbol)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (boolean) bool-exp)
    (expression (imaginary-number) imag-exp)
    (expression (string) string-exp)
    (expression (identifier) variable-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("var" identifier "=" expression (arbno ";" identifier "=" expression) "in" expression)
                var-exp)
    (expression ("const" identifier "=" expression (arbno ";" identifier "=" expression) "in" expression)
                const-exp)
    (expression ("let" identifier "=" expression (arbno ";" identifier "=" expression) "in" expression)
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
    (expression ("for" identifier "=" expression "to" expression "do" expression) for-exp)
    (expression ("while" expression "do" expression) while-exp)
    (expression ("begin" (arbno expression) "end") begin-exp)
    (expression ("prototipo" "{" (arbno identifier ":" expression) "}") proto-exp)
    (expression ("clone" "(" expression ")") clone-exp)
    (expression ("get-prop" "(" expression "," identifier ")") get-prop-exp)
    (expression ("set-prop" "(" expression "," identifier "," expression ")") set-prop-exp)
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
    (primitive ("div-complex") div-complex-prim)
    (primitive ("vacio") vacio-prim)
    (primitive ("crear-lista") crear-lista-prim)
    (primitive ("cabeza") cabeza-prim)
    (primitive ("cola") cola-prim)
    (primitive ("vacio?") vacio-check-prim)
    (primitive ("lista?") lista-check-prim)
    (primitive ("append") append-prim)
    (primitive ("ref-lista") ref-lista-prim)
    (primitive ("set-lista") set-lista-prim)
    (primitive ("crear-diccionario") crear-diccionario-prim)
    (primitive ("diccionario-set") diccionario-set-prim)
    (primitive ("diccionario-get") diccionario-get-prim)
    (primitive ("diccionario?") diccionario-check-prim)
    (primitive ("claves") claves-prim)
    (primitive ("valores") valores-prim)
    (primitive ("=") equal-prim)
    (primitive ("<") less-prim)
    (primitive (">") greater-prim)
    (primitive ("<=") less-equal-prim)
    (primitive (">=") greater-equal-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("not") not-prim)
    (primitive ("print") print-prim)))


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
        (cond
          ((string? result) result)
          ((boolean? result) (if result 'true 'false))
          ((complex-number? result) (show-complex result))
          (else result)))) 
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
     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (variable-exp 'y) (cons (primapp-exp (decr-prim) (cons (variable-exp 'y) '())) '())))
                      (empty-env)))
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (bool-exp (datum) (symbol->boolean datum))
      (imag-exp (datum) 
                (let ((num-str (symbol->string datum)))
                  (let ((num-part (string->number (substring num-str 0 (- (string-length num-str) 1)))))
                    num-part)))
      (string-exp (str) str)
      (variable-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (var-exp (id1 rand1 ids rands body)
               (let ((val1 (eval-expression rand1 env)))
                 (let ((all-ids (cons id1 ids))
                       (all-vals (cons val1 (eval-rands rands env))))
                   (eval-expression body (extend-env all-ids all-vals env)))))
      (const-exp (id1 rand1 ids rands body)
                 (let ((val1 (eval-expression rand1 env)))
                   (let ((all-ids (cons id1 ids))
                         (all-vals (cons val1 (eval-rands rands env))))
                     (eval-expression body (extend-env-const all-ids all-vals env)))))
      (let-exp (id1 rand1 ids rands body)
               (let ((val1 (eval-expression rand1 env)))
                 (let ((all-ids (cons id1 ids))
                       (all-vals (cons val1 (eval-rands rands env))))
                   (eval-expression body (extend-env all-ids all-vals env)))))
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
                  (eval-switch switch-expr case-vals case-bodies default-body env))
      (for-exp (var start-exp end-exp body)
               (eval-for var start-exp end-exp body env))
      (while-exp (test-exp body)
                 (eval-while test-exp body env))
      (begin-exp (exps)
                 (eval-begin exps env))
      (proto-exp (prop-names prop-exps)
                 (let ((prop-values (map (lambda (e) (eval-expression e env)) prop-exps)))
                   (create-proto-with-props prop-names prop-values)))
      (clone-exp (proto-expr)
                 (let ((proto-obj (eval-expression proto-expr env)))
                   (if (prototype? proto-obj)
                       (proto-clone proto-obj)
                       (eopl:error 'eval-expression "Cannot clone non-prototype ~s" proto-obj))))
      (get-prop-exp (obj-expr prop-name)
                    (let ((obj (eval-expression obj-expr env)))
                      (if (prototype? obj)
                          (proto-get obj prop-name)
                          (eopl:error 'eval-expression "Cannot access property of non-prototype ~s" obj))))
      (set-prop-exp (obj-expr prop-name value-expr)
                    (let ((obj (eval-expression obj-expr env))
                          (value (eval-expression value-expr env)))
                      (if (prototype? obj)
                          (proto-set obj prop-name value)
                          (eopl:error 'eval-expression "Cannot set property of non-prototype ~s" obj)))))))

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

; Función auxiliar para evaluar for
(define eval-for
  (lambda (var start-exp end-exp body env)
    (let ((start-val (eval-expression start-exp env))
          (end-val (eval-expression end-exp env)))
      (let loop ((i start-val))
        (if (<= i end-val)
            (begin
              (eval-expression body (extend-env (list var) (list i) env))
              (loop (+ i 1)))
            0)))))

; Función auxiliar para evaluar while
(define eval-while
  (lambda (test-exp body env)
    (let loop ()
      (if (true-value? (eval-expression test-exp env))
          (begin
            (eval-expression body env)
            (loop))
          0))))

; Función auxiliar para evaluar begin
(define eval-begin
  (lambda (exps env)
    (if (null? exps)
        0
        (let loop ((exps exps))
          (if (null? (cdr exps))
              (eval-expression (car exps) env)
              (begin
                (eval-expression (car exps) env)
                (loop (cdr exps))))))))

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
                                                          (/ (- (* b c) (* a d)) denom)))))))))
      (vacio-prim () '())
      (crear-lista-prim () (cons (car args) (cadr args)))
      (cabeza-prim () (car (car args)))
      (cola-prim () (cdr (car args)))
      (vacio-check-prim () (if (null? (car args)) #t #f))
      (lista-check-prim () (if (list? (car args)) #t #f))
      (append-prim () (append (car args) (cadr args)))
      (ref-lista-prim () (list-ref (car args) (cadr args)))
      (set-lista-prim () (set-lista-helper (car args) (cadr args) (caddr args)))
      (crear-diccionario-prim () (crear-diccionario-helper args))
      (diccionario-set-prim () 
                     (let ((dict (car args))
                           (key (cadr args))
                           (value (caddr args)))
                       (diccionario-set dict key value)))
      (diccionario-get-prim () 
                     (let ((dict (car args))
                           (key (cadr args)))
                       (diccionario-get dict key)))
      (diccionario-check-prim () (if (diccionario? (car args)) #t #f))
      (claves-prim () (diccionario-claves (car args)))
      (valores-prim () (diccionario-valores (car args)))
      (equal-prim () (if (equal? (car args) (cadr args)) #t #f))
      (less-prim () (if (< (car args) (cadr args)) #t #f))
      (greater-prim () (if (> (car args) (cadr args)) #t #f))
      (less-equal-prim () (if (<= (car args) (cadr args)) #t #f))
      (greater-equal-prim () (if (>= (car args) (cadr args)) #t #f))
      (and-prim () (if (and (true-value? (car args)) (true-value? (cadr args))) #t #f))
      (or-prim () (if (or (true-value? (car args)) (true-value? (cadr args))) #t #f))
      (not-prim () (if (true-value? (car args)) #f #t))
      (print-prim () (print-helper (car args))))))

;*******************************************************************************************
;Funciones auxiliares para listas

; my-filter: filtra una lista según un predicado
(define my-filter
  (lambda (pred lst)
    (cond
      ((null? lst) '())
      ((pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))))
      (else (my-filter pred (cdr lst))))))

; my-findf: encuentra el primer elemento que cumple el predicado
(define my-findf
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) (car lst))
      (else (my-findf pred (cdr lst))))))

;*******************************************************************************************
;Funciones auxiliares para listas - continuación

; set-lista-helper: reemplaza el elemento en la posición i por valor
(define set-lista-helper
  (lambda (lst index value)
    (cond
      ((null? lst) (eopl:error 'set-lista-helper "Index ~s out of bounds" index))
      ((= index 0) (cons value (cdr lst)))
      (else (cons (car lst) (set-lista-helper (cdr lst) (- index 1) value))))))

;*******************************************************************************************
;Diccionarios - Implementación simple usando listas de asociación

(define-datatype dictionary dictionary?
  (make-dict
   (bindings list?)))

; crear-diccionario-helper: crea un diccionario vacío o con pares clave-valor iniciales
; Si no hay argumentos, crea un diccionario vacío
; Si hay argumentos, espera pares: clave1 valor1 clave2 valor2 ...
(define crear-diccionario-helper
  (lambda (args)
    (if (null? args)
        (make-dict '())
        (let loop ((remaining-args args) (bindings '()))
          (if (null? remaining-args)
              (make-dict bindings)
              (if (null? (cdr remaining-args))
                  (eopl:error 'crear-diccionario "Número de argumentos incorrecto - se esperan pares clave-valor")
                  (let ((key (car remaining-args))
                        (value (cadr remaining-args)))
                    (loop (cddr remaining-args)
                          (cons (cons key value) bindings)))))))))

; diccionario-set: agrega o actualiza una clave en el diccionario
(define diccionario-set
  (lambda (dict key value)
    (cases dictionary dict
      (make-dict (bindings)
                 (make-dict (cons (cons key value)
                                  (my-filter (lambda (pair) (not (equal? (car pair) key))) 
                                         bindings)))))))

; diccionario-get: obtiene el valor asociado a una clave
(define diccionario-get
  (lambda (dict key)
    (cases dictionary dict
      (make-dict (bindings)
                 (let ((pair (my-findf (lambda (p) (equal? (car p) key)) bindings)))
                   (if pair
                       (cdr pair)
                       (eopl:error 'diccionario-get "Key ~s not found in dictionary" key)))))))

; diccionario?: verifica si un valor es un diccionario
(define diccionario?
  (lambda (val)
    (dictionary? val)))

; diccionario-claves: devuelve una lista con todas las claves del diccionario
(define diccionario-claves
  (lambda (dict)
    (cases dictionary dict
      (make-dict (bindings)
                 (map (lambda (pair) (car pair)) bindings)))))

; diccionario-valores: devuelve una lista con todos los valores del diccionario
(define diccionario-valores
  (lambda (dict)
    (cases dictionary dict
      (make-dict (bindings)
                 (map (lambda (pair) (cdr pair)) bindings)))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (cond
      ((boolean? x) x)
      ((number? x) (not (zero? x)))
      (else #t))))

; symbol->boolean: convierte símbolo a booleano
(define symbol->boolean
  (lambda (sym)
    (cond
      ((eq? sym 'true) #t)
      ((eq? sym 'false) #f)
      (else (eopl:error 'symbol->boolean "Invalid boolean symbol: ~s" sym)))))

; boolean->symbol: convierte booleano a símbolo para mostrar
(define boolean->symbol
  (lambda (b)
    (if b 'true 'false)))

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
;Prototipos - Sistema de objetos basado en prototipos similar a JavaScript

(define-datatype prototype prototype?
  (proto
   (bindings list?)           ; lista de asociación con propiedades (name . value)
   (proto-parent (lambda (x) (or (prototype? x) (null? x))))))  ; referencia al prototipo padre

; create-proto: crea un nuevo prototipo vacío sin padre
(define create-proto
  (lambda ()
    (proto '() '())))

; create-proto-with-props: crea un prototipo con propiedades iniciales
(define create-proto-with-props
  (lambda (property-names property-values)
    (proto (map cons property-names property-values) '())))

; proto-clone: clona un prototipo (crea una nueva referencia con el mismo padre)
(define proto-clone
  (lambda (proto-obj)
    (cases prototype proto-obj
      (proto (bindings parent)
             (proto (list-copy bindings) proto-obj)))))

; proto-get: obtiene una propiedad de un prototipo (busca recursivamente en la cadena de prototipos)
(define proto-get
  (lambda (proto-obj prop-name)
    (cases prototype proto-obj
      (proto (bindings parent)
             (let ((pair (my-findf (lambda (p) (equal? (car p) prop-name)) bindings)))
               (if pair
                   (cdr pair)
                   (if (null? parent)
                       (eopl:error 'proto-get "Property ~s not found in prototype chain" prop-name)
                       (proto-get parent prop-name))))))))

; proto-set: establece una propiedad en un prototipo (mutación local)
(define proto-set
  (lambda (proto-obj prop-name prop-value)
    (cases prototype proto-obj
      (proto (bindings parent)
             (let ((filtered-bindings (my-filter (lambda (p) (not (equal? (car p) prop-name))) bindings)))
               (proto (cons (cons prop-name prop-value) filtered-bindings) parent))))))

; list-copy: copia una lista de asociación
(define list-copy
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (caar lst) (cdar lst)) (list-copy (cdr lst))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (extended-env-const-record (syms (list-of symbol?))
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

;extend-env-const: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido para constantes
(define extend-env-const
  (lambda (syms vals env)
    (extended-env-const-record syms vals env)))

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
      (extended-env-const-record (syms vals old-env)
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

; print-helper: imprime un valor y devuelve el mismo valor
(define print-helper
  (lambda (value)
    (cond
      ((boolean? value)
       (begin
         (display (if value "true" "false"))
         (newline)
         value))
      ((complex-number? value)
       (begin
         (display (show-complex value))
         (newline)
         value))
      ((list? value)
       (begin
         (display value)
         (newline)
         value))
      ((dictionary? value)
       (begin
         (display "#<dictionary>")
         (newline)
         value))
      (else
       (begin
         (display value)
         (newline)
         value)))))

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
(interpretador)
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
(define exp-ident (variable-exp 'c))
(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
(define programa (a-program exp-app))
(define una-expresion-dificil (primapp-exp (mult-prim)
                                           (list (primapp-exp (incr-prim)
                                                              (list (variable-exp 'v)
                                                                    (variable-exp 'y)))
                                                 (variable-exp 'x)
                                                 (lit-exp 200))))
(define un-programa-dificil
    (a-program una-expresion-dificil))

;******************************************************************************************
;Ejemplos de uso de VAR y CONST

; Ejemplo 1: Usar var para declarar variables
(define var-ejemplo1
  (scan&parse "var x = 10 in +(x, 5)"))

; Ejemplo 2: Múltiples variables con var (sin comas)
(define var-ejemplo2
  (scan&parse "var x = 10 y = 20 z = 30 in +(+(x, y), z)"))

; Ejemplo 3: Usar const para declarar constantes
(define const-ejemplo1
  (scan&parse "const pi = 3.14159 in *(pi, 2)"))

; Ejemplo 4: Múltiples constantes (sin comas)
(define const-ejemplo2
  (scan&parse "const x = 5 y = 10 in *(x, y)"))

; Ejemplo 5: Combinar var y const
(define var-const-ejemplo1
  (scan&parse "const pi = 3.14 in const radio = 5 in var area = *(*(pi, radio), radio) in area"))

; Ejemplo 6: Reasignación con var (simulada con var anidado)
(define var-ejemplo3
  (scan&parse "var x = 5 in var x = 10 in x"))

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

;******************************************************************************************
;Ejemplos de uso de OPERACIONES BOOLEANAS

; Ejemplo 1: Operación AND
(define bool-ejemplo1
  (scan&parse "and(>(10, 5), <(3, 5))"))

; Ejemplo 2: Operación OR
(define bool-ejemplo2
  (scan&parse "or(>(2, 10), <(3, 5))"))

; Ejemplo 3: Operación NOT
(define bool-ejemplo3
  (scan&parse "not(>(2, 10))"))

; Ejemplo 4: AND con valores verdadero/falso
(define bool-ejemplo4
  (scan&parse "and(true, true)"))

; Ejemplo 5: OR con valores verdadero/falso
(define bool-ejemplo5
  (scan&parse "or(false, true)"))

; Ejemplo 6: Combinación de booleanos
(define bool-ejemplo6
  (scan&parse "and(or(>(5, 3), <(2, 1)), not(<(10, 5)))"))

; Ejemplo 7: Uso de booleanos en condicional
(define bool-ejemplo7
  (scan&parse "if and(>(x, 0), <(x, 100)) then \"en rango\" else \"fuera de rango\""))

; Ejemplo 8: Operaciones booleanas complejas
(define bool-ejemplo8
  (scan&parse "let
                 a = 10
                 b = 5
                 c = 15
               in
                 or(and(>(a, b), <(a, c)), =(a, 20))"))

; Ejemplo 9: Literales booleanos
(define bool-ejemplo9
  (scan&parse "true"))

; Ejemplo 10: Literales booleanos false
(define bool-ejemplo10
  (scan&parse "false"))

;******************************************************************************************
;Ejemplos de uso de PRINT

; Ejemplo 1: Imprimir un número
(define print-ejemplo1
  (scan&parse "print(42)"))

; Ejemplo 2: Imprimir una cadena
(define print-ejemplo2
  (scan&parse "print(\"Hola desde FlowLang\")"))

; Ejemplo 3: Imprimir resultado de operación
(define print-ejemplo3
  (scan&parse "print(+(10, 20))"))

; Ejemplo 4: Imprimir booleano
(define print-ejemplo4
  (scan&parse "print(>(5, 3))"))

; Ejemplo 5: Imprimir en let
(define print-ejemplo5
  (scan&parse "let
                 x = 100
                 y = print(x)
               in
                 y"))

; Ejemplo 6: Imprimir lista
(define print-ejemplo6
  (scan&parse "print(crear-lista(1, crear-lista(2, crear-lista(3, vacio()))))"))

; Ejemplo 7: Imprimir con condicional
(define print-ejemplo7
  (scan&parse "if >(10, 5) then print(\"10 es mayor que 5\") else print(\"10 NO es mayor que 5\")"))

; Ejemplo 8: Múltiples impresiones
(define print-ejemplo8
  (scan&parse "let
                 a = print(\"Valor de a: \")
                 b = print(5)
                 c = print(\" fin\")
               in
                 +(5, 10)"))

;***************************** ***********************
;Ejemplos de uso de LISTAS

; Ejemplo 1: Crear una lista simple
(define list-ejemplo1
  (scan&parse "crear-lista(1, crear-lista(2, crear-lista(3, crear-lista(4, crear-lista(5, vacio())))))"))

; Ejemplo 2: Usar crear-lista para agregar elementos
(define list-ejemplo2
  (scan&parse "crear-lista(1, crear-lista(2, crear-lista(3, vacio())))"))

; Ejemplo 3: Obtener el primer elemento con cabeza
(define list-ejemplo3
  (scan&parse "cabeza(crear-lista(10, crear-lista(20, crear-lista(30, vacio()))))"))

; Ejemplo 4: Obtener el resto con cola
(define list-ejemplo4
  (scan&parse "cola(crear-lista(10, crear-lista(20, crear-lista(30, vacio()))))"))

; Ejemplo 5: Verificar si es lista vacía
(define list-ejemplo5
  (scan&parse "vacio?(vacio())"))

; Ejemplo 6: Verificar si es una lista
(define list-ejemplo6
  (scan&parse "lista?(crear-lista(1, crear-lista(2, crear-lista(3, vacio()))))"))

; Ejemplo 7: Concatenar listas con append
(define list-ejemplo7
  (scan&parse "append(crear-lista(1, crear-lista(2, crear-lista(3, vacio()))), crear-lista(4, crear-lista(5, crear-lista(6, vacio()))))"))

; Ejemplo 8: Obtener elemento por índice con ref-lista
(define list-ejemplo8
  (scan&parse "ref-lista(crear-lista(10, crear-lista(20, crear-lista(30, crear-lista(40, vacio())))), 2)"))

; Ejemplo 9: Cambiar elemento por índice with set-lista
(define list-ejemplo9
  (scan&parse "set-lista(crear-lista(1, crear-lista(2, crear-lista(3, crear-lista(4, vacio())))), 1, 99)"))

; Ejemplo 10: Operaciones combinadas con listas
(define list-ejemplo10
  (scan&parse "let
                 lista1 = crear-lista(1, crear-lista(2, crear-lista(3, vacio())))
                 lista2 = crear-lista(4, crear-lista(5, crear-lista(6, vacio())))
                 combinada = append(lista1, lista2)
               in
                 ref-lista(combinada, 4)"))

;******************************************************************************************
;Ejemplos de uso de DICCIONARIOS

; Ejemplo 1: Crear diccionario vacío
(define dict-ejemplo1
  (scan&parse "crear-diccionario()"))

; Ejemplo 2: Crear diccionario con pares clave-valor iniciales
(define dict-ejemplo1b
  (scan&parse "crear-diccionario(\"nombre\", \"Juan\", \"edad\", 25)"))

; Ejemplo 3: Agregar elementos al diccionario
(define dict-ejemplo2
  (scan&parse "let
                 d1 = crear-diccionario()
                 d2 = diccionario-set(d1, \"nombre\", \"Juan\")
                 d3 = diccionario-set(d2, \"edad\", 25)
               in
                 d3"))

; Ejemplo 4: Obtener valor del diccionario
(define dict-ejemplo3
  (scan&parse "let
                 d1 = crear-diccionario()
                 d2 = diccionario-set(d1, \"nombre\", \"Juan\")
                 d3 = diccionario-set(d2, \"edad\", 25)
               in
                 diccionario-get(d3, \"nombre\")"))

; Ejemplo 5: Verificar si es diccionario
(define dict-ejemplo4
  (scan&parse "diccionario?(crear-diccionario())"))

; Ejemplo 6: Obtener todas las claves de un diccionario
(define dict-ejemplo5
  (scan&parse "let
                 d = crear-diccionario(\"nombre\", \"Juan\", \"edad\", 25, \"ciudad\", \"Bogotá\")
               in
                 claves(d)"))

; Ejemplo 7: Obtener todos los valores de un diccionario
(define dict-ejemplo6
  (scan&parse "let
                 d = crear-diccionario(\"nombre\", \"Juan\", \"edad\", 25, \"ciudad\", \"Bogotá\")
               in
                 valores(d)"))

; Ejemplo 8: Operaciones con claves y valores
(define dict-ejemplo7
  (scan&parse "let
                 d1 = crear-diccionario(\"x\", 10, \"y\", 20, \"z\", 30)
                 todas-claves = claves(d1)
                 todas-valores = valores(d1)
               in
                 d1"))

;******************************************************************************************
;Ejemplos de uso de FOR

; Ejemplo 1: Sumar números del 1 al 5 (simulado imprimiendo)
(define for-ejemplo1
  (scan&parse "for i = 1 to 5 do i"))

; Ejemplo 2: For con expresión más compleja
(define for-ejemplo2
  (scan&parse "let
                 resultado = 0
               in
                 for i = 1 to 10 do +(i, i)"))

;******************************************************************************************
;Ejemplos de uso de WHILE

; Ejemplo 1: While simple
(define while-ejemplo1
  (scan&parse "let
                 x = 5
               in
                 while >(x, 0) do -(x, 1)"))

;******************************************************************************************
;Ejemplos de uso de BEGIN

; Ejemplo 1: Ejecutar múltiples expresiones en secuencia
(define begin-ejemplo1
  (scan&parse "begin
                 +(1, 2)
                 *(3, 4)
                 -(10, 5)
               end"))

; Ejemplo 2: Begin con let
(define begin-ejemplo2
  (scan&parse "let
                 x = 5
                 y = 10
               in
                 begin
                   +(x, y)
                   *(x, y)
                   -(y, x)
                 end"))

;******************************************************************************************
;Ejemplos de uso de PROTOTIPOS

; Ejemplo 1: Crear un prototipo simple
(define proto-ejemplo1
  (scan&parse "prototipo { nombre: \"Ana\" edad: 25 }"))

; Ejemplo 2: Crear prototipo persona con propiedades
(define proto-ejemplo2
  (scan&parse "let
                 persona = prototipo { nombre: \"Juan\" edad: 30 }
               in
                 get-prop(persona, nombre)"))

; Ejemplo 3: Acceder a propiedad
(define proto-ejemplo3
  (scan&parse "let
                 persona = prototipo { nombre: \"Ana\" edad: 25 }
               in
                 get-prop(persona, edad)"))

; Ejemplo 4: Modificar propiedad
(define proto-ejemplo4
  (scan&parse "let
                 persona = prototipo { nombre: \"Juan\" edad: 30 }
                 persona_mod = set-prop(persona, edad, 31)
               in
                 get-prop(persona_mod, edad)"))

; Ejemplo 5: Clonar prototipo (herencia)
(define proto-ejemplo5
  (scan&parse "let
                 persona = prototipo { nombre: \"Ana\" edad: 25 }
                 estudiante = clone(persona)
               in
                 get-prop(estudiante, nombre)"))

; Ejemplo 6: Clonar y extender propiedades
(define proto-ejemplo6
  (scan&parse "let
                 persona = prototipo { nombre: \"Juan\" edad: 30 }
                 estudiante = clone(persona)
                 est_mod = set-prop(estudiante, promedio, 4.3)
               in
                 get-prop(est_mod, promedio)"))

; Ejemplo 7: Múltiples clones (independencia)
(define proto-ejemplo7
  (scan&parse "let
                 vehiculo = prototipo { tipo: \"auto\" }
                 carro = clone(vehiculo)
                 carro_mod = set-prop(carro, modelo, \"Sedan\")
               in
                 get-prop(carro_mod, modelo)"))

; Ejemplo 8: Herencia en cadena
(define proto-ejemplo8
  (scan&parse "let
                 sensor = prototipo { encendido: 0 tipo: \"temperatura\" }
                 sensor1 = clone(sensor)
                 sen1_mod = set-prop(sensor1, encendido, 1)
               in
                 get-prop(sen1_mod, encendido)"))

;***************************** ***********************
;Ejemplos combinados

(define todos-valores-programa
  (scan&parse "let
                 entero = 42
                 flotante = 3.14159
                 complejo = complex(3, 4)
                 nulo = vacio()
                 cadena = \"Hola FlowLang\"
                 booleano = true
                 procedimiento = proc(x) +(x, 10)
                 lista = crear-lista(1, crear-lista(2, crear-lista(3, vacio())))
                 diccionario = crear-diccionario(\"nombre\", \"Juan\")
                 prototipo = prototipo { tipo: \"estudiante\" }
               in
                 diccionario"))

; Ejemplo 1: Lista con operaciones
(define combinado-ejemplo1
  (scan&parse "let
                 lista = crear-lista(1, crear-lista(2, crear-lista(3, crear-lista(4, crear-lista(5, vacio())))))
                 primero = cabeza(lista)
                 resto = cola(lista)
               in
                 +(primero, cabeza(resto))"))

; Ejemplo 2: Diccionario con operaciones matemáticas
(define combinado-ejemplo2
  (scan&parse "let
                 d = crear-diccionario()
                 d1 = diccionario-set(d, \"x\", 10)
                 d2 = diccionario-set(d1, \"y\", 20)
                 x = diccionario-get(d2, \"x\")
                 y = diccionario-get(d2, \"y\")
               in
                 +(x, y)"))

; Ejemplo 3: For con lista
(define combinado-ejemplo3
  (scan&parse "let
                 suma = 0
               in
                 for i = 1 to 5 do +(i, 10)"))

;******************************************************************************************
; EXPORTAR FUNCIONES PARA USO EXTERNO
(provide (all-defined-out))



