#lang eopl



;************************************* PUNTO 11 A EVALAUAR *****************************************

;Punto a)

; Procedimiento areaCirculo que permite calcular el área de un circulo. Se declara la variable @radio como parámetro.
;
; -->  declarar (
;      @radio=2.5;
;      @areaCirculo= procedimiento (@radio) haga (3.14159265358979323846 *(@radio * @radio)) finProc
;      ) { 
;          evaluar @areaCirculo(@radio) finEval  
;        }

; > 19.634954084936208



;Punto b)

; Programa recursivo que calcula el factorial de un número n. Usa el procedimiento recursivo
; letrec propuesto por el grupo. 
;
;
;-->letrec 
;       @factorial(@numero) = 
;                  Si @numero entonces (@numero * evaluar @factorial((@numero ~ 1)) finEval) sino 1 finSI
;        {evaluar @factorial(5) finEval}
;
; > 120
;
;
;-->letrec 
;       @factorial(@numero) = 
;                  Si @numero entonces (@numero * evaluar @factorial((@numero ~ 1)) finEval) sino 1 finSI
;        {evaluar @factorial(10) finEval}
;
; > 3628800



;Punto c)

; Se define un procedimiento de suma recursiva, se usa la variable @suma para su invocación.
;
; --> letrec
;     @sumar(@a, @b) =
;            Si @a entonces evaluar @sumar(sub1(@a),add1(@b)) finEval sino @b finSI
;     {evaluar @sumar(4,5) finEval}
;
; > 9



;Punto d)

; Procedimientos recursivos que permita restar y multiplicar dos números haciendo uso
; de solamente las primitivas add1 y sub1

; --> letrec
;     @restar(@a, @b) =
;            Si @b entonces evaluar @restar(sub1(@a),sub1(@b)) finEval sino @a finSI
;     {evaluar @restar(10,3) finEval}
;
; > 7

;
;--> letrec
;     @sumar(@a, @b) =
;            Si @a entonces evaluar @sumar(sub1(@a),add1(@b)) finEval sino @b finSI
;     @multiplicar(@a, @b) =
;            Si @a entonces evaluar @sumar( evaluar @multiplicar(sub1(@a), @b) finEval, @b) finEval sino @a finSI
;     {evaluar @multiplicar(10,3) finEval}
;
; > 30



;Punto e)

; Se definen tres procedimientos: @integrantes, @saludar y @decorate. El procedimiento
; @integrantes retorna una cadena de texto con los nombres "Kevin-Miguel-Néstor".
; El procedimiento @saludar espera un parámetro n para retornar la cadena de texto "Hola:" concatenado
; con el parámetro n. El procedimiento @decorate evalúa el procedimiento @saludar con el parámetro
; @integrantes. Se espera que la salida sea: "Hola:Kevin-Miguel-y-Néstor".

; -->letrec
;     @integrantes() = "Kevin-Miguel-y-Néstor"
;     @saludar(@n) = ("Hola:" concat evaluar @n() finEval) 
;     @decorate() = evaluar @saludar(@integrantes) finEval
;     {evaluar @decorate() finEval}
;
; > "Hola:Kevin-Miguel-y-Néstor"



;Punto f)

; Se toman los procedimeintos del punto anterior (punto e), se edita el procedimiento @decorate para que
; concatene una cadena m de texto al final del procedimiento a evaluar, en este caso al procedimiento
; @decorate se le pasará un parámetro m para que concatene al final del procedimiento @saludar.
; Se espera que la salida sea: "Hola:Kevin-Miguel-y-Néstor-EstudiantesFLP"

; --> letrec
;     @integrantes() = "Kevin-Miguel-y-Néstor"
;     @saludar(@n) = ("Hola:" concat evaluar @n() finEval)
;     @decorate(@m) = (evaluar @saludar(@integrantes) finEval concat @m)
;     {evaluar @decorate("-EstudiantesFLP") finEval}
;
; > "Hola:Kevin-Miguel-y-Néstor-EstudiantesFLP"


;******************************************************************************************



;******************************************************************************************
;;  Interpretador Simple

;;  Definición BNF para las expresiones del lenguaje:
;;
;;  <programa>         ::= <expression>
;;                          <un-programa (exp)>
;;
;;
;;  <expression>       ::= <numero>
;;                         <numero-lit (num)>
;;
;;                     ::= "\""<texto>"\""
;;                         <texto-lit (txt)>
;;
;;                     ::= <identificador>
;;                         <var-exp (id)>
;;
;;                     ::= (<expresion> <primitiva-binaria> <expresion>)
;;                         <primapp-bin-exp (exp1 prim-binaria exp2)>
;;
;;                     ::= <primitiva-unaria> (<expresion>)
;;                         <primapp-un-exp (prim-unaria exp)>
;;
;;                     ::= Si <expresion> entonces <expresion> sino <expresion> finSI
;;                         <condicional-exp (test-exp true-exp false-exp)>
;;
;;                     ::= declarar ({<identificador>=<expresion>}*(;)) {<expresion>}
;;                         <variableLocal-exp (ids exps cuerpo)
;;
;;                     ::= procedimiento ({<identificador>}*(,)) haga <expresion> finProc
;;                         <procedimiento-exp (ids cuerpo)>
;;
;;                     ::= evaluar <expresion> ({expresion}*(,)) finEval
;;                         <app-exp (rator rands)>
;;
;;                     ::= letrec {<identificador> ({<identificador}*(,)) = <expresion>}* {<expresion>}
;;                         <letrec-exp (proc-names idss bodies letrec-body)
;;
;;
;;
;; <primitiva-binaria> ::= + (primitiva-suma)
;;                     ::= ~ (primitiva-resta)
;;                     ::= * (primitiva-div)
;;                     ::= concat (primitiva-concat)
;;
;;
;;
;; <primitiva-unaria>  ::= longitud (primitiva-longitud)
;;                     ::= add1 (primitiva-add1)
;;                     ::= sub1 (primitiva-sub1)

;******************************************************************************************

;Especificación léxica
(define scanner-spec-simple-interpreter
  '((white-sp ;Espacios en blanco
     (whitespace) skip) 
    (comentario ;Comentarios
     ("//" (arbno (not #\newline))) skip);
    (texto
     ((or letter "-") (arbno (or letter digit "-" ":"))) string);Esta regla define cómo reconocer y manejar cadenas de texto.
                                                                ;Puede comenzar con una letra o un guión ("-") y luego puede contener letras, dígitos, guiones y dos puntos.
    (identificador ;Identificadores
     ("@" (arbno letter)) symbol)
    (numero ;Número entero positivo
     (digit (arbno digit)) number)
    (numero ;Número entero negativo
     ("-" digit (arbno digit)) number)
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number) ;Número decimal positivo
    (numero ;Número decimal negativo
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ))

;Especificación Sintáctica (Gramática)
(define grammar-simple-interpreter
  '(;Programa
    (programa (expresion) un-programa)

    ;Expresiones

    ;Una expresión que es un número literal
    (expresion (numero) numero-lit)

    ;Una expresión que es un texto literal entre comillas dobles
    (expresion ("\""texto"\"") texto-lit)

    ;Una expresión que es un identificador
    (expresion (identificador) var-exp)

    ;Expresión que contiene una operación binaria
    (expresion
     ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    ;Expresión que es una operación unaria entre paréntesis
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    
    ;Condicional
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
    
    ;Variables Locales
    (expresion ("declarar" "("  (separated-list identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)
    
    ;Procedimientos
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
    
    ;Recursividad
     (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "{" expresion "}") 
                letrec-exp)
     
    ;Primitivas-binarias
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ;Primitivas-unarias
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    
    ))

;Construcción de datos automáticamente
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;Definición de la función show-the-datatypes
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

; El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

; El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

; El Interpretador (FrontEnd + Evaluación + Señal para lectura)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************

;El Interpretador

;eval-program: <programa> -> numero
;función que toma en cuenta un ambiente específico (previamente establecido dentro del programa) para evaluar su funcionamiento.

(define eval-program
  (lambda (pgm)
    ; Se utiliza el macro/casos 'cases' para hacer coincidencia de patrones sobre 'pgm'
    (cases programa pgm
      ; Si 'pgm' coincide con el patrón '(un-programa (body))', entonces:
      (un-programa (body)
                   ; Se evalúa la expresión 'body' en el entorno inicial
                   (eval-expresion body (init-env))))))

;Ambiente inicial
;Inicializa y declara el ambiente con el que el interpretador comenzará

; Definición de la función init-env
(define init-env
  (lambda ()
    ; Se llama a la función extend-env con tres argumentos:
    ; 1. Una lista de símbolos: '@a', '@b', '@c', '@d', '@e'
    ; 2. Una lista de valores asociados a los símbolos: 1, 2, 3, "hola", "FLP"
    ; 3. El resultado de llamar a la función empty-env
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))


;eval-expresion: <expresion> <environment> -> numero
;Evalúa la expresión dentro del ambiente de entrada.

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      ; Caso: Número literal. Retorna el propio número.
      (numero-lit (num) num)
      
      ; Caso: Texto literal. Retorna el propio texto.
      (texto-lit (txt) txt)
      
      ; Caso: Expresión de variable. Busca la variable en el entorno y la retorna.
      (var-exp (id) (buscar-variable env id))
      
      ; Caso: Aplicación de operador binario. Evalúa los operandos y aplica la operación.
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let (
                             (args1 (eval-rand exp1 env))
                             (args2 (eval-rand exp2 env))
                             )
                         (apply-primitiva-binaria args1 prim-binaria args2)))
      
      ; Caso: Aplicación de operador unario. Evalúa el operando y aplica la operación.
      (primapp-un-exp (prim-unaria exp)
                      (let (
                            (args (eval-rand exp env))
                            )
                        (apply-primitiva-unaria prim-unaria args)))
      
      ; Caso: Expresión condicional. Evalúa la condición y retorna la rama correspondiente.
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (eval-expresion test-exp env))
                           (eval-expresion true-exp env)
                           (eval-expresion false-exp env)))
      
      ; Caso: Expresión de variable local. Evalúa los valores y extiende el entorno.
      (variableLocal-exp (ids exps cuerpo)
                          (let ((args (eval-rands exps env)))
                           (eval-expresion cuerpo (extend-env ids args env))))
                           
      ; Caso: Expresión de procedimiento. Crea una cerradura(closure).
      (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo env))
      
      ; Caso: Aplicación de procedimiento. Evalúa el procedimiento y aplica los argumentos.
      (app-exp (rator rands)
               (let ((proc (eval-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion
                                 "Intento de aplicar un no-procedimiento ~s" proc))))
      
      ; Caso: Expresión letrec. Evalúa el cuerpo con entorno extendido de forma recursiva.
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))))
  )


;Función auxiliar para aplicar eval-rand para cada elemento (exp1 y exp2)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;Función auxiliar para evaluar un dato "rand"
(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

;apply-primitiva-binaria: <expresion> <primitiva> <expresion> -> número o string
(define apply-primitiva-binaria
  (lambda (exp1 prim exp2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ exp1 exp2))
      (primitiva-resta () (- exp1 exp2))
      (primitiva-multi () (* exp1 exp2))
      (primitiva-div () (/ exp1 exp2))
      (primitiva-concat () (string-append exp1 exp2))
      )))

;apply-primitiva-unaria: <primitiva> <expresion> -> numero
(define apply-primitiva-unaria
  (lambda (prim exp)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length exp))
      (primitiva-add1 () (+ exp 1))
      (primitiva-sub1 () (- exp 1))
      )))

;valor-verdad? <expresion> -> Boolean
;Evalúa si un valor es verdadero (#t), siendo falso (#f) si el valor es 0.
(define valor-verdad?
  (lambda(x)
    (not (zero? x))))

;*******************************************************************************************

;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  ; Representa un ambiente vacío (sin símbolos ni valores)
  (empty-env-record)
  ; Representa un ambiente extendido con símbolos, valores y una referencia a un ambiente anterior
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  ; Representa un ambiente extendido de manera recursiva con proc-names, ids, bodies y un ambiente anterior
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?))
)


(define scheme-value? (lambda (v) #t))

;empty-env: -> environment
;Una función que genera un entorno vacío.
(define empty-env  
  (lambda ()
    (empty-env-record)));llamado al constructor de ambiente vacío

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;Función que amplía el ambiente vinculando símbolos a valores.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;función que extiende recursivamente el ambiente vinculando nombres de procedimientos, listas de argumentos y cuerpos.
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;Función que busca un símbolo en un ambiente
;Función llamada buscar-variable que toma dos argumentos: env (ambiente) y sym (un símbolo que representa una variable).
(define buscar-variable
  (lambda (env sym)
    ; El mecanismo env esta manejando tres casos diferentes de ambientes: empty-env-record, extended-env-record y recursively-extended-env-record.
    (cases environment env
      ;En el caso de empty-env-record, si el entorno está vacío, se lanza un error indicando que la variable no existe.
      (empty-env-record ()
                        (eopl:error "Error, la variable no existe"))
      ;En el caso de extended-env-record, se espera una lista de símbolos (syms), una lista de valores (vals) y un ambiente (env).
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym))))
      ;En el caso de recursively-extended-env-record, este caso esta tratando con un ambiente que ha sido extendido de forma recursiva.
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       ;En este bloque let, se define una variable local pos para almacenar la posición de sym en la lista de símbolos syms.
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env sym)))))))

;Una función que se encarga de crear closures (cerraduras)
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb environment?)
   )
  )

;apply-procedure: Evalúa el cuerpo de un procedimiento en un ambiente extendido
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (eval-expresion body (extend-env ids args env))))))



;****************************************************************************************

;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

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

;Se inicializa  el interpretador
(interpretador)

;******************************************************************************************

;***************************** PUNTOS EXTRA (Punto 9 y 10) ********************************

; ;Ejemplo 1, 2 y 3 del punto 7 con su respectivo Diagrama de Árbol de Sintaxis Abstracta y Diagrama de Ambiente
; 
; 
; .
; .
; .
; .
; .
; .
; 













