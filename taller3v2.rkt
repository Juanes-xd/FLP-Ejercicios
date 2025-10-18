
#lang eopl
#|

Taller 3
Juan David Olaya - 2410206 - 3743
Juan Esteban Ortiz - 2410227 - 3743
Jean Pierre Cardenas - 2510003 - 3743


1) Diseñe un interpretador para la siguiente gramática que realiza operaciones con notación infija:



Valores denotados: Texto + Número + Booleano + ProcVal

Valores expresado: Texto + Número + Booleano + ProcVal



<programa> :=  <expresion>

               un-programa (exp)



<expresion> := <numero>

               numero-lit (num)

            := "\""<texto> "\""

               texto-lit (txt)

            := <identificador>

               var-exp (id)

             := (<expresion> <primitiva-binaria> <expresion>)

               primapp-bin-exp (exp1 prim-binaria exp2)

              := <primitiva-unaria> (<expresion>)

               primapp-un-exp (prim-unaria exp)



<primitiva-binaria> :=  + (primitiva-suma)

  :=  ~ (primitiva-resta)

  :=  / (primitiva-div)

  :=  * (primitiva-multi)

  :=  concat (primitiva-concat)



<primitiva-unaria>:=  longitud (primitiva-longitud)

            :=  add1 (primitiva-add1)

            :=  sub1 (primitiva-sub1)



Tenga en cuenta que:



<numero>: Debe definirse para valores decimales y enteros (positivos y negativos)

<texto>: Debe definirse para cualquier texto escrito en racket

<identificador>: En este lenguaje todo identificador iniciará con el símbolo  @, es decir las variables @x y @z son válidas





2)Defina un ambiente inicial con las variables (@a @b @c @d @e) con valores (1 2 3 "hola" "FLP") y modifique su función evaluar-expresión para que acepte dicho ambiente.



-Diseñe una función llamada (buscar-variable) que recibe un símbolo (identificador) y un ambiente, retorna el valor si encuentra la variable en el ambiente. En caso contrario: "Error, la variable no existe"



Pruebe:

--> @a

1

--> @b

2

--> @e

"FLP"



3) Implemente los Booleanos:

En una expresión numérica, 0 es falso, cualquier otro caso es verdadero. Para esto diseñe la función valor-verdad? que realiza esta verificación.



4) Extienda la gramática con condicionales:

<expresion> := Si <expresion> entonces <expresion>  sino <expresion> finSI

               condicional-exp (test-exp true-exp false-exp)



Debe probar:

--> Si (2+3) entonces 2 sino 3 finSI

2

--> Si (longitud(@d) ~ 4) entonces 2 sino 3 finSI

3



5) Implemente declaración de variables locales:

<expresion> := declarar (<identificador> = <expresion> (;)) { <expresion> }

               variableLocal-exp (ids exps cuerpo)



Debe probar:

--> declarar (@x=2;@y=3;@a=7){ 

       (@a+(@x~@y)) 

    }

6

--> declarar (@x=2;@y=3;@a=7) { 

      (@a+@b) 

   }

9



6) Extienda la gramática para crear procedimientos

<expresion> := procedimiento (<identificador>*',') haga <expresion> finProc

              procedimiento-ex (ids cuero)



Para esto debe definir un datatype para la cerradura (o ProcVal) que debe tener 3 campos:



1. Lista ID del procedimiento

2. Cuerpo del procedimiento

3. Ambiente donde fue declarado



(define-datatype procVal procVal?

  (cerradura

   (lista-ID (list-of symbol?))

   (exp expresion?)

   (amb ambiente?)

   )

  )



Debe probar:



--> procedimiento (@x,@y,@z) haga ((@x+@y)+@z) finProc



#(struct:cerradura (@x @y @z) #(struct:primapp-bin-exp #(struct:primapp-bin-exp #(struct:var-exp @x) #(struct:primitiva-sum) #(struct:var-exp @y)) #(struct:primitiva-sum) #(struct:var-exp @z)) #(struct:extendido (@a @b @c @d @e) (1 2 3 "hola" "FLP") #(struct:vacio)))

Se debe retornar una cerradura



7) Extienda la gramática para evaluar procedimientos:

<expresion> :=  "evaluar" expresion   (expresion ",")*  finEval

                               app-exp(exp exps) 

Debe probar:



-->  declarar (

      @x=2;

      @y=3;

      @a=procedimiento (@x,@y,@z) haga ((@x+@y)+@z) finProc

     ) { 

         evaluar @a (1,2,@x) finEval  

       }



5

 

--> declarar (

     @x=procedimiento (@a,@b) haga ((@a*@a) + (@b*@b)) finProc;

     @y=procedimiento (@x,@y) haga (@x+@y) finProc

    ) { 

      ( evaluar @x(1,2) finEval + evaluar @y(2,3) finEval )  

     }



10





--> declarar (

      @x= Si (@a*@b) entonces (@d concat @e) sino longitud((@d concat @e)) finSI;

      @y=procedimiento (@x,@y) haga (@x+@y) finProc

   ) { 

      ( longitud(@x) * evaluar @y(2,3) finEval )  

     }



35



8) Extienda la gramática para incluir llamados recursivos. Proponga una definición en la gramática e impleméntela.



9) Utilización del lenguaje de programación:



Sólo esta parte del taller será evaluada:

a) 10pts. Escriba un programa en su lenguaje de programación que contenga un procedimiento areaCirculo que permita calcular el area de un circulo dado un radio (A=PI*r*r). Debe incluir valores flotantes en su lenguaje de programación. Deberá invocarlo utilizando una variable @radio como parámetro:

-->  declarar (

      @radio=2.5;

      @areaCirculo= //aquí va el procedimiento

     ) { 

         evaluar @areaCirculo (@radio) finEval  

       }



b) 5pts. Escriba un programa en su lenguaje de programación que contenga un procedimiento que permita calcular el factorial de un número n. Como la gramática para funciones recursivos debe ser propuesta por el grupo, incluya dos ejemplos de uso para el factorial de 5 y el factorial de 10.

c) 10pts. Escriba un programa en su lenguaje de programación que contenga un procedimiento que permita calcular una suma de forma recursivo. Debe hacer uso de las funciones add1 y sub1 (remitase a la clase donde se implementó la interfaz con las funciones zero, isZero?, sucessor, predecessor). Si no se evidencia el uso de add1 y sub1, el ejercicio no será valido. Incluya un llamado a la función recursivo: "evaluar @sumar (4, 5) finEval "

d) 15pts. Escriba un programa en su lenguaje de programación que permita restar y multiplicar dos números haciendo uso solamente de las primitivas add1 y sub1. Incluya llamados:  "evaluar @restar (10, 3) finEval  ",  "evaluar @multiplicar (10, 3) finEval  ".

e) 25pts. En python se puede utilizar algo que se llaman decoradores (por favor leer aquí). Crea una función @integrantes que muestre los nombres de los integrantes del grupo y adicionalmente crea un decorador que al invocarlo salude a los integrantes:

. . .

@integrantes = //un procedimiento que retorna un string con los nombres de los integrantes, ejemplo"Robinson-y-Sara"

. . .

@saludar = //un procedimiento que recibe un procedimiento y retorna otro procedimiento que le agrega el string "Hola:" a la invocación del procedimiento que recibió en sus argumentos.

. . .

//  Creación del decorador

@decorate = evaluar @saludar (@integrantes) finEval      

. . .

// Invocación del decorador

evaluar @decorate ( ) finEval   //Deberá retornar "Hola:Robinson-y-Sara"


Esto quiere decir que la función @saludar recibe como parámetro la función @integrantes y a su vez, retorna una función que se almacena en la variable @decorate. Esta última función, al ser invocada, deberá retornar un saludo a los integrantes (cualquier implementación sin el concepto de decorador no será evaluada).




f) 35pts. Modifique el ejercicio anterior para que el decorador reciba como parámetro otro mensaje que debe ponerse al final de todo el string (cualquier implementación sin el concepto de decorador no será evaluada). Ejemplo

// Invocación del decorador

evaluar @decorate ("-ProfesoresFLP") finEval  //Deberá retornar "Hola:Robinson-y-Sara-ProfesoresFLP"


|#


;Especificación léxica
(define scanner-spec-simple-interpreter
  '((white-sp 
     (whitespace) skip) 
    (comentario 
     ("//" (arbno (not #\newline))) skip);
    (texto
     ((or letter "-") (arbno (or letter digit "-" ":"))) string)                                                          
    (identificador 
     ("@" (arbno letter)) symbol)
    (numero 
     (digit (arbno digit)) number)
    (numero 
     ("-" digit (arbno digit)) number)
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number) 
    (numero 
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ))

;Especificación Sintáctica (Gramática)
(define grammar-simple-interpreter
  '(
    (programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion ("\""texto"\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion
     ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
    
    (expresion ("declarar" "("  (separated-list identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)

    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
    
    ;Recursividad
     (expresion ("recursivo" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "{" expresion "}") 
                recursivo-exp)
     
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
    (cases programa pgm
      (un-programa (body)
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
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let (
                             (args1 (eval-rand exp1 env))
                             (args2 (eval-rand exp2 env))
                             )
                         (apply-primitiva-binaria args1 prim-binaria args2)))
      (primapp-un-exp (prim-unaria exp)
                      (let (
                            (args (eval-rand exp env))
                            )
                        (apply-primitiva-unaria prim-unaria args)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (eval-expresion test-exp env))
                           (eval-expresion true-exp env)
                           (eval-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                          (let ((args (eval-rands exps env)))
                           (eval-expresion cuerpo (extend-env ids args env))))
      (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo env))
      (app-exp (rator rands)
               (let ((proc (eval-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion
                                 "Intento de aplicar un no-procedimiento ~s" proc))))
      (recursivo-exp (proc-names idss bodies recursivo-body)
                  (eval-expresion recursivo-body
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
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
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

;función que extiende recursivomente el ambiente vinculando nombres de procedimientos, listas de argumentos y cuerpos.
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;Función que busca un símbolo en un ambiente
;Función llamada buscar-variable que toma dos argumentos: env (ambiente) y sym (un símbolo que representa una variable).
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error "Error, la variable no existe"))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
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


;****************************************************************************************
;CASOS DE PRUEBA PARA LOS PUNTOS DEL TALLER
;****************************************************************************************

#|
===================================================================================
PUNTO A (10 pts): Área de círculo con valores flotantes
===================================================================================
Fórmula: A = PI * r * r
Usar PI = 3.1416

Caso de prueba:
 -->  declarar ( @radio=5;
      @areaCirculo= procedimiento (@radio) haga (3.141592 *(@radio * @radio))
      finProc ) { evaluar @areaCirculo(@radio) finEval }

  78.5398

 --> declarar( @radio=3.7;
          @areaCirculo = procedimiento (@radio) haga (3.141592 * (@radio * @radio))
          finProc ) { evaluar @areaCirculo(@radio) finEval }

  43.00839448000001


===================================================================================
PUNTO B (5 pts): Factorial recursivo
===================================================================================
Factorial(n) = n * Factorial(n-1), con caso base Factorial(0) = 1

Caso de prueba - Factorial de 5:
--> recursivo
      @factorial(@n)=
        Si @n entonces (@n * evaluar @factorial((@n~1)) finEval) sino 1 finSI
    {
      evaluar @factorial(5) finEval
    }

Resultado esperado: 120


Caso de prueba - Factorial de 10:
--> recursivo
      @factorial(@n)=
        Si @n entonces (@n * evaluar @factorial((@n~1)) finEval) sino 1 finSI
    {
      evaluar @factorial(10) finEval
    }

Resultado esperado: 3628800


===================================================================================
PUNTO C (10 pts): Suma recursivo usando add1 y sub1
===================================================================================
Sumar(a, b) = si b es 0, retorna a, sino Sumar(add1(a), sub1(b))

Caso de prueba:
--> recursivo
      @sumar(@x,@y)=
        Si @y entonces evaluar @sumar(add1(@x),sub1(@y)) finEval sino @x finSI
    {
      evaluar @sumar(4,5) finEval
    }

Resultado esperado: 9


Prueba adicional:
--> recursivo
      @sumar(@x,@y)=
        Si @y entonces evaluar @sumar(add1(@x),sub1(@y)) finEval sino @x finSI
    {
      evaluar @sumar(10,25) finEval
    }

Resultado esperado: 35


===================================================================================
PUNTO D (15 pts): Restar y multiplicar usando solo add1 y sub1
===================================================================================

RESTA: Restar(a, b) = si b es 0, retorna a, sino Restar(sub1(a), sub1(b))

Caso de prueba - Resta:
--> recursivo
      @restar(@x,@y)=
        Si @y entonces evaluar @restar(sub1(@x),sub1(@y)) finEval sino @x finSI
    {
      evaluar @restar(10,3) finEval
    }

Resultado esperado: 7


MULTIPLICACIÓN: Multiplicar(a, b) = si b es 0, retorna 0, sino a + Multiplicar(a, sub1(b))

Caso de prueba - Multiplicación:
--> recursivo
      @multiplicar(@x,@y)=
        Si @y entonces (@x + evaluar @multiplicar(@x,sub1(@y)) finEval) sino 0 finSI
    {
      evaluar @multiplicar(10,3) finEval
    }

Resultado esperado: 30


Pruebas combinadas (resta y multiplicación juntas):
--> recursivo
      @restar(@x,@y)=
        Si @y entonces evaluar @restar(sub1(@x),sub1(@y)) finEval sino @x finSI
      @multiplicar(@x,@y)=
        Si @y entonces (@x + evaluar @multiplicar(@x,sub1(@y)) finEval) sino 0 finSI
    {
      (evaluar @restar(10,3) finEval + evaluar @multiplicar(10,3) finEval)
    }

Resultado esperado: 37 (7 + 30)


===================================================================================
PUNTO E (25 pts): Decorador simple - Saludo a integrantes
===================================================================================
El decorador @saludar recibe una función y retorna otra función que agrega "Hola:"

Caso de prueba:
--> declarar (
      @integrantes=procedimiento () haga "Juan-David-Jean" finProc;
      @saludar=procedimiento (@f) haga 
        procedimiento () haga 
          ("Hola:" concat evaluar @f() finEval) 
        finProc 
      finProc
    ) {
      declarar (
        @decorate=evaluar @saludar(@integrantes) finEval
      ) {
        evaluar @decorate() finEval
      }
    }

Resultado esperado: "Hola:Juan-David-Jean"


Prueba con otros nombres:
--> declarar (
      @integrantes=procedimiento () haga "Robinson-y-Sara" finProc;
      @saludar=procedimiento (@f) haga 
        procedimiento () haga 
          ("Hola:" concat evaluar @f() finEval) 
        finProc 
      finProc
    ) {
      declarar (
        @decorate=evaluar @saludar(@integrantes) finEval
      ) {
        evaluar @decorate() finEval
      }
    }

Resultado esperado: "Hola:Robinson-y-Sara"


===================================================================================
PUNTO F (35 pts): Decorador con parámetro adicional
===================================================================================
El decorador recibe un mensaje adicional que se agrega al final

Caso de prueba:
--> declarar (
      @integrantes=procedimiento () haga "Juan-David-Jean" finProc;
      @saludar=procedimiento (@f) haga 
        procedimiento (@msg) haga 
          (("Hola:" concat evaluar @f() finEval) concat @msg) 
        finProc 
      finProc
    ) {
      declarar (
        @decorate=evaluar @saludar(@integrantes) finEval
      ) {
        evaluar @decorate("-ProfesoresFLP") finEval
      }
    }

Resultado esperado: "Hola:Juan-David-Jean-ProfesoresFLP"


Prueba con mensaje diferente:
--> declarar (
      @integrantes=procedimiento () haga "Robinson-y-Sara" finProc;
      @saludar=procedimiento (@f) haga 
        procedimiento (@msg) haga 
          (("Hola:" concat evaluar @f() finEval) concat @msg) 
        finProc 
      finProc
    ) {
      declarar (
        @decorate=evaluar @saludar(@integrantes) finEval
      ) {
        evaluar @decorate("-EOPL-2025") finEval
      }
    }

Resultado esperado: "Hola:Robinson-y-Sara-EOPL-2025"


Prueba con despedida:
--> declarar (
      @integrantes=procedimiento () haga "Juan-David-Jean" finProc;
      @saludar=procedimiento (@f) haga 
        procedimiento (@msg) haga 
          (("Hola:" concat evaluar @f() finEval) concat @msg) 
        finProc 
      finProc
    ) {
      declarar (
        @decorate=evaluar @saludar(@integrantes) finEval
      ) {
        evaluar @decorate("-Bienvenidos") finEval
      }
    }

Resultado esperado: "Hola:Juan-David-Jean-Bienvenidos"

===================================================================================
|#

(interpretador)





