#lang eopl

;******************************************************************************************
;; PROGRAMA: Simulación de Clases Forma, Rectangulo y Circulo
;; Usando el interpretador con asignación de variables (interpretador_asign.scm)
;******************************************************************************************

;; Este programa implementa tres "clases" usando solo procedimientos y estado:
;; - Forma: clase base con Ubicacion_x y Ubicacion_y
;; - Rectangulo: hereda de Forma, añade lado1 y lado2
;; - Circulo: hereda de Forma, añade radio
;;
;; Sistema de mensajes:
;; Cada objeto es una función que recibe una señal (número) para despachar métodos:
;; - setX, setY, getX, getY (mensajes 1-4 para Forma)
;; - setBase, setAltura, getBase, getAltura (mensajes adicionales para Rectangulo)
;; - setRadio, getRadio (mensajes adicionales para Circulo)

;; ============================================================================
;; CLASE FORMA (Base)
;; ============================================================================
;; Atributos: Ubicacion_x, Ubicacion_y
;; Métodos: initialize(x, y), getUbicacionX(), setUbicacionX(x), 
;;          getUbicacionY(), setUbicacionY(y)

(define programa-forma
  "let
    Forma = proc(x_inicial y_inicial)
              let
                Ubicacion_x = x_inicial
                Ubicacion_y = y_inicial
              in
                proc(mensaje)
                  if -(mensaje, 1)
                  then
                    if -(mensaje, 2)
                    then
                      if -(mensaje, 3)
                      then
                        proc(nuevoValor)
                          begin
                            set Ubicacion_y = nuevoValor;
                            Ubicacion_y
                          end
                      else
                        Ubicacion_y
                    else
                      proc(nuevoValor)
                        begin
                          set Ubicacion_x = nuevoValor;
                          Ubicacion_x
                        end
                  else
                    Ubicacion_x
  in
    let
      obj1 = (Forma 10 20)
    in
      begin
        ((obj1 1));          % getX -> 10
        ((obj1 3));          % getY -> 20
        ((obj1 2) 15);       % setX(15)
        ((obj1 1))           % getX -> 15
      end
  ")

;; ============================================================================
;; CLASE RECTANGULO (Hereda de Forma CON DELEGACIÓN)
;; ============================================================================
;; Atributos heredados: Ubicacion_x, Ubicacion_y (a través de super)
;; Atributos propios: lado1, lado2
;; Métodos: todos los de Forma + getBase(), setBase(), getAltura(), setAltura()

(define programa-rectangulo
  "let
    Forma = proc(x_inicial y_inicial)
              let
                Ubicacion_x = x_inicial
                Ubicacion_y = y_inicial
              in
                proc(mensaje)
                  if -(mensaje, 1)
                  then
                    if -(mensaje, 2)
                    then
                      if -(mensaje, 3)
                      then
                        proc(nuevoValor)
                          begin
                            set Ubicacion_y = nuevoValor;
                            Ubicacion_y
                          end
                      else
                        Ubicacion_y
                    else
                      proc(nuevoValor)
                        begin
                          set Ubicacion_x = nuevoValor;
                          Ubicacion_x
                        end
                  else
                    Ubicacion_x
    Rectangulo = proc(base altura x_inicial y_inicial)
                   let
                     super = (Forma x_inicial y_inicial)  % HERENCIA: crear objeto padre
                     lado1 = base
                     lado2 = altura
                   in
                     proc(mensaje)
                       if -(mensaje, 5)
                       then
                         if -(mensaje, 6)
                         then
                           if -(mensaje, 7)
                           then
                             if -(mensaje, 8)
                             then
                               lado2  % Mensaje 7: getAltura
                             else
                               proc(nuevoValor)
                                 begin
                                   set lado2 = nuevoValor;
                                   lado2
                                 end  % Mensaje 8: setAltura
                           else
                             lado1  % Mensaje 6: getBase
                         else
                           proc(nuevoValor)
                             begin
                               set lado1 = nuevoValor;
                               lado1
                             end  % Mensaje 6: setBase (error en índice, corrección)
                       else
                         (super mensaje)  % mensajes al padre
  in
    let
      rect = (Rectangulo 20 30 9 6)
    in
      begin
        ((rect 5));          % getBase -> 20
        ((rect 7));          % getAltura -> 30
        ((rect 1));          % getX -> 9 (delegado a super)
        ((rect 3))           % getY -> 6 (delegado a super)
      end
  ")

;; ============================================================================
;; CLASE CIRCULO (Hereda de Forma CON DELEGACIÓN)
;; ============================================================================
;; Atributos heredados: Ubicacion_x, Ubicacion_y (a través de super)
;; Atributos propios: radio
;; Métodos: todos los de Forma + getRadio(), setRadio()

(define programa-circulo
  "let
    Forma = proc(x_inicial y_inicial)
              let
                Ubicacion_x = x_inicial
                Ubicacion_y = y_inicial
              in
                proc(mensaje)
                  if -(mensaje, 1)
                  then
                    if -(mensaje, 2)
                    then
                      if -(mensaje, 3)
                      then
                        proc(nuevoValor)
                          begin
                            set Ubicacion_y = nuevoValor;
                            Ubicacion_y
                          end
                      else
                        Ubicacion_y
                    else
                      proc(nuevoValor)
                        begin
                          set Ubicacion_x = nuevoValor;
                          Ubicacion_x
                        end
                  else
                    Ubicacion_x
    Circulo = proc(rad x_inicial y_inicial)
                let
                  super = (Forma x_inicial y_inicial)  % HERENCIA: crear objeto padre
                  radio = rad
                in
                  proc(mensaje)
                    if -(mensaje, 5)
                    then
                      if -(mensaje, 6)
                      then
                        radio  % Mensaje 5: getRadio
                      else
                        proc(nuevoValor)
                          begin
                            set radio = nuevoValor;
                            radio
                          end  % Mensaje 6: setRadio
                    else
                      (super mensaje)  % DELEGACIÓN: delegar mensajes 1-4 al padre
  in
    let
      circ = (Circulo 45 3 4)
    in
      begin
        ((circ 5));          % getRadio -> 45
        ((circ 1));          % getX -> 3 (delegado a super)
        ((circ 3))           % getY -> 4 (delegado a super)
      end
  ")

;; ============================================================================
;; PROGRAMA COMPLETO SIMPLIFICADO (CON HERENCIA)
;; ============================================================================
;; Solo crea objetos y obtiene sus valores sin modificarlos
;; Más fácil de entender y probar

(define programa-completo
  "let
    Forma = proc(x_inicial y_inicial)
              let
                Ubicacion_x = x_inicial
                Ubicacion_y = y_inicial
              in
                proc(mensaje)
                  if -(mensaje, 1)
                  then
                    if -(mensaje, 2)
                    then
                      if -(mensaje, 3)
                      then
                        proc(nuevoValor)
                          begin
                            set Ubicacion_y = nuevoValor;
                            Ubicacion_y
                          end
                      else
                        Ubicacion_y
                    else
                      proc(nuevoValor)
                        begin
                          set Ubicacion_x = nuevoValor;
                          Ubicacion_x
                        end
                  else
                    Ubicacion_x
    Circulo = proc(rad x_inicial y_inicial)
                let
                  super = (Forma x_inicial y_inicial)
                  radio = rad
                in
                  proc(mensaje)
                    if -(mensaje, 5)
                    then
                      if -(mensaje, 6)
                      then
                        radio
                      else
                        proc(nuevoValor)
                          begin
                            set radio = nuevoValor;
                            radio
                          end
                    else
                      (super mensaje)
    Rectangulo = proc(base altura x_inicial y_inicial)
                   let
                     super = (Forma x_inicial y_inicial)
                     lado1 = base
                     lado2 = altura
                   in
                     proc(mensaje)
                       if -(mensaje, 5)
                       then
                         if -(mensaje, 6)
                         then
                           if -(mensaje, 7)
                           then
                             if -(mensaje, 8)
                             then
                               lado2
                             else
                               proc(nuevoValor)
                                 begin
                                   set lado2 = nuevoValor;
                                   lado2
                                 end
                           else
                             lado1
                         else
                           proc(nuevoValor)
                             begin
                               set lado1 = nuevoValor;
                               lado1
                             end
                       else
                         (super mensaje)
  in
    let
      circulo = (Circulo 50 10 20)
      rectangulo = (Rectangulo 30 40 5 15)
    in
      +(((circulo 5)), ((rectangulo 5)))
  ")

;; ============================================================================
;; INSTRUCCIONES DE USO
;; ============================================================================
;; 
;; 1. Carga el interpretador_asign.scm primero
;; 2. En el REPL del interpretador, copia y pega el programa completo
;; 
;; PROGRAMA SIMPLIFICADO:
;; Crea un Circulo(radio=50, x=10, y=20) y un Rectangulo(base=30, altura=40, x=5, y=15)
;; Resultado: radio + base = 50 + 30 = 80
;;
;; PRUEBAS INDIVIDUALES:
;; - ((circulo 5))  → getRadio → 50
;; - ((circulo 1))  → getX → 10 (delegado a super)
;; - ((circulo 3))  → getY → 20 (delegado a super)
;; - ((rectangulo 5)) → getBase → 30
;; - ((rectangulo 7)) → getAltura → 40
;; - ((rectangulo 1)) → getX → 5 (delegado a super)
;; - ((rectangulo 3)) → getY → 15 (delegado a super)

(display "\n=== PROGRAMA DE FORMAS GEOMÉTRICAS ===\n")
(display "\nEste archivo contiene los programas para ejecutar en el interpretador_asign.scm\n")
(display "\nProgramas disponibles:\n")
(display "1. programa-forma: Prueba de la clase Forma\n")
(display "2. programa-circulo: Prueba de la clase Circulo CON HERENCIA\n")
(display "3. programa-rectangulo: Prueba de la clase Rectangulo CON HERENCIA\n")
(display "4. programa-completo: Programa final con ambos objetos CON HERENCIA\n")
(display "\nCopia y pega cualquiera de estos programas en el REPL del interpretador.\n\n")

(display "=== ¿CÓMO SE VE LA HERENCIA? ===\n\n")
(display "La herencia se implementa mediante DELEGACIÓN:\n\n")
(display "1. Las clases hijas (Circulo, Rectangulo) crean un objeto 'super':\n")
(display "   let super = (Forma x_inicial y_inicial) ...\n\n")
(display "2. Las clases hijas manejan SOLO sus propios métodos (radio, lado1, lado2)\n\n")
(display "3. Los métodos heredados se DELEGAN al objeto padre:\n")
(display "   else (super mensaje)  % Si no es mi método, se lo paso al padre\n\n")
(display "4. Esto evita DUPLICAR código y simula herencia real:\n")
(display "   - Circulo NO repite el código de getX/setX/getY/setY\n")
(display "   - Rectangulo NO repite el código de getX/setX/getY/setY\n")
(display "   - Ambos DELEGAN esos métodos a 'super' (el objeto Forma)\n\n")

(display "=== MAPEO DE MENSAJES ===\n\n")
(display "FORMA:\n")
(display "  Mensaje 1: getUbicacionX()\n")
(display "  Mensaje 2: setUbicacionX(valor)\n")
(display "  Mensaje 3: getUbicacionY()\n")
(display "  Mensaje 4: setUbicacionY(valor)\n\n")

(display "CIRCULO (hereda de Forma CON DELEGACIÓN):\n")
(display "  Mensajes 1-4: DELEGADOS a super (Forma)\n")
(display "  Mensaje 5: getRadio()\n")
(display "  Mensaje 6: setRadio(valor)\n\n")

(display "RECTANGULO (hereda de Forma CON DELEGACIÓN):\n")
(display "  Mensajes 1-4: DELEGADOS a super (Forma)\n")
(display "  Mensaje 5: getBase() / getLado1()\n")
(display "  Mensaje 6: setBase(valor) / setLado1(valor)\n")
(display "  Mensaje 7: getAltura() / getLado2()\n")
(display "  Mensaje 8: setAltura(valor) / setLado2(valor)\n\n")

(display "=== EJEMPLO DE USO ===\n\n")
(display "En el REPL del interpretador_asign.scm, ejecuta:\n\n")
(display programa-completo)
(display "\n\n")
