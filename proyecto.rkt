#lang eopl
 
;******************************************************************************************
;;Interpretador para LogicFusion: lenguaje funcional e imperativo con soporte
;;para lógica booleana (instancias SAT en forma normal conjuntiva)
 
;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= GLOBALS { {<declaration>}* } PROGRAM { proc main ( ) <body> }
;;                      <a-program (decls body)>

;;  <body>          ::= { <expression> ; {<expression> ;}* }
;;                      <a-body (exp exps)>

;;  <declaration>   ::= <type-exp> <identifier> = <expression> ;
;;                      <var-decl (texp id exp)>

;;                  ::= const <type-exp> <identifier> = <expression> ;
;;                      <const-decl (texp id exp)>

;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>

;;                  ::= <string>
;;                      <text-exp (txt)>
;;                  ::= True                              <true-exp ()>
;;                  ::= False                             <false-exp ()>

;;                  ::= <identifier> {<ident-tail>}*
;;                      <ident-exp (id tails)>
;;                      (sin tails es una variable; con tails es una llamada a procedimiento)

;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>

;;                  ::= ( <expression> <bin-primitive> <expression> )
;;                       <bin-primapp-exp (rand1 prim rand2)>

;;                  ::= set <identifier> -> <expression>
;;                      <set-exp (id rhs)>

;;                  ::= if <expression> <body> else <body>
;;                      <if-exp (test true-body false-body)>

;;                  ::= switch <expression> { {case <expression> : <body>}* default : <body> }
;;                      <switch-exp (exp vals bodies default)>

;;                  ::= while <expression> <body>
;;                      <while-exp (test body)>

;;                  ::= for ( <expression> ; <expression> ; <expression> ) <body>
;;                      <for-exp (init test update body)>

;;                  ::= BLOCK <body>
;;                      <block-exp (body)>

;;                  ::= LOCALS { {<declaration>}* } <body>
;;                      <locals-exp (decls body)>

;;                  ::= return <expression>
;;                      <return-exp (exp)>

;;                  ::= lambda ({<type-exp> <identifier>}*(,)) <body>
;;                      <proc-exp (arg-texps ids body)>

;;                  ::= list ( {<expression>}*(,) )
;;                      <list-exp (exps)>

;;                  ::= empty                             <empty-list-exp ()>

;;                  ::= [ {<expression>}*(,) ]
;;                      <vector-exp (exps)>

;;                  ::= { {<expression> : <expression>}*(,) }
;;                      <dict-exp (keys vals)>

;;                  ::= FNC <number> ( <sat-clause> {and <sat-clause>}* )
;;                      <sat-exp (nvars clause clauses)>

;;  <ident-tail>    ::= ( {<expression>}*(,) )
;;                      <call-tail (rands)>

;;  <sat-clause>    ::= ( <number> {or <number>}* )
;;                      <a-sat-clause (lit lits)>

;;  <type-exp>      ::= int | float | string | bool | void | sat | proc
;;                  ::= list < <type-exp> > | vector < <type-exp> >
;;                  ::= dict < <type-exp> , <type-exp> >

;;  <bin-primitive> ::= + | - | * | / | == | != | > | < | >= | <=

;;  <primitive>     ::= print | longitud | concatenar | solve
;;                  ::= empty? | make-list | list? | head | tail | append | ref-list | length
;;                  ::= vector? | make-vector | ref-vector | set-vector | append-vector | delete-val-vector | insert-val-vector
;;                  ::= create-dict | dict? | ref-dict | set-dict | append-dict | keys-dict | values-dict
 
;******************************************************************************************************************************
;******************************************************************************************
;Especificación Léxica
 
(define scanner-spec-logicfusion
'((white-sp (whitespace) skip)
  (comment ("%" (arbno (not #\newline))) skip)
  (identifier (letter (arbno (or letter digit "?" "_"))) symbol)
  (string ("\"" (arbno (not #\")) "\"") string)
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)
  (number (digit (arbno digit) "." digit (arbno digit)) number)
  (number ("-" digit (arbno digit) "." digit (arbno digit)) number)))


;Especificación Sintáctica (gramática)
 
(define grammar-logicfusion
  '((program ("GLOBALS" "{" (arbno declaration) "}"
              "PROGRAM" "{" "proc" "main" "(" ")" body "}") a-program)
 
  ; cuerpo: una o más expresiones, cada una terminada en ";"
    (body ("{" expression ";" (arbno expression ";") "}") a-body)
 
  ; declaraciones (variables y constantes)
    (declaration (type-exp identifier "=" expression ";") var-decl)
    (declaration ("const" type-exp identifier "=" expression ";") const-decl)
 
  ; expresiones básicas
    (expression (number) lit-exp)
    (expression (string) text-exp)
    (expression ("True") true-exp)
    (expression ("False") false-exp)
    (expression (identifier (arbno ident-tail)) ident-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    (expression ("(" expression bin-primitive expression ")") bin-primapp-exp)
 
  ; procedimientos
    (expression ("lambda" "(" (separated-list type-exp identifier ",") ")" body) proc-exp)
    (ident-tail ("(" (separated-list expression ",") ")") call-tail)
    (expression ("return" expression) return-exp)

  ; asignación
    (expression ("set" identifier "->" expression) set-exp)
 
  ; secuenciación
    (expression ("BLOCK" body) block-exp)
    (expression ("LOCALS" "{" (arbno declaration) "}" body) locals-exp)
 
  ; estructuras de control
    (expression ("if" expression body "else" body) if-exp)
    (expression ("switch" expression "{" (arbno "case" expression ":" body) "default" ":" body "}") switch-exp)
    (expression ("while" expression body) while-exp)
    (expression ("for" "(" expression ";" expression ";" expression ")" body) for-exp)
 
  ; estructuras de datos
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression ("empty") empty-list-exp)
    (expression ("[" (separated-list expression ",") "]") vector-exp)
    (expression ("{" (separated-list expression ":" expression ",") "}") dict-exp)
 
  ; instancias SAT
    (expression ("FNC" number "(" sat-clause (arbno "and" sat-clause) ")") sat-exp)
    (sat-clause ("(" number (arbno "or" number) ")") a-sat-clause)
 
  ; tipos de datos primitivos
    (type-exp ("int") int-type-exp)
    (type-exp ("float") float-type-exp)
    (type-exp ("string") string-type-exp)
    (type-exp ("bool") bool-type-exp)
    (type-exp ("void") void-type-exp)
    (type-exp ("sat") sat-type-exp)

  ;funciones
    (type-exp ("proc") proc-type-exp)
 
  ; tipos de datos compuestos
    (type-exp ("list" "<" type-exp ">") list-type-exp)
    (type-exp ("vector" "<" type-exp ">") vector-type-exp)
    (type-exp ("dict" "<" type-exp "," type-exp ">") dict-type-exp)
 
  ; primitivas binarias (infijas)
    (bin-primitive ("+") add-prim)
    (bin-primitive ("-") substract-prim)
    (bin-primitive ("*") mult-prim)
    (bin-primitive ("/") div-prim)
    (bin-primitive ("==") equal-prim)
    (bin-primitive ("!=") diff-prim)
    (bin-primitive (">") greater-prim)
    (bin-primitive ("<") less-prim)
    (bin-primitive (">=") greater-eq-prim)
    (bin-primitive ("<=") less-eq-prim)
 
  ; primitivas generales
    (primitive ("print") print-prim)
    (primitive ("solve") solve-prim)
 
  ; primitivas de cadenas
    (primitive ("longitud") longitud-prim)
    (primitive ("concatenar") concatenar-prim)
 
    ; primitivas de listas
    (primitive ("empty?") empty?-prim)
    (primitive ("make-list") make-list-prim)
    (primitive ("list?") list?-prim)
    (primitive ("head") head-prim)
    (primitive ("tail") tail-prim)
    (primitive ("append") append-prim)
    (primitive ("ref-list") ref-list-prim)
    (primitive ("length") length-prim)
 
    ; primitivas de vectores
    (primitive ("vector?") vector?-prim)
    (primitive ("make-vector") make-vector-prim)
    (primitive ("ref-vector") ref-vector-prim)
    (primitive ("set-vector") set-vector-prim)
    (primitive ("append-vector") append-vector-prim)
    (primitive ("delete-val-vector") delete-val-vector-prim)
    (primitive ("insert-val-vector") insert-val-vector-prim)
 
    ; primitivas de diccionarios
    (primitive ("create-dict") create-dict-prim)
    (primitive ("dict?") dict?-prim)
    (primitive ("ref-dict") ref-dict-prim)
    (primitive ("set-dict") set-dict-prim)
    (primitive ("append-dict") append-dict-prim)
    (primitive ("keys-dict") keys-dict-prim)
    (primitive ("values-dict") values-dict-prim)))


;Tipos de datos para la sintaxis abstracta de la gramática
 
;Construidos automáticamente:
 
(sllgen:make-define-datatypes scanner-spec-logicfusion grammar-logicfusion)
 
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-logicfusion grammar-logicfusion)))
 
;*******************************************************************************************
;Parser, Scanner, Interfaz
 
;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
 
(define scan&parse
  (sllgen:make-string-parser scanner-spec-logicfusion grammar-logicfusion))
 
;El Analizador Léxico (Scanner)
 
(define just-scan
  (sllgen:make-string-scanner scanner-spec-logicfusion grammar-logicfusion))
 
;El Interpretador (FrontEnd + Evaluación + señal para lectura )
 
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser
      scanner-spec-logicfusion
      grammar-logicfusion)))
 
;*******************************************************************************************
;El Interprete
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (decls main-body)
                 (let ((env (init-env decls)))
                   (begin
                     (eval-declarations! decls env)
                     (eval-body main-body env)))))))

; Ambiente inicial
;init-env: <list-of declaration> -> environment
; contiene TODAS las variables de GLOBALS con su tipo y bandera const, pero todavía
; sin valor (se les da valor en eval-declarations!). Es la misma idea de
; extend-env-recursively de clase: el ambiente existe antes de evaluar los valores,
; de modo que los procedimientos globales puedan verse a sí mismos (recursión).
(define init-env
  (lambda (decls)
    (extend-env
     (map decl-id decls)
     (map (lambda (decl) 'no-inicializada) decls)
     (map decl->var-type decls)
     (empty-env))))

;decl-id: <declaration> -> symbol
(define decl-id
  (lambda (decl)
    (cases declaration decl
      (var-decl (texp id rhs-exp) id)
      (const-decl (texp id rhs-exp) id))))

;decl->var-type: <declaration> -> <var-type>
; tipo declarado y si es constante (const) o variable
(define decl->var-type
  (lambda (decl)
    (cases declaration decl
      (var-decl (texp id rhs-exp) (a-var-type texp #f))
      (const-decl (texp id rhs-exp) (a-var-type texp #t)))))

;eval-declarations!: <list-of declaration> <environment> -> void
; evalúa las declaraciones en orden y guarda cada valor en el ambiente
(define eval-declarations!
  (lambda (decls env)
    (for-each (lambda (decl) (eval-declaration! decl env)) decls)))

(define eval-declaration!
  (lambda (decl env)
    (cases declaration decl
      (var-decl (texp id rhs-exp) (init-var! texp id rhs-exp env))
      (const-decl (texp id rhs-exp) (init-var! texp id rhs-exp env)))))

;init-var!: <type-exp> <symbol> <expression> <environment> -> void
; evalúa el valor inicial, valida su tipo (validación al crear) y lo guarda
(define init-var!
  (lambda (texp id rhs-exp env)
    (let ((val (eval-expression rhs-exp env)))
      (begin
        (stamp-type! texp val)
        (check-declaration id texp val)
        (setref! (apply-env-ref env id) val)))))

;assign-var!: <symbol> <valor> <environment> -> void
; modifica una variable existente (set x -> expr): valida que no sea const y que el
; valor nuevo tenga el tipo declarado (validación al modificar) y luego lo guarda.
; Igual que el set-exp de clase (setref! sobre apply-env-ref), con la validación antes.
(define assign-var!
  (lambda (id val env)
    (let ((info (apply-env-type env id)))
      (begin
        (check-assignment id info val)
        (cases var-type info
          (a-var-type (texp const?) (stamp-type! texp val)))
        (setref! (apply-env-ref env id) val)))))

;stamp-type!: <type-exp> <valor> -> void
; un vector o diccionario recién creado no conoce el tipo de sus elementos;
; al declararlo con vector<T> o dict<K,V> se le anota ese tipo para validar
; luego append-vector, set-vector, set-dict, etc.
(define stamp-type!
  (lambda (texp val)
    (cases type-exp texp
      (vector-type-exp (elem-texp)
                       (cases vector-val val
                         (a-vector (state)
                                   (let ((elements (vector-ref state 1)))
                                     (begin
                                       (vector-set! state 0 elem-texp)
                                       (if (and (not (null? elements))
                                                (eqv? (car elements) 'sin-inicializar))
                                           (let ((default
                                                  (cases type-exp elem-texp
                                                    (int-type-exp () 0)
                                                    (float-type-exp () 0.0)
                                                    (string-type-exp () "")
                                                    (bool-type-exp () #f)
                                                    (void-type-exp () 'void)
                                                    (else 'sin-inicializar))))
                                             (vector-set!
                                              state 1
                                              (make-list-of-length default
                                                                   (length elements))))
                                           #t))))))
      (dict-type-exp (key-texp val-texp)
                     (cases dict-val val
                       (a-dict (state)
                               (begin
                                 (vector-set! state 0 key-texp)
                                 (vector-set! state 1 val-texp)))))
      (else #t))))

;eval-body: <body> <environment> -> valor
; evalúa las expresiones del cuerpo en orden y retorna el valor de la última
; (es el begin-exp de clase)
(define eval-body
  (lambda (bdy env)
    (cases body bdy
      (a-body (exp exps)
              (let loop ((acc (eval-expression exp env))
                         (exps exps))
                (if (null? exps)
                    acc
                    (loop (eval-expression (car exps) env)
                          (cdr exps))))))))

;eval-expression: <expression> <enviroment> -> valor
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (text-exp (txt) (substring txt 1 (- (string-length txt) 1)))
      (true-exp () #t)
      (false-exp () #f)
      (ident-exp (id tails)
                 (eval-tails (apply-env env id) tails env))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (bin-primapp-exp (rand1 prim rand2)
                       (let ((arg1 (eval-rand rand1 env))
                             (arg2 (eval-rand rand2 env)))
                         (apply-bin-primitive prim arg1 arg2)))
      (block-exp (bdy) (eval-body bdy env))
      (return-exp (val-exp) (eval-expression val-exp env))
      (set-exp (id rhs-exp)
               (begin
                 (assign-var! id (eval-expression rhs-exp env) env) 1))
      (proc-exp (texps ids bdy) (closure ids texps bdy env))
      (if-exp (test-exp true-bdy false-bdy)
              (if (true-value? (eval-expression test-exp env))
                  (eval-body true-bdy env)
                  (eval-body false-bdy env)))
      (locals-exp (decls bdy)
                  (let ((env-local
                         (extend-env
                          (map decl-id decls)
                          (map (lambda (decl) 'no-inicializada) decls)
                          (map decl->var-type decls)
                          env)))
                    (begin
                      (eval-declarations! decls env-local)
                      (eval-body bdy env-local))))
      (while-exp (test-bdy bdy)
                 (let loop ()
                   (if (true-value? (eval-expression test-bdy env))
                       (begin
                         (eval-body bdy env)
                         (loop))
                       1)))
      (for-exp (init-exp test-exp update-exp bdy)
               (begin
                 (eval-expression init-exp env)
                 (let loop ()
                   (if (true-value? (eval-expression test-exp env))
                       (begin
                         (eval-body bdy env)
                         (eval-expression update-exp env)
                         (loop))
                       1))))
      ; -- listas y vectores --
      (list-exp (exprs) (eval-rands exprs env))
      (empty-list-exp () '())
      (vector-exp (exprs)
                  (let ((elements (eval-rands exprs env)))
                    (a-vector (vector 'void elements))))
      ; -- control y diccionarios
      (dict-exp (keys vals)
                (a-dict (vector (void-type-exp)
                                (void-type-exp)
                                (map cons (eval-rands keys env) (eval-rands vals env)))))
      (switch-exp (exp casos cuerpos-casos cuerpo-default)
                  (eval-switch (eval-expression exp env)
                               casos cuerpos-casos cuerpo-default env))
      ; -- instancias SAT
      (sat-exp (num-vars clausula clausulas)
               (crear-instancia-sat num-vars
                                    (map clausula->lista (cons clausula clausulas))))


      (else (eopl:error 'eval-expression
                        "Esta expresión aún no está implementada: ~s" exp)))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))


;eval-tails: <valor> <list-of ident-tail> <environment> -> valor
; una variable sin tails es solo su valor; con tails es una llamada (o varias
; encadenadas, f(1)(2)): se evalúan los argumentos y se aplica el procedimiento
(define eval-tails
  (lambda (val tails env)
    (if (null? tails)
        val
        (cases ident-tail (car tails)
          (call-tail (rands)
                     (eval-tails (apply-call val (eval-rands rands env))
                                 (cdr tails)
                                 env))))))

;apply-call: <valor> <list-of-valores> -> valor
; verifica que lo que se invoca sea un procedimiento (igual que app-exp de clase)
(define apply-call
  (lambda (proc args)
    (if (procval? proc)
        (apply-procedure proc args)
        (eopl:error 'apply-call
                    "Se intento invocar algo que no es un procedimiento: ~s" proc))))


;apply-procedure: evalúa el cuerpo de un procedimiento en un ambiente extendido
; con los parámetros y sus argumentos. El paso de parámetros es por valor: cada
; parámetro es una variable nueva con una copia del valor del argumento.
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids texps cuerpo env)
               (if (= (length ids) (length args))
                   (eval-body cuerpo
                              (extend-env ids
                                          args
                                          (map (lambda (texp) (a-var-type texp #f)) texps)
                                          env))
                   (eopl:error 'apply-procedure
                               "Numero incorrecto de argumentos: se esperaban ~s y se recibieron ~s"
                               (length ids) (length args)))))))

;apply-primitive: <primitiva> <list-of-valores> -> valor
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (print-prim () (begin
                       (for-each (lambda (x) (display (show-value x))) args)
                       (newline)
                       1))
      ; -- listas y vectores
      (list?-prim () (list? (car args)))
      (empty?-prim () (null? (car args)))
      (head-prim () (let ((lst (car args)))
                      (if (null? lst)
                          (eopl:error 'head "No se puede sacar head de una lista vacia")
                          (car lst))))
      (tail-prim () (let ((lst (car args)))
                      (if (null? lst)
                          (eopl:error 'tail "No se puede sacar tail de una lista vacia")
                          (cdr lst))))
      (length-prim () (length (car args)))
      (ref-list-prim () (let ((lst (car args))
                              (pos (cadr args)))
                          (if (and (>= pos 0) (< pos (length lst)))
                              (list-ref lst pos)
                              (eopl:error 'ref-list "La posicion ~s esta fuera de rango" pos))))
      (make-list-prim () (cons (car args) (cadr args)))
      (append-prim () (append (car args) (cadr args)))

      (vector?-prim () (vector-val? (car args)))
      (make-vector-prim ()
                        (let ((n (car args)))
                          (if (and (integer? n) (exact? n) (>= n 0))
                              (a-vector
                               (vector 'void
                                       (make-list-of-length 'sin-inicializar n)))
                              (eopl:error 'make-vector
                                          "El tamaño debe ser un entero no negativo: ~s" n))))
      (ref-vector-prim ()
                       (let ((vec (car args))
                             (pos (cadr args)))
                         (if (vector-val? vec)
                             (let ((elements (vector-val-elements vec)))
                               (if (and (integer? pos) (exact? pos)
                                        (>= pos 0)
                                        (< pos (length elements)))
                                   (list-ref elements pos)
                                   (eopl:error 'ref-vector
                                               "La posicion ~s esta fuera de rango" pos)))
                             (eopl:error 'ref-vector
                                         "El primer argumento debe ser un vector"))))
      (set-vector-prim ()
                       (let ((vec (car args))
                             (pos (cadr args))
                             (val (caddr args)))
                         (if (vector-val? vec)
                             (cases vector-val vec
                               (a-vector (state)
                                         (let ((elements (vector-ref state 1))
                                               (elem-texp (vector-ref state 0)))
                                           (if (and (integer? pos) (exact? pos)
                                                    (>= pos 0)
                                                    (< pos (length elements)))
                                               (if (or (eqv? elem-texp 'void)
                                                       (type-matches? elem-texp val))
                                                   (begin
                                                     (vector-set!
                                                      state 1
                                                      (replace-list-position elements pos val))
                                                     val)
                                                   (eopl:error 'set-vector
                                                               "El valor ~s no corresponde al tipo del vector" val))
                                               (eopl:error 'set-vector
                                                           "La posicion ~s esta fuera de rango" pos)))))
                             (eopl:error 'set-vector
                                         "El primer argumento debe ser un vector"))))
      (append-vector-prim ()
                          (let ((vec (car args))
                                (val (cadr args)))
                            (if (vector-val? vec)
                                (cases vector-val vec
                                  (a-vector (state)
                                            (let ((elements (vector-ref state 1))
                                                  (elem-texp (vector-ref state 0)))
                                              (if (or (eqv? elem-texp 'void)
                                                      (type-matches? elem-texp val))
                                                  (begin
                                                    (vector-set! state 1
                                                                 (append elements (list val)))
                                                    vec)
                                                  (eopl:error 'append-vector
                                                              "El valor ~s no corresponde al tipo del vector" val)))))
                                (eopl:error 'append-vector
                                            "El primer argumento debe ser un vector"))))
      (delete-val-vector-prim ()
                              (let ((vec (car args))
                                    (pos (cadr args)))
                                (if (vector-val? vec)
                                    (cases vector-val vec
                                      (a-vector (state)
                                                (let ((elements (vector-ref state 1)))
                                                  (if (and (integer? pos) (exact? pos)
                                                           (>= pos 0)
                                                           (< pos (length elements)))
                                                      (begin
                                                        (vector-set!
                                                         state 1
                                                         (delete-list-position elements pos))
                                                        vec)
                                                      (eopl:error 'delete-val-vector
                                                                  "La posicion ~s esta fuera de rango" pos)))))
                                    (eopl:error 'delete-val-vector
                                                "El primer argumento debe ser un vector"))))
      (insert-val-vector-prim ()
                              (let ((vec (car args))
                                    (pos (cadr args))
                                    (val (caddr args)))
                                (if (vector-val? vec)
                                    (cases vector-val vec
                                      (a-vector (state)
                                                (let ((elements (vector-ref state 1))
                                                      (elem-texp (vector-ref state 0)))
                                                  (if (and (integer? pos) (exact? pos)
                                                           (>= pos 0)
                                                           (<= pos (length elements)))
                                                      (if (or (eqv? elem-texp 'void)
                                                              (type-matches? elem-texp val))
                                                          (begin
                                                            (vector-set!
                                                             state 1
                                                             (insert-list-position elements pos val))
                                                            vec)
                                                          (eopl:error 'insert-val-vector
                                                                      "El valor ~s no corresponde al tipo del vector" val))
                                                      (eopl:error 'insert-val-vector
                                                                  "La posicion ~s esta fuera de rango" pos)))))
                                    (eopl:error 'insert-val-vector
                                                "El primer argumento debe ser un vector"))))
      ; -- cadenas y diccionarios
      (longitud-prim ()
                     (let ((txt (car args)))
                       (if (string? txt)
                           (string-length txt)
                           (eopl:error 'longitud
                                       "longitud requiere una cadena, recibió ~s" txt))))
      (concatenar-prim ()
                       (let ((txt1 (car args))
                             (txt2 (cadr args)))
                         (if (and (string? txt1) (string? txt2))
                             (string-append txt1 txt2)
                             (eopl:error 'concatenar
                                         "concatenar requiere dos cadenas"))))
      (create-dict-prim () (a-dict (vector (void-type-exp) (void-type-exp) '())))
      (dict?-prim () (dict-val? (car args)))
      (ref-dict-prim () (let ((par (buscar-par (dict-val-pairs (car args)) (cadr args))))
                          (if par
                              (cdr par)
                              (eopl:error 'ref-dict "La clave ~s no existe" (cadr args)))))
      (keys-dict-prim () (map car (dict-val-pairs (car args))))
      (values-dict-prim () (map cdr (dict-val-pairs (car args))))
      (set-dict-prim () (let ((dic (car args)) (clave (cadr args)) (val (caddr args)))
                          (begin
                            (revisar-tipos-dict dic clave val)
                            (if (buscar-par (dict-val-pairs dic) clave)
                                (cambiar-pares! dic (reemplazar-valor (dict-val-pairs dic) clave val))
                                (cambiar-pares! dic (append (dict-val-pairs dic) (list (cons clave val)))))
                            1)))
      (append-dict-prim () (let ((dic (car args)) (clave (cadr args)) (val (caddr args)))
                             (if (buscar-par (dict-val-pairs dic) clave)
                                 (eopl:error 'append-dict "La clave ~s ya existe" clave)
                                 (begin
                                   (revisar-tipos-dict dic clave val)
                                   (cambiar-pares! dic (append (dict-val-pairs dic) (list (cons clave val))))
                                   1))))            
      ; -- instancias SAT
      (solve-prim () (resolver-sat (car args)))
                                               
      (else (eopl:error 'apply-primitive
                        "La primitiva ~s aun no esta implementada" prim)))))

;show-value: <valor> -> <valor>
; para imprimir True y False en vez de #t y #f, tambien dentro de listas
(define show-value
  (lambda (v)
    (cond
      ((eqv? v #t) "True")
      ((eqv? v #f) "False")
      ((list? v) (map show-value v))
      (else v))))

;apply-bin-primitive: <bin-primitive> <valor> <valor> -> valor
; operaciones aritméticas y relacionales (se asume que los operandos son correctos,
; como dijo el profesor: no hay chequeo de tipos en estas operaciones)
(define apply-bin-primitive
  (lambda (prim arg1 arg2)
    (cases bin-primitive prim
      (add-prim () (+ arg1 arg2))
      (substract-prim () (- arg1 arg2))
      (mult-prim () (* arg1 arg2))
      (div-prim () (let ((r (/ arg1 arg2)))
                     (if (integer? r)
                         r
                         (exact->inexact r))))
      (equal-prim () (equal-values? arg1 arg2))
      (diff-prim () (not (equal-values? arg1 arg2)))
      (greater-prim () (> arg1 arg2))
      (less-prim () (< arg1 arg2))
      (greater-eq-prim () (>= arg1 arg2))
      (less-eq-prim () (<= arg1 arg2)))))

;equal-values?: <valor> <valor> -> boolean
; los números se comparan por valor (=), los demás con equal?
(define equal-values?
  (lambda (a b)
    (if (and (number? a) (number? b))
        (= a b)
        (equal? a b))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
; (el profesor confirmó que las condiciones aceptan booleanos y números: 0 es falso)
(define true-value?
  (lambda (x)
    (if (boolean? x)
        x
        (not (zero? x)))))
 
;*******************************************************************************************
;Procedimientos
; (igual a procval de clase, pero guarda también los tipos de los parámetros
;  para poder validar luego las asignaciones a esos parámetros)
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (texps (list-of type-exp?))
   (cuerpo body?)
   (env environment?)))

;*******************************************************************************************
;Valores expresados (CONTRATO PARA TODO EL GRUPO)
;
;enteros y flotantes : números de Scheme (int = exacto, float = inexacto, ej. 20.0)
;cadenas             : strings de Scheme
;booleanos           : #t y #f  (True y False del lenguaje)
;listas              : listas de Scheme (inmutables). empty es '()
;procedimientos      : closure (procval)
;vectores            : a-vector, mutable y de tamaño variable
;diccionarios        : a-dict, mutable
;instancias SAT      : a-sat
 
;a-vector guarda un "estado" (vector de Scheme de 2 posiciones) para poder cambiar su contenido y su tamaño:
; posición 0: tipo de los elementos (type-exp), void mientras no se declare
; posición 1: lista de Scheme con los elementos
(define-datatype vector-val vector-val?
  (a-vector (state vector?)))
 
; a-dict guarda un estado de 3 posiciones:
; posición 0: tipo de las claves (type-exp)
; posición 1: tipo de los valores (type-exp)
; posición 2: lista de pares (clave . valor)
(define-datatype dict-val dict-val?
  (a-dict (state vector?)))
 
; a-sat: número de variables y lista de cláusulas;
; cada cláusula es una lista de enteros (literales), ej. (1 -2 3 4)
(define-datatype sat-val sat-val?
  (a-sat (nvars integer?)
         (clauses list?)))
 
; vector-val-elements: <vector-val> -> list
(define vector-val-elements
  (lambda (v)
    (cases vector-val v
      (a-vector (state) (vector-ref state 1)))))
 
; dict-val-pairs: <dict-val> -> list
(define dict-val-pairs
  (lambda (d)
    (cases dict-val d
      (a-dict (state) (vector-ref state 2)))))
 
;*******************************************************************************************
;Validación de tipos
; (solo se usa al crear una variable y al modificarla)
 
;type-matches?: <type-exp> <valor> -> boolean
;función que determina si un valor corresponde a un tipo declarado
(define type-matches?
  (lambda (texp val)
    (cases type-exp texp
      (int-type-exp () (and (integer? val) (exact? val)))
      (float-type-exp () (and (real? val) (inexact? val)))
      (string-type-exp () (string? val))
      (bool-type-exp () (boolean? val))
      (void-type-exp () #t)
      (sat-type-exp () (sat-val? val))
      (proc-type-exp () (procval? val))
      (list-type-exp (elem-texp)
                     (and (list? val)
                          (all-match? elem-texp val)))
      (vector-type-exp (elem-texp)
                       (and (vector-val? val)
                            (all-match? elem-texp (vector-val-elements val))))
      (dict-type-exp (key-texp val-texp)
                     (and (dict-val? val)
                          (all-pairs-match? key-texp val-texp (dict-val-pairs val)))))))
 
;all-match?: <type-exp> <list> -> boolean
(define all-match?
  (lambda (texp lst)
    (cond
      ((null? lst) #t)
      ((type-matches? texp (car lst)) (all-match? texp (cdr lst)))
      (else #f))))
 
;all-pairs-match?: <type-exp> <type-exp> <list-of pares> -> boolean
(define all-pairs-match?
  (lambda (key-texp val-texp pairs)
    (cond
      ((null? pairs) #t)
      ((and (type-matches? key-texp (car (car pairs)))
            (type-matches? val-texp (cdr (car pairs))))
       (all-pairs-match? key-texp val-texp (cdr pairs)))
      (else #f))))
 
;type->string: <type-exp> -> string
;función que escribe un tipo para los mensajes de error
(define type->string
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () "int")
      (float-type-exp () "float")
      (string-type-exp () "string")
      (bool-type-exp () "bool")
      (void-type-exp () "void")
      (sat-type-exp () "sat")
      (proc-type-exp () "proc")
      (list-type-exp (elem-texp)
                     (string-append "list<" (type->string elem-texp) ">"))
      (vector-type-exp (elem-texp)
                       (string-append "vector<" (type->string elem-texp) ">"))
      (dict-type-exp (key-texp val-texp)
                     (string-append "dict<" (type->string key-texp) ","
                                    (type->string val-texp) ">")))))


;check-declaration: <symbol> <type-exp> <valor> -> #t o error
;validación al momento de crear una variable o constante
(define check-declaration
  (lambda (id texp val)
    (if (type-matches? texp val)
        #t
        (eopl:error 'check-declaration
                    "El valor ~s no corresponde al tipo ~s de la variable ~s"
                    val (type->string texp) id))))


;check-assignment: <symbol> <var-type> <valor> -> #t o error
;validación al momento de modificar una variable:
; las constantes no se pueden modificar y el valor nuevo debe tener el tipo declarado
(define check-assignment
  (lambda (id info val)
    (cases var-type info
      (a-var-type (texp const?)
                  (cond
                    (const? (eopl:error 'check-assignment
                                        "No se puede modificar la constante ~s" id))
                    ((not (type-matches? texp val))
                     (eopl:error 'check-assignment
                                 "El valor ~s no corresponde al tipo ~s de la variable ~s"
                                 val (type->string texp) id))
                    (else #t))))))
 
;*******************************************************************************************

;Ambientes
 
;información de tipo que se guarda en el ambiente junto a cada variable:
; el tipo declarado y si es constante
(define-datatype var-type var-type?
  (a-var-type (texp type-exp?)
              (const? boolean?)))
 
;definición del tipo de dato ambiente
; (igual al de clase, con un vector adicional "types" paralelo a "vec")
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (types vector?)
   (env environment?)))
 
(define scheme-value? (lambda (v) #t))

;empty-env: -> enviroment
;función que crea un ambiente vacío
(define empty-env
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío
 
;extend-env: <list-of symbols> <list-of valores> <list-of var-type> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals types env)
    (extended-env-record syms (list->vector vals) (list->vector types) env)))
 
;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
 
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals types env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))
 
;apply-env-type: <enviroment> <symbol> -> <var-type>
;función que busca el tipo (y si es constante) de una variable en un ambiente
(define apply-env-type
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-type "No binding for ~s" sym))
      (extended-env-record (syms vals types env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (vector-ref types pos)
                                 (apply-env-type env sym)))))))
 
;*******************************************************************************************
;Referencias
 
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))
 
(define deref
  (lambda (ref)
    (primitive-deref ref)))
 
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))
 
(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))
 
(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))
 
;****************************************************************************************
;Funciones Auxiliares
 
; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente
 
(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))
 
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
 
; make-list-of-length: valor <numero> -> lista
(define make-list-of-length
  (lambda (val n)
    (if (= n 0)
        '()
        (cons val (make-list-of-length val (- n 1))))))

; replace-list-position: reemplaza un elemento 
(define replace-list-position
  (lambda (lst pos val)
    (if (= pos 0)
        (cons val (cdr lst))
        (cons (car lst)
              (replace-list-position (cdr lst) (- pos 1) val)))))

; delete-list-position: elimina un elemento y desplaza los siguientes
(define delete-list-position
  (lambda (lst pos)
    (if (= pos 0)
        (cdr lst)
        (cons (car lst)
              (delete-list-position (cdr lst) (- pos 1))))))

; insert-list-position: inserta un elemento y desplaza los siguientes
(define insert-list-position
  (lambda (lst pos val)
    (if (= pos 0)
        (cons val lst)
        (cons (car lst)
              (insert-list-position (cdr lst) (- pos 1) val)))))

;Auxiliares para diccionarios

;buscar-par: <list-of pares> <claves> -> par o #f
(define buscar-par
  (lambda (pares clave)
    (cond
      ((null? pares) #f)
      ((equal-values? (car (car pares)) clave) (car pares))
      (else (buscar-par (cdr pares) clave)))))

;reemplazar-valor: <list-of pares> <clave> <valor> -> list-of pares
;lista nueva con el valor de la clave cambiado
(define reemplazar-valor
  (lambda (pares clave val)
    (if (equal-values? (car (car pares)) clave)
        (cons (cons clave val) (cdr pares))
        (cons (car pares) (reemplazar-valor (cdr pares) clave val)))))

;cambiar-pares!: <dict-val> <list-of pares> -> void
;guarda la lista nueva en la posicion 2 de la caja
(define cambiar-pares!
  (lambda (dic nuevos)
    (cases dict-val dic
      (a-dict (state) (vector-set! state 2 nuevos)))))

;revisar-tipos-dict: <dict-val> <clave> <valor> -> #t / error
;revisa la clave con el tipo de la posicion 0 y el valor con el de la posicion 1
(define revisar-tipos-dict
  (lambda (dic clave val)
    (cases dict-val dic
      (a-dict (state)
              (cond
                ((not (type-matches? (vector-ref state 0) clave))
                 (eopl:error 'dict "La clave ~s no corresponde al tipo del diccionario" clave))
                ((not (type-matches? (vector-ref state 1) val))
                 (eopl:error 'dict "El valor ~s no corresponde al tipo del diccionario" val))
                (else #t))))))

;Auxiliares para switch

;eval-switch: <valor> <list-of expression> <list-of body> <body> <environment> -> valor
;evalua el cuerpo del primer case igual al valor; si ninguno coincide, el del default
(define eval-switch
  (lambda (valor casos cuerpos-casos cuerpo-default ambiente)
    (cond
      ((null? casos) (eval-body cuerpo-default ambiente))
      ((equal-values? valor (eval-expression (car casos) ambiente))
       (eval-body (car cuerpos-casos) ambiente))
      (else (eval-switch valor (cdr casos) (cdr cuerpos-casos) cuerpo-default ambiente)))))

;Auxiliares para instancias SAT

;clausula->lista: <sat-clause> -> <list-of int>
;une el primer literal con los demas: (a-sat-clause 1 (2)) -> (1 2)
(define clausula->lista
  (lambda (clausula)
    (cases sat-clause clausula
      (a-sat-clause (literal literales) (cons literal literales)))))

;crear-instancia-sat: <number> <list-of <list-of number>> -> sat-val
;valida que el numero de variables sea un entero positivo y los literales, y crea la instancia
(define crear-instancia-sat
  (lambda (num-vars clausulas)
    (if (not (and (integer? num-vars) (exact? num-vars) (> num-vars 0)))
        (eopl:error 'sat-exp "El numero de variables debe ser un entero positivo: ~s" num-vars)
        (begin
          (validar-clausulas num-vars clausulas)
          (a-sat num-vars clausulas)))))

;validar-clausulas: <int> <list-of <list-of number>> -> #t / error
(define validar-clausulas
  (lambda (num-vars clausulas)
    (if (null? clausulas)
        #t
        (begin
          (for-each (lambda (literal) (validar-literal num-vars literal)) (car clausulas))
          (validar-clausulas num-vars (cdr clausulas))))))

;validar-literal: <int> <number> -> #t / error
;un literal debe ser entero, distinto de 0 y con indice entre 1 y num-vars
(define validar-literal
  (lambda (num-vars literal)
    (cond
      ((not (and (integer? literal) (exact? literal)))
       (eopl:error 'sat-exp "Los literales deben ser enteros: ~s" literal))
      ((= literal 0)
       (eopl:error 'sat-exp "El literal 0 no es valido (las variables van de 1 a ~s)" num-vars))
      ((> (abs literal) num-vars)
       (eopl:error 'sat-exp "La variable ~s esta fuera de rango: debe estar entre 1 y ~s"
                   (abs literal) num-vars))
      (else #t))))

;resolver-sat: <valor> -> list
;primitiva solve: retorna (satisfactible (asignacion)) o (insatisfactible ())
(define resolver-sat
  (lambda (instancia)
    (if (sat-val? instancia)
        (cases sat-val instancia
          (a-sat (num-vars clausulas)
                 (let ((asignacion (buscar-asignacion num-vars clausulas '())))
                   (if asignacion
                       (list 'satisfactible asignacion)
                       (list 'insatisfactible '())))))
        (eopl:error 'solve "solve debe recibir una instancia SAT y recibio: ~s" instancia))))

;buscar-asignacion: <int> <list-of <list-of int>> <list-of bool> -> <list-of bool> o #f
;busqueda exhaustiva: prueba cada variable con True y luego con False, y se detiene
;en la primera asignacion completa que cumple la formula (#f si no hay ninguna)
(define buscar-asignacion
  (lambda (faltan clausulas asignacion)
    (if (= faltan 0)
        (if (formula-verdadera? clausulas asignacion)
            asignacion
            #f)
        (let ((con-true (buscar-asignacion (- faltan 1) clausulas
                                           (append asignacion (list #t)))))
          (if con-true
              con-true
              (buscar-asignacion (- faltan 1) clausulas
                                 (append asignacion (list #f))))))))

;formula-verdadera?: <list-of <list-of int>> <list-of bool> -> bool
;la formula es verdadera si todas sus clausulas lo son (and)
(define formula-verdadera?
  (lambda (clausulas asignacion)
    (cond
      ((null? clausulas) #t)
      ((clausula-verdadera? (car clausulas) asignacion)
       (formula-verdadera? (cdr clausulas) asignacion))
      (else #f))))

;clausula-verdadera?: <list-of int> <list-of bool> -> bool
;la clausula es verdadera si alguno de sus literales lo es (or)
(define clausula-verdadera?
  (lambda (literales asignacion)
    (cond
      ((null? literales) #f)
      ((literal-verdadero? (car literales) asignacion) #t)
      (else (clausula-verdadera? (cdr literales) asignacion)))))

;literal-verdadero?: <int> <list-of bool> -> bool
;el literal k vale lo que la variable k (posicion k-1) y el literal -k su negacion
(define literal-verdadero?
  (lambda (literal asignacion)
    (let ((valor (list-ref asignacion (- (abs literal) 1))))
      (if (> literal 0)
          valor
          (not valor)))))
        
               

;******************************************************************************************


;******************************************************************************************
;Pruebas (scan&parse): un ejemplo por cada tipo de producción

; Declaraciones de variables, constantes y todos los tipos, lambda, llamada y main
(scan&parse "GLOBALS {
  int a = 10;
  float pi = 3.1415;
  string nombre = \"Robinson\";
  bool docente? = True;
  void sinTipo = 5;
  list<int> nums1 = list(1, 2, 3);
  list<float> nums2 = list(1.5, 2.3, 3.8);
  vector<int> nums3 = [1, 2, 3];
  vector<float> nums4 = [1.5, 2.3, 3.8];
  const int valor = 10;
  const float valor2 = 20.0;
  dict<string, int> edades = {\"Juan\" : 25, \"Ana\" : 30};
  dict<string, void> datos = {\"Nombre\" : \"Juan\", \"Edad\" : 40, \"Casado\" : True};
  proc square = lambda (int x) { return (x * x); };
} % End of Globals
PROGRAM {
  proc main() { return square(a); }
}")

; BLOCK, print y llamada a procedimiento
(scan&parse "GLOBALS {
  int a = 10;
  proc square = lambda (int x) {
    return BLOCK {
      print(\"Funcion square\");
      (x * x);
    };
  };
}
PROGRAM {
  proc main() {
    return BLOCK {
      print(\"Bienvenidos a LogicFusion\");
      square(a);
    };
  }
}")

; LOCALS, asignación (set ... ->), while y switch
(scan&parse "GLOBALS {
  float base = 10.5;
  int max_num = 5;
  proc triangle = lambda (float base, float height) {
    return LOCALS {
      float area = 0.0;
      float b = base;
      float h = height;
    } {
      print(\"Esta es la primera instruccion del cuerpo\");
      set area -> ((base * height) / 2);
      area;
    };
  };
}
PROGRAM {
  proc main() {
    return LOCALS {
      int count = 1;
      int fact = 0;
    } {
      while (count <= max_num) {
        print(\"Cuenta: \", count);
        set count -> (count + 1);
      };
      switch count {
        case 1 : { \"uno\"; }
        case 6 : { \"seis\"; }
        default : { \"otro\"; }
      };
      triangle(base, 6.4);
    };
  }
}")

; if / else y recursión (factorial)
(scan&parse "GLOBALS {
  proc factorial = lambda (int n) {
    if (n <= 1) { return 1; } else { return (n * factorial((n - 1))); };
  };
}
PROGRAM {
  proc main() { return factorial(5); }
}")

; for
(scan&parse "GLOBALS {
  proc factorial = lambda (int num) {
    return LOCALS {
      int result = 1;
      int i = 1;
    } {
      for (set i -> 1; (i <= num); set i -> (i + 1)) {
        set result -> (result * i);
      };
      result;
    };
  };
}
PROGRAM {
  proc main() { return factorial(5); }
}")

; primitivas de listas, vectores, diccionarios y cadenas
(scan&parse "GLOBALS {
  list<int> l = make-list(1, empty);
  vector<int> v = make-vector(3);
  dict<string, int> d = create-dict();
}
PROGRAM {
  proc main() {
    return BLOCK {
      head(l);
      tail(l);
      empty?(l);
      append(l, l);
      ref-list(l, 0);
      length(l);
      append-vector(v, 40);
      set-vector(v, 1, 50);
      ref-vector(v, 1);
      delete-val-vector(v, 1);
      insert-val-vector(v, 1, -5);
      set-dict(d, \"edad\", 34);
      ref-dict(d, \"edad\");
      keys-dict(d);
      values-dict(d);
      longitud(\"hola\");
      concatenar(\"ho\", \"la\");
    };
  }
}")

; primitivas de predicado (list?, vector?, dict?) y append-dict
(scan&parse "GLOBALS {
  list<int> l = list(1, 2);
  vector<int> v = [1, 2];
  dict<string, int> d = {\"a\" : 1};
}
PROGRAM {
  proc main() {
    return BLOCK {
      list?(l);
      vector?(v);
      dict?(d);
      append-dict(d, \"b\", 2);
    };
  }
}")

; instancias SAT: ejemplo 1 (satisfactible) y ejemplo 2 (insatisfactible) del enunciado
(scan&parse "GLOBALS {
  sat x = FNC 4 (
    (1 or -2 or 3 or 4) and
    (-2 or 3) and
    (-1 or -2 or -3) and
    (3 or 4) and
    (2)
  );
}
PROGRAM {
  proc main() { return solve(x); }
}")

(scan&parse "GLOBALS {
  sat x = FNC 2 (
    (1 or 2) and
    (-1) and
    (-2)
  );
}
PROGRAM {
  proc main() { return solve(x); }
}")

;Pruebas de evaluacion: switch e instancias SAT

; switch (ejemplo del enunciado)
(eval-program (scan&parse "GLOBALS { string color = \"amarillo\"; }
PROGRAM {
  proc main() {
    return switch color {
      case \"rojo\" : { \"Detente\"; }
      case \"amarillo\" : { \"Precaucion\"; }
      case \"verde\" : { \"Sigue\"; }
      default : { \"Color desconocido\"; }
    };
  }
}"))   ; "Precaucion"
(eval-program (scan&parse "GLOBALS { string color = \"azul\"; } PROGRAM { proc main() { return switch color { case \"rojo\" : { \"Detente\"; } default : { \"Color desconocido\"; } }; } }"))   ; "Color desconocido"
(eval-program (scan&parse "GLOBALS { float x = 2.0; } PROGRAM { proc main() { return switch x { case 1 : { \"uno\"; } case 2 : { \"dos\"; } default : { \"otro\"; } }; } }"))   ; "dos"
(eval-program (scan&parse "GLOBALS { int n = 1; } PROGRAM { proc main() { return switch n { case 1 : { \"primero\"; } case 1 : { \"segundo\"; } default : { \"otro\"; } }; } }"))   ; "primero"

; instancias SAT
(eval-program (scan&parse "GLOBALS { sat x = FNC 2 ((1 or 2) and (-1) and (-2)); } PROGRAM { proc main() { return x; } }"))   ; #(struct:a-sat 2 ((1 2) (-1) (-2)))
; errores (quitar el ; para probarlos):
;(eval-program (scan&parse "GLOBALS { sat x = FNC 0 ((1)); } PROGRAM { proc main() { return x; } }"))   ; El numero de variables debe ser un entero positivo: 0
;(eval-program (scan&parse "GLOBALS { sat x = FNC 2.5 ((1)); } PROGRAM { proc main() { return x; } }"))   ; El numero de variables debe ser un entero positivo: 2.5
;(eval-program (scan&parse "GLOBALS { sat x = FNC 2 ((1 or 0)); } PROGRAM { proc main() { return x; } }"))   ; El literal 0 no es valido (las variables van de 1 a 2)
;(eval-program (scan&parse "GLOBALS { sat x = FNC 2 ((1 or -3)); } PROGRAM { proc main() { return x; } }"))   ; La variable 3 esta fuera de rango: debe estar entre 1 y 2
;(eval-program (scan&parse "GLOBALS { sat x = FNC 2 ((1.5)); } PROGRAM { proc main() { return x; } }"))   ; Los literales deben ser enteros: 1.5
;(eval-program (scan&parse "GLOBALS { sat x = 5; } PROGRAM { proc main() { return x; } }"))   ; El valor 5 no corresponde al tipo "sat" de la variable x

; solve: ejemplo 1 del enunciado
(eval-program (scan&parse "GLOBALS {
  sat x = FNC 4 (
    (1 or -2 or 3 or 4) and
    (-2 or 3) and
    (-1 or -2 or -3) and
    (3 or 4) and
    (2)
  );
}
PROGRAM {
  proc main() { return solve(x); }
}"))   ; (satisfactible (#f #t #t #t))
; solve: ejemplo 2 del enunciado
(eval-program (scan&parse "GLOBALS {
  sat x = FNC 2 (
    (1 or 2) and
    (-1) and
    (-2)
  );
}
PROGRAM {
  proc main() { return solve(x); }
}"))   ; (insatisfactible ())
(eval-program (scan&parse "GLOBALS { } PROGRAM { proc main() { return solve(FNC 1 ((-1))); } }"))   ; (satisfactible (#f))
(eval-program (scan&parse "GLOBALS { proc resolver = lambda (sat f) { return solve(f); }; } PROGRAM { proc main() { return resolver(FNC 3 ((1 or 2) and (-1) and (-2 or 3))); } }"))   ; (satisfactible (#f #t #t))
(eval-program (scan&parse "GLOBALS { sat y = FNC 3 ((1) and (-2)); } PROGRAM { proc main() { return solve(y); } }"))   ; (satisfactible (#t #f #t))
;(eval-program (scan&parse "GLOBALS { } PROGRAM { proc main() { return solve(5); } }"))   ; solve debe recibir una instancia SAT y recibio: 5

; print muestra True y False dentro de listas
(eval-program (scan&parse "GLOBALS { sat x = FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and (-1 or -2 or -3) and (3 or 4) and (2)); } PROGRAM { proc main() { return print(solve(x)); } }"))   ; imprime (satisfactible (False True True True))
(eval-program (scan&parse "GLOBALS { list<bool> l = list(True, False); } PROGRAM { proc main() { return print(l); } }"))   ; imprime (True False)

;(interpretador)