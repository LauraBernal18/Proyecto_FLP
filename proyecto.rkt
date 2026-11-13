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
 
;eval-program: <programa> -> valor
; (PROVISIONAL: se reemplaza en el Paso 3; por ahora solo devuelve el árbol de sintaxis abstracta)
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
        (check-declaration id texp val)
        (stamp-type! texp val)
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
                         (a-vector (state) (vector-set! state 0 elem-texp))))
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
      (text-exp (txt) txt)
      (true-exp () #t)
      (false-exp () #f)
      (ident-exp (id tails)
                 (if (null? tails)
                     (apply-env env id)
                     (eopl:error 'eval-expression
                                 "Las llamadas a procedimientos se implementan en el Paso 5")))
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
                 (assign-var! id (eval-expression rhs-exp env) env)
                 1))
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

;apply-primitive: <primitiva> <list-of-valores> -> valor
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (print-prim () (begin
                       (for-each (lambda (x) (display (show-value x))) args)
                       (newline)
                       1))
      (else (eopl:error 'apply-primitive
                        "La primitiva ~s aun no esta implementada" prim)))))

;show-value: <valor> -> <valor>
; para imprimir True y False en vez de #t y #f
(define show-value
  (lambda (v)
    (cond
      ((eqv? v #t) "True")
      ((eqv? v #f) "False")
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
      (div-prim () (/ arg1 arg2))
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
;Procedimientos
; (igual a procval de clase, pero guarda también los tipos de los parámetros
;  para poder validar luego las asignaciones a esos parámetros)

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

;empty-env:      -> enviroment
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
 
;******************************************************************************************