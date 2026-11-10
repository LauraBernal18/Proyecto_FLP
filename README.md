# LogicFusion

Proyecto final del curso **Fundamentos de Interpretación y Compilación de Lenguajes de Programación**
(Universidad del Valle). Profesor: Robinson Duque, Ph.D.

LogicFusion es un lenguaje que combina los paradigmas funcional e imperativo, con soporte para
instancias del problema de satisfacibilidad booleana (SAT) en forma normal conjuntiva (FNC).
El intérprete está escrito en Racket (`#lang eopl`) con SLLGEN, usando solo las librerías EOPL y Stream.

## Integrantes

| Integrante | Responsabilidad |
|---|---|
| (nombre 1) | Base: léxico, gramática, ambientes con tipos, variables, procedimientos, BLOCK, LOCALS |
| (nombre 2) | Control (if, switch, while, for), cadenas y diccionarios |
| (nombre 3) | Listas y vectores |
| (nombre 4) | Instancias SAT (`solve`), pruebas integradas y documentación |

## Estructura

- `LogicFusion.rkt`: intérprete (un solo archivo, dividido en secciones comentadas).
- `README.md`: este archivo.

## Cómo ejecutarlo

1. Abrir `LogicFusion.rkt` en DrRacket (lenguaje `The Racket Language`, primera línea `#lang eopl`).
2. Presionar *Run*. Las pruebas al final del archivo muestran ejemplos de `scan&parse` para cada producción.
3. Para el REPL, descomentar `(interpretador)` al final del archivo.

## Sintaxis adoptada (ajustes leves al enunciado)

- Toda expresión dentro de `{ ... }` termina en `;` (incluidos los cuerpos de `if`, `while`, `switch`).
- Toda declaración termina en `;` (incluidas las de `proc`).
- Las operaciones binarias van entre paréntesis: `(x * x)`, `(n <= 1)`.
- Los números negativos se escriben pegados: `-5`. Para restar: `(a - b)`.
- Dentro de `if` y `while` la condición es una expresión: `if (n <= 1) { ... } else { ... }`.
- Diccionarios: `create-dict`, `dict?`, `ref-dict`, `set-dict`, `append-dict`, `keys-dict`, `values-dict`.
- Al cerrar un tipo genérico antes de `=` se deja un espacio: `vector<int> v = [1, 2];`.

## Flujo de trabajo

- Cada integrante trabaja en su propia rama (`base`, `control`, `listas-vectores`, `sat`) y entrega por *pull request*.
- Un commit por funcionalidad, con mensajes descriptivos.
