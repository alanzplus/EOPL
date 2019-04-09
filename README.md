# Essentials of Programming Lanugage

A repository contains the notes and exericises of __Friedman__'s great book __Essentials of Programing Lanugage__ 3rd Edition.

The code is implemented using [DrRacket](https://download.racket-lang.org/) with eopl packages. [Reference](https://docs.racket-lang.org/eopl/index.html?q=sllgen#%28part._top%29)

Each implementation is associated with a test using Racket Unit Testing Framework.  

## Helper Scripts

### Run Tests

Exmpale,

```bash
cd ch3
../run-test.sh
```

### Craete Interpreter From Template

Example,

```bash
cd ch5
../create-from ../interpreter A cp
```

## Interpreter

Folder `interpreter` contains the minial template spec and implementation for different interpter.

`xx-sepc-repl.rkt` contains the AST REPL for each specification.

Example of running `racket -r A-spec-repl.rkt`

```bash
ƛ if let x = 5 in -(x,3) then zero?(0) else -(3,4)
#(struct:a-program
  #(struct:if-exp
    #(struct:let-exp
      x
      #(struct:const-exp 5)
      #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 3)))
    #(struct:zero?-exp #(struct:const-exp 0))
    #(struct:diff-exp #(struct:const-exp 3) #(struct:const-exp 4))))
#<void>
```

### A Interpreter

#### Basic Features

* Environment

#### Support Syntax and Expressioni

Const Expression

```bash
identifier
```

Const Number Expression

```bash
number
```

Diff Expression

```bash
-(expression1, expression2)
```

Zero Expression

```bash
zero?(expression)
```

If Expression

```bash
if expression then expression else expression
```

Let Expression

```bash
let identifier = expression in expression
```

LetRec Expression

```bash
letrec identifier(identifier) = expression in expression
```

Procedure Definition Expression

```bash
proc(identifier) expression
```

Procedure Call Expression

```bash
(expression expression)
```

Begin Expression

```bash
begin
  exp1;
  exp2;
  ...
end
```

## Chapter 1 Inductive Sets Of Data

### Examples

* bintree.rkt
* bst.rkt
* list-length.rkt
* list-sum.rkt
* nth-element.rkt
* number-elements.rkt
* occurs-free.rkt
* remove-first.rkt
* remove.rkt
* subst.rkt
* vector-sum.rkt

### Exercises
* 1.13.rkt
* 1.15.rkt
* 1.16.rkt
* 1.17.rkt
* 1.18.rkt
* 1.19.rkt
* 1.20.rkt
* 1.21.rkt
* 1.22.rkt
* 1.23.rkt
* 1.24.rkt
* 1.25.rkt
* 1.26.rkt
* 1.27.rkt
* 1.28.rkt
* 1.29.rkt
* 1.30.rkt
* 1.31.rkt
* 1.32.rkt
* 1.33.rkt
* 1.34.rkt
* 1.35.rkt
* 1.36.rkt

## Chapter 2 Data Abstraction

### TODO
* implement other examples (lc-exp parser)

### Examples
* env-data-structure.rkt
* env-procedural.rkt

### Exercises
* 2.1.rkt
* 2.3.rkt
* 2.5.rkt
* 2.11.rkt
* 2.12.rkt
* 2.13.rkt
* 2.14.rkt
* 2.15.rkt
* 2.16.rkt
* 2.17.rkt
* 2.18.rkt
* 2.19.rkt
* 2.21.rkt
* 2.22.rkt
* 2.23.rkt
* 2.24.rkt
* 2.25.rkt
* 2.26.rkt

## Chapter 3 Expression

### TODO

* 3.2
* 3.3
* 3.4
* 3.5
* 3.11
* 3.12
* 3.13
* 3.14
* 3.15
* 3.16
* 3.17
* 3.18
* 3.22 - 3.29
* 3.30

### Examples

* let-interpreter.rkt

### Exercises

* 3.6.rkt
* 3.7.rkt
* 3.8.rkt
* 3.9.rkt
* 3.10.rkt
* 3.19.rkt
* 3.20.rkt
* 3.31.rkt
* 3.32.rkt
* 3.33.rkt
* 3.34.rkt

## Chapter 4 State

### TODO

* 4.9
* 4.12
* 4.13
* 4.19
* 4.20
* 4.21
* 4.22
* 4.23
* 4.24
* 4.25
* 4.26
* 4.27
* 4.29
* 4.30
* 4.32
* 4.33
* 4.34
* 4.35
* 4.36
* 4.37
* 4.40
* 4.42

### Examples

* explicit-refs-interpreter.rkt
* implicit-refs-interpreter.rkt
* mutable-pairs-interpreter.rkt

### Exercises

* 4.10.rkt
* 4.11.rkt

## Chapter 5 Continuation-Passing Interpreters

### Examples

* cp-interpreter.rkt

### TODO
* Fix letrec grammar definition
* 5.10.rkt
* 5.14.rkt
* 5.15.rkt
* 5.16.rkt
* 5.35 - 5.36
* 5.39 - 5.44
* 5.47 - 5.58

### Exercises

* 5.1.rkt
* 5.2.rkt
* 5.3.rkt
* 5.4.rkt
* 5.5.rkt
* 5.6.rkt
* 5.7.rkt
* 5.8.rkt
* 5.9.rkt
* 5.11.rkt
* 5.12.rkt
* 5.13.rkt
* 5.37.rkt
* 5.38.rkt
* 5.45.rkt
* 5.46.rkt

## TODO For Interpreter

* Add `begin` to `A` interpreter
* Move Explicit Reference Implementation to new template
* Move implementations of interpreter in chapter 3 to new template

## Chapter 6 Continuation-Passing Style

### TODO
* 6.5 - 6.10
* 6.15 - 6.19

### Exercises
* 6.2
* 6.3
* 6.11
* 6.12
* 6.13
* 6.14
