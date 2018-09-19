# Essentials of Programming Lanugage
A repository contains the notes and exericises of __Friedman__'s great book __Essentials of Programing Lanugage__ 3rd Edition.

The code is implemented using [DrRacket](https://download.racket-lang.org/) with eopl packages. [Reference](https://docs.racket-lang.org/eopl/index.html?q=sllgen#%28part._top%29)

Each implementation is associated with a test using Racket Unit Testing Framework.

## Parser
The `parser` folder constains the __scanner and parser__ specification of languages used in the book.

There also a `xxx-read-print-ast.rkt` for each language specification.

Example of running `racket -t let-read-print-ask.rkt`

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

### Examples
* let-interpreter.rkt

### Exercises
* 3.6.rkt
* 3.7.rkt
* 3.8.rkt

