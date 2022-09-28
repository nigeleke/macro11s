# Macro-11

## Introduction

This project is a folly[^1]. This is a [Macro-11 Assembler](https://en.wikipedia.org/wiki/MACRO-11) written in [Scala](https://scala-lang.org/).  Macro-11 was one of my first programming languages whereas Scala is one of my current languages of choice.

[^1]: a whimsical or extravagant structure built to serve as a conversation piece, lend interest to a view, commemorate a person or event, etc.: found especially in England in the 18th century ([dictionary.com](https://www.dictionary.com/browse/folly))

The project will build an input file for the [PDP-11 SimH](https://github.com/simh/simh/tree/master/PDP11) simulator which I have driving my [PiDP-11](https://obsolescence.wixsite.com/obsolescence/pidp-11).

## Build

```commandline
> sbt clean coverage test coverageReport doc assemble`
```

## Run

```commandline
> macro11 [options] sourceFile*
```

## Acknowledgements

Much of the commentary text is taken directly from:

1. The [MACRO-11 Language Reference Manual](http://www.dmv.net/dec/pdf/macro.pdf)
2. The [PDP-11 Processor Handbook](http://pages.cpsc.ucalgary.ca/~dsb/PDP11/)
