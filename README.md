## HalGrep
### A full-text search utility written in Haskell

### To run: 
```bash
stack build
stack run
```

### To install the binary:
```bash
stack install
halgrep --help
```

### To run interactive:
```bash
stack ghci
```

### To run unit tests
```bash
stack test
```

### Man Page:
HALGREP(1) Version 0.1 | CPSC312 Project 1 | HALGREP(1)

NAME
====

**halgrep** -- a text search utility

SYNOPSIS
========

| **halgrep** \[**-c** | **--context-lines** NUM] [**-f** | **--fuzzy** LEVEL] [**-r** | **--recursive**] [**-n** | **--line-numbers**] PATTERN FILES ...


DESCRIPTION
===========

Performs a search for the specified pattern in the given files. 

Available Options
-------

-c, --context-lines NUM

: Print NUM lines of context preceding and following each matched line.

-f, --fuzzy LEVEL

: Desired level of fuzziness: NONE, LOW, MED, HIGH.

-r, --recursive

: Recursively search subdirectories.

-n, --line-numbers

: Print the line numbers of each match.

PATTERN

: Pattern to search for.

FILES

: Files to search.

*Markdown manual page template from https://gist.github.com/eddieantonio/55752dd76a003fefb562*