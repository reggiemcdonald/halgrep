## HalGrep
### A full-text search utility written in Haskell

### To run: 
```bash
stack build
stack run
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

**halgrep** -- Full-text search utility

SYNOPSIS
========

| **halgrep** \[**-hl**] \[**-c**\[Num]] \[**--context-lines**\[=Num]] [pattern] [file ...]


DESCRIPTION
===========

Performs a search for the specified pattern in the given files. In general, a match is one where a regular expression is found in the input text. 

Options
-------

-h, --help

:   Prints brief usage information.


-c[num, --context-lines=num]

:   Print num lines preceding and following each matched line.

--fuzzy=num

:   TBD.

Exit Status
-----------

The halgrep utility exits with one of the following values:
| Exit Code | Meaning |
| :-------: | :-----: |
| 0 | Matches found. |
| 1 | No matches found. |
| Otherwise | Error. |

*Markdown manual page template from https://gist.github.com/eddieantonio/55752dd76a003fefb562*