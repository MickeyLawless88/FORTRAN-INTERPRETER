FORTRAN IV INTERACTIVE INTERPRETER v2.1
========================================
Compatible with Microsoft FORTRAN Version 3.31  
Optimized for DOS Memory Constraints (640KB)  
By: Mickey W. Lawless (C) 2025, 2026

## FEATURES:
---------
* Interactive command-line interpreter
* Supports basic FORTRAN IV statements
* Variable declarations (INTEGER, REAL, LOGICAL)
* 1, 2, and 3-dimensional arrays
* DIMENSION statements for array declarations
* Array subscript expressions A(I), B(I,J), C(I,J,K)
* Arithmetic expressions with proper precedence
* Assignment statements (scalars and arrays)
* PRINT statements for output
* Program storage (up to 200 lines)
* Symbol table (up to 50 variables/arrays)
* Array pool (up to 1000 elements total)
* Numbered programs execute only on RUN command
* LOAD command for loading programs from files

## BUILDING:
---------
Run BUILD.BAT to compile all modules:

```
  C:\> BUILD.BAT
```

This will create INTERP.EXE using the FL compiler

## SYSTEM REQUIREMENTS:
--------------------
* DOS 3.0 or higher
* Microsoft FORTRAN Version 3.31
* LINK.EXE (Microsoft linker)
* 256KB RAM minimum (640KB recommended)

## RUNNING:
--------
Start the interpreter:

```
  C:\> INTERP.EXE
```

You will see the prompt:

```
  FORTRAN> 
```

## COMMANDS:
---------
```
HELP       - Display help information
CLEAR      - Clear program memory
LIST       - List current program
RUN        - Execute current program
LOAD "FILE" - Load program from file
EXIT       - Exit interpreter
```

## FORTRAN STATEMENTS:
------------------
* Variable declarations:
  ```
  INTEGER A, B, C
  REAL X, Y, Z
  ```
  
* Array declarations:
  ```
  DIMENSION A(10)
  DIMENSION B(5,4)
  DIMENSION C(3,3,2)
  ```
  
* Assignment statements:
  ```
  A = 10
  B = A + 5
  X = (A * B) / 2
  ```
  
* Array assignments:
  ```
  A(5) = 100
  B(2,3) = A(5) + 10
  C(1,2,1) = B(2,3) * 2
  ```
  
* Print statements:
  ```
  PRINT A
  PRINT A(5), B(2,3)
  PRINT A(I) + B(J,K)
  ```
  
* Program lines (numbered):
  ```
  10 DIMENSION A(10)
  20 A(5) = 100
  30 PRINT A(5)
  ```

## EXAMPLES:
---------
1. Simple calculation:
   ```
   FORTRAN> A = 10
   FORTRAN> B = 20  
   FORTRAN> PRINT A + B
    30
   ```

2. Basic program:
   ```
   FORTRAN> 10 INTEGER A, B, C
   FORTRAN> 20 A = 10
   FORTRAN> 30 B = 20
   FORTRAN> 40 C = A + B
   FORTRAN> 50 PRINT C
   FORTRAN> RUN
   ```

3. Array example:
   ```
   FORTRAN> DIMENSION A(5)
   FORTRAN> A(1) = 10
   FORTRAN> A(2) = 20
   FORTRAN> PRINT A(1), A(2)
    10   20
   ```

4. Array program:
   ```
   FORTRAN> 10 DIMENSION B(3,3)
   FORTRAN> 20 B(1,1) = 100
   FORTRAN> 30 B(2,2) = 200
   FORTRAN> 40 PRINT B(1,1) + B(2,2)
   FORTRAN> RUN
   ```

5. Loading a program from file:
   ```
   FORTRAN> LOAD "MYPROG.FOR"
   OK
   FORTRAN> LIST
   FORTRAN> RUN
   ```

## MEMORY USAGE:
-------------
* Program storage: ~14.4KB (200 lines * 72 chars)
* Symbol table: ~4KB (50 vars/arrays * 80 bytes)
* Array pool: ~4KB (1000 integers)
* Code size: ~30KB estimated
* Total: ~53KB (fits well within DOS memory)

## LIMITATIONS:
------------
* Integer arithmetic only (for simplicity)
* No floating point operations
* No string handling
* No file I/O (except LOAD command)
* No subroutines/functions
* Maximum 200 program lines
* Maximum 50 variables/arrays
* Maximum 1000 array elements total
* Arrays limited to 3 dimensions
* No dynamic array sizing
* Filenames limited to 64 characters

## DOS COMPATIBILITY:
------------------
* Uses Microsoft FORTRAN 3.31 compiler (FOR1 command)
* Standard DOS LINK.EXE for linking
* Fits in base 640KB memory

## FILES:
------
```
INTERP.FOR   - Main interpreter program
LEXER.FOR    - Lexical analyzer (tokenizer)
SYMBOLS.FOR  - Symbol table management
PARSER.FOR   - Expression parser and evaluator
PROGRAM.FOR  - Program storage and execution
STMTS.FOR    - Statement handlers
BUILD.BAT    - Build script
README.TXT   - This file
TEST.FOR     - Basic test program
ARRTEST.FOR  - Array test program
```

## TECHNICAL NOTES:
----------------
The interpreter is designed to work within DOS memory constraints.  
Uses Microsoft FORTRAN 3.31 compiler (FOR1 command).  
Code follows FORTRAN IV standards with some FORTRAN 77 extensions.  
LOAD command supports standard DOS file operations.
