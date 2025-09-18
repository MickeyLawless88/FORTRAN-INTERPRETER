C     EXPRESSION PARSER AND EVALUATOR FOR FORTRAN IV INTERPRETER
C     OPTIMIZED FOR DOS MEMORY CONSTRAINTS
C     RECURSIVE DESCENT PARSER WITH OPERATOR PRECEDENCE
C
C     PARSE EXPRESSION (MAIN ENTRY POINT)
C
      SUBROUTINE PARSEEX(RESULT, ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
C
      CALL TERM(RESULT, ERROR)
      IF (ERROR) RETURN
C
      CALL ADDEXP(RESULT, ERROR)
      RETURN
      END
C
C     PARSE ADDITION/SUBTRACTION EXPRESSIONS
C
      SUBROUTINE ADDEXP(RESULT, ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
C
10    CONTINUE
      IF (TOKTYP .NE. 4) RETURN
      IF (TOKVAL .EQ. 1) THEN
C         ADDITION
          CALL GETTOK
          CALL TERM(TEMP, ERROR)
          IF (ERROR) RETURN
          RESULT = RESULT + TEMP
          GO TO 10
      ELSE IF (TOKVAL .EQ. 2) THEN
C         SUBTRACTION
          CALL GETTOK
          CALL TERM(TEMP, ERROR)
          IF (ERROR) RETURN
          RESULT = RESULT - TEMP
          GO TO 10
      END IF
      RETURN
      END
C
C     PARSE MULTIPLICATION/DIVISION TERMS
C
      SUBROUTINE TERM(RESULT, ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
C
      CALL FACTOR(RESULT, ERROR)
      IF (ERROR) RETURN
C
      CALL MULEXP(RESULT, ERROR)
      RETURN
      END
C
C     PARSE MULTIPLICATION/DIVISION EXPRESSIONS
C
      SUBROUTINE MULEXP(RESULT, ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
C
20    CONTINUE
      IF (TOKTYP .NE. 4) RETURN
      IF (TOKVAL .EQ. 3) THEN
C         MULTIPLICATION
          CALL GETTOK
          CALL FACTOR(TEMP, ERROR)
          IF (ERROR) RETURN
          RESULT = RESULT * TEMP
          GO TO 20
      ELSE IF (TOKVAL .EQ. 4) THEN
C         DIVISION
          CALL GETTOK
          CALL FACTOR(TEMP, ERROR)
          IF (ERROR) RETURN
          IF (TEMP .EQ. 0) THEN
              ERROR = .TRUE.
              RETURN
          END IF
          RESULT = RESULT / TEMP
          GO TO 20
      END IF
      RETURN
      END
C
C     PARSE FACTORS (NUMBERS, VARIABLES, PARENTHESES)
C
      SUBROUTINE FACTOR(RESULT, ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
      CHARACTER*72 LINE
      CHARACTER*8 VARNAM
C
      ERROR = .FALSE.
      RESULT = 0
C
      IF (TOKTYP .EQ. 3) THEN
C         NUMBER
          RESULT = TOKVAL
          CALL GETTOK
      ELSE IF (TOKTYP .EQ. 2) THEN
C         VARIABLE OR ARRAY
          CALL GETVRN(VARNAM)
          CALL GETTOK
C         CHECK FOR ARRAY SUBSCRIPT
          IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 1) THEN
C             ARRAY ELEMENT
              CALL GETTOK
              CALL PARSEEX(I1, ERROR)
              IF (ERROR) RETURN
              I2 = 0
              I3 = 0
C             CHECK FOR SECOND DIMENSION
              IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
                  CALL GETTOK
                  CALL PARSEEX(I2, ERROR)
                  IF (ERROR) RETURN
              END IF
C             CHECK FOR THIRD DIMENSION
              IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
                  CALL GETTOK
                  CALL PARSEEX(I3, ERROR)
                  IF (ERROR) RETURN
              END IF
C             EXPECT CLOSING PARENTHESIS
              IF (TOKTYP .NE. 5 .OR. TOKVAL .NE. 2) THEN
                  ERROR = .TRUE.
                  RETURN
              END IF
              CALL GETTOK
              CALL GETARR(VARNAM, I1, I2, I3, RESULT, ERROR)
              IF (ERROR) THEN
                  RESULT = 0
                  ERROR = .FALSE.
              END IF
          ELSE
C             SCALAR VARIABLE
              CALL GETVAR(VARNAM, RESULT, ERROR)
              IF (ERROR) THEN
C                 UNDEFINED VARIABLE - CREATE AS ZERO
                  CALL SETVAR(VARNAM, 0, ERROR)
                  RESULT = 0
                  ERROR = .FALSE.
              END IF
          END IF
      ELSE IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 1) THEN
C         LEFT PARENTHESIS
          CALL GETTOK
          CALL PARSEEX(RESULT, ERROR)
          IF (ERROR) RETURN
          IF (TOKTYP .NE. 5 .OR. TOKVAL .NE. 2) THEN
              ERROR = .TRUE.
              RETURN
          END IF
          CALL GETTOK
      ELSE IF (TOKTYP .EQ. 4 .AND. TOKVAL .EQ. 2) THEN
C         UNARY MINUS
          CALL GETTOK
          CALL FACTOR(RESULT, ERROR)
          IF (ERROR) RETURN
          RESULT = -RESULT
      ELSE
          ERROR = .TRUE.
      END IF
      RETURN
      END
C
C     GET VARIABLE NAME FROM CURRENT TOKEN
C
      SUBROUTINE GETVRN(VARNAM)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 VARNAM
      CHARACTER*72 LINE
      CHARACTER*1 CH
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
C
C     BACK UP TO GET VARIABLE NAME
      OLDPOS = POS
      POS = POS - 1
C     FIND START OF VARIABLE
30    CONTINUE
      IF (POS .LE. 1) GO TO 40
      CH = LINE(POS-1:POS-1)
      IF ((CH .GE. 'A' .AND. CH .LE. 'Z') .OR.
     1    (CH .GE. 'a' .AND. CH .LE. 'z') .OR.
     2    (CH .GE. '0' .AND. CH .LE. '9')) THEN
          POS = POS - 1
          GO TO 30
      END IF
C
40    CONTINUE
      VARNAM = '        '
      VLEN = 0
      DO 50 I = POS, OLDPOS - 1
          CH = LINE(I:I)
          IF (VLEN .LT. 8) THEN
              VLEN = VLEN + 1
              VARNAM(VLEN:VLEN) = CH
          END IF
50    CONTINUE
      POS = OLDPOS
      RETURN
      END
C
C     PARSE ASSIGNMENT STATEMENT
C
      SUBROUTINE ASSIGN(ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR, ISARRAY
      CHARACTER*8 VARNAM
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
C
      ERROR = .FALSE.
      ISARRAY = .FALSE.
C
C     GET VARIABLE NAME
      IF (TOKTYP .NE. 2) THEN
          ERROR = .TRUE.
          RETURN
      END IF
      CALL GETVRN(VARNAM)
      CALL GETTOK
C
C     CHECK FOR ARRAY SUBSCRIPT
      IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 1) THEN
          ISARRAY = .TRUE.
          CALL GETTOK
          CALL PARSEEX(I1, ERROR)
          IF (ERROR) RETURN
          I2 = 0
          I3 = 0
C         CHECK FOR SECOND DIMENSION
          IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
              CALL GETTOK
              CALL PARSEEX(I2, ERROR)
              IF (ERROR) RETURN
          END IF
C         CHECK FOR THIRD DIMENSION
          IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
              CALL GETTOK
              CALL PARSEEX(I3, ERROR)
              IF (ERROR) RETURN
          END IF
C         EXPECT CLOSING PARENTHESIS
          IF (TOKTYP .NE. 5 .OR. TOKVAL .NE. 2) THEN
              ERROR = .TRUE.
              RETURN
          END IF
          CALL GETTOK
      END IF
C
C     EXPECT EQUALS SIGN
      IF (TOKTYP .NE. 4 .OR. TOKVAL .NE. 5) THEN
          ERROR = .TRUE.
          RETURN
      END IF
      CALL GETTOK
C
C     PARSE RIGHT SIDE EXPRESSION
      CALL PARSEEX(VALUE, ERROR)
      IF (ERROR) RETURN
C
C     ASSIGN VALUE
      IF (ISARRAY) THEN
          CALL SETARR(VARNAM, I1, I2, I3, VALUE, ERROR)
      ELSE
          CALL SETVAR(VARNAM, VALUE, ERROR)
      END IF
      RETURN
      END