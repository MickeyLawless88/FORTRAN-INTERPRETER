C     LEXICAL ANALYZER FOR FORTRAN IV INTERPRETER
C     OPTIMIZED FOR DOS MEMORY CONSTRAINTS
C
C     TOKEN TYPES
C     1=KEYWORD, 2=IDENT, 3=NUMBER, 4=OPERATOR, 5=DELIM, 6=EOF
C
      SUBROUTINE INITPS
      IMPLICIT INTEGER (A-Z)
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
      CHARACTER*72 LINE
C
      POS = 1
      LINLEN = 0
      TOKTYP = 6
      TOKVAL = 0
      RETURN
      END
C
C     SET INPUT LINE
C
      SUBROUTINE SETLIN(INPLIN)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 INPLIN, LINE
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
C
      LINE = INPLIN
      LINLEN = 72
      POS = 1
C     FIND ACTUAL LINE LENGTH
      DO 10 I = 72, 1, -1
          IF (LINE(I:I) .NE. ' ') THEN
              LINLEN = I
              GO TO 20
          END IF
10    CONTINUE
      LINLEN = 1
20    CONTINUE
      RETURN
      END
C
C     GET NEXT TOKEN
C
      SUBROUTINE GETTOK
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINE
      CHARACTER*1 CH
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
C
C     SKIP WHITESPACE
10    CONTINUE
      IF (POS .GT. LINLEN) THEN
          TOKTYP = 6
          RETURN
      END IF
      CH = LINE(POS:POS)
      IF (CH .EQ. ' ' .OR. CH .EQ. CHAR(9)) THEN
          POS = POS + 1
          GO TO 10
      END IF
C
C     CHECK FOR NUMBERS
      IF (CH .GE. '0' .AND. CH .LE. '9') THEN
          CALL GETNUM
          RETURN
      END IF
C
C     CHECK FOR LETTERS (KEYWORDS OR IDENTIFIERS)
      IF ((CH .GE. 'A' .AND. CH .LE. 'Z') .OR.
     1    (CH .GE. 'a' .AND. CH .LE. 'z')) THEN
          CALL GETID
          RETURN
      END IF
C
C     CHECK FOR STRING LITERALS
      IF (CH .EQ. '"') THEN
          CALL GETSTR
          RETURN
      END IF
C
C     CHECK FOR OPERATORS AND DELIMITERS
      CALL GETOP
      RETURN
      END
C
C     GET NUMBER TOKEN
C
      SUBROUTINE GETNUM
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINE
      CHARACTER*1 CH
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
C
      TOKTYP = 3
      TOKVAL = 0
      START = POS
C
20    CONTINUE
      IF (POS .GT. LINLEN) GO TO 30
      CH = LINE(POS:POS)
      IF (CH .GE. '0' .AND. CH .LE. '9') THEN
          POS = POS + 1
          GO TO 20
      END IF
C
30    CONTINUE
C     CONVERT TO INTEGER (SIMPLE CASE)
      DO 40 I = START, POS - 1
          CH = LINE(I:I)
          DIGIT = ICHAR(CH) - ICHAR('0')
          TOKVAL = TOKVAL * 10 + DIGIT
40    CONTINUE
      RETURN
      END
C
C     GET IDENTIFIER OR KEYWORD
C
      SUBROUTINE GETID
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINE
      CHARACTER*8 WORD
      CHARACTER*1 CH
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
C
      START = POS
      WLEN = 0
C
50    CONTINUE
      IF (POS .GT. LINLEN) GO TO 60
      CH = LINE(POS:POS)
      IF ((CH .GE. 'A' .AND. CH .LE. 'Z') .OR.
     1    (CH .GE. 'a' .AND. CH .LE. 'z') .OR.
     2    (CH .GE. '0' .AND. CH .LE. '9')) THEN
          IF (WLEN .LT. 8) THEN
              WLEN = WLEN + 1
              WORD(WLEN:WLEN) = CH
          END IF
          POS = POS + 1
          GO TO 50
      END IF
C
60    CONTINUE
C     PAD WORD WITH SPACES
      DO 70 I = WLEN + 1, 8
          WORD(I:I) = ' '
70    CONTINUE
C
C     CHECK FOR KEYWORDS
      CALL CHKKWD(WORD, TOKTYP, TOKVAL)
      RETURN
      END
C
C     CHECK FOR KEYWORDS
C
      SUBROUTINE CHKKWD(WORD, TTYPE, TVAL)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 WORD
C
C     KEYWORD TABLE - COMPACT REPRESENTATION
      IF (WORD .EQ. 'IF      ') THEN
          TTYPE = 1
          TVAL = 1
      ELSE IF (WORD .EQ. 'THEN    ') THEN
          TTYPE = 1
          TVAL = 2
      ELSE IF (WORD .EQ. 'ELSE    ') THEN
          TTYPE = 1
          TVAL = 3
      ELSE IF (WORD .EQ. 'GOTO    ') THEN
          TTYPE = 1
          TVAL = 4
      ELSE IF (WORD .EQ. 'DO      ') THEN
          TTYPE = 1
          TVAL = 5
      ELSE IF (WORD .EQ. 'READ    ') THEN
          TTYPE = 1
          TVAL = 6
      ELSE IF (WORD .EQ. 'WRITE   ') THEN
          TTYPE = 1
          TVAL = 7
      ELSE IF (WORD .EQ. 'PRINT   ') THEN
          TTYPE = 1
          TVAL = 8
      ELSE IF (WORD .EQ. 'INTEGER ') THEN
          TTYPE = 1
          TVAL = 9
      ELSE IF (WORD .EQ. 'REAL    ') THEN
          TTYPE = 1
          TVAL = 10
      ELSE IF (WORD .EQ. 'LOGICAL ') THEN
          TTYPE = 1
          TVAL = 11
      ELSE IF (WORD .EQ. 'CALL    ') THEN
          TTYPE = 1
          TVAL = 12
      ELSE IF (WORD .EQ. 'RETURN  ') THEN
          TTYPE = 1
          TVAL = 13
      ELSE IF (WORD .EQ. 'END     ') THEN
          TTYPE = 1
          TVAL = 14
      ELSE IF (WORD .EQ. 'DIMENSIO') THEN
          TTYPE = 1
          TVAL = 15
      ELSE IF (WORD .EQ. 'LOAD    ') THEN
          TTYPE = 1
          TVAL = 16
      ELSE
          TTYPE = 2
          TVAL = 0
      END IF
      RETURN
      END
C
C     GET OPERATOR OR DELIMITER
C
      SUBROUTINE GETOP
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINE
      CHARACTER*1 CH
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
C
      CH = LINE(POS:POS)
      POS = POS + 1
C
      IF (CH .EQ. '+') THEN
          TOKTYP = 4
          TOKVAL = 1
      ELSE IF (CH .EQ. '-') THEN
          TOKTYP = 4
          TOKVAL = 2
      ELSE IF (CH .EQ. '*') THEN
          TOKTYP = 4
          TOKVAL = 3
      ELSE IF (CH .EQ. '/') THEN
          TOKTYP = 4
          TOKVAL = 4
      ELSE IF (CH .EQ. '=') THEN
          TOKTYP = 4
          TOKVAL = 5
      ELSE IF (CH .EQ. '(') THEN
          TOKTYP = 5
          TOKVAL = 1
      ELSE IF (CH .EQ. ')') THEN
          TOKTYP = 5
          TOKVAL = 2
      ELSE IF (CH .EQ. ',') THEN
          TOKTYP = 5
          TOKVAL = 3
      ELSE
          TOKTYP = 5
          TOKVAL = 0
      END IF
      RETURN
      END
C
C     GET STRING LITERAL
C
      SUBROUTINE GETSTR
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINE
      CHARACTER*1 CH
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
      COMMON /LEXCHR/ LINE
      COMMON /STRVAL/ STRVAL
      CHARACTER*64 STRVAL
C
      TOKTYP = 7
      TOKVAL = 0
      STRVAL = ' '
      STRLEN = 0
C
C     SKIP OPENING QUOTE
      POS = POS + 1
C
80    CONTINUE
      IF (POS .GT. LINLEN) THEN
C         UNTERMINATED STRING
          TOKTYP = 5
          TOKVAL = 0
          RETURN
      END IF
C
      CH = LINE(POS:POS)
      IF (CH .EQ. '"') THEN
C         CLOSING QUOTE
          POS = POS + 1
          RETURN
      END IF
C
C     ADD CHARACTER TO STRING
      IF (STRLEN .LT. 64) THEN
          STRLEN = STRLEN + 1
          STRVAL(STRLEN:STRLEN) = CH
      END IF
      POS = POS + 1
      GO TO 80
      END
