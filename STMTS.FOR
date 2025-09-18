C     STATEMENT HANDLERS FOR FORTRAN IV INTERPRETER
C     OPTIMIZED FOR DOS MEMORY CONSTRAINTS
C
C     PRINT STATEMENT HANDLER
C
      SUBROUTINE DPRINT(ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
C
      ERROR = .FALSE.
      CALL GETTOK
C
C     CHECK FOR EXPRESSION LIST
      IF (TOKTYP .EQ. 6) THEN
C         EMPTY PRINT - JUST NEWLINE
          WRITE(*,100)
100       FORMAT(' ')
          RETURN
      END IF
C
C     PRINT EXPRESSION
10    CONTINUE
      CALL PARSEEX(RESULT, ERROR)
      IF (ERROR) RETURN
C
      WRITE(*,200) RESULT
200   FORMAT(' ',I8)
C
C     CHECK FOR MORE EXPRESSIONS
      IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
C         COMMA - MORE EXPRESSIONS
          CALL GETTOK
          GO TO 10
      END IF
      RETURN
      END
C
C     DECLARATION STATEMENT HANDLER
C
      SUBROUTINE DECLAR(VARTYP, ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
      CHARACTER*8 VARNAM
C
      ERROR = .FALSE.
      CALL GETTOK
C
C     PROCESS VARIABLE LIST
20    CONTINUE
      IF (TOKTYP .NE. 2) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
      CALL GETVRN(VARNAM)
      CALL ADDVAR(VARNAM, VARTYP, ERROR)
      IF (ERROR) THEN
          WRITE(*,300)
300       FORMAT('ERROR: VARIABLE ALREADY DECLARED OR NO SPACE')
          RETURN
      END IF
C
      CALL GETTOK
C
C     CHECK FOR MORE VARIABLES
      IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
C         COMMA - MORE VARIABLES
          CALL GETTOK
          GO TO 20
      END IF
      RETURN
      END
C
C     READ STATEMENT HANDLER (SIMPLE VERSION)
C
      SUBROUTINE DREAD(ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
      CHARACTER*8 VARNAM
C
      ERROR = .FALSE.
      CALL GETTOK
C
C     READ VARIABLE LIST
30    CONTINUE
      IF (TOKTYP .NE. 2) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
      CALL GETVRN(VARNAM)
      CALL GETTOK
C
C     PROMPT AND READ VALUE
      WRITE(*,400)
400   FORMAT('? ')
      READ(*,500) VALUE
500   FORMAT(I8)
C
      CALL SETVAR(VARNAM, VALUE, ERROR)
      IF (ERROR) RETURN
C
C     CHECK FOR MORE VARIABLES
      IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
C         COMMA - MORE VARIABLES
          CALL GETTOK
          GO TO 30
      END IF
      RETURN
      END
C
C     WRITE STATEMENT HANDLER (SIMPLE VERSION)
C
      SUBROUTINE DWRITE(ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
C
      ERROR = .FALSE.
      CALL GETTOK
C
C     WRITE EXPRESSION LIST
40    CONTINUE
      CALL PARSEEX(RESULT, ERROR)
      IF (ERROR) RETURN
C
      WRITE(*,600) RESULT
600   FORMAT(I8)
C
C     CHECK FOR MORE EXPRESSIONS
      IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
C         COMMA - MORE EXPRESSIONS
          CALL GETTOK
          GO TO 40
      END IF
      RETURN
      END
C
C     DIMENSION STATEMENT HANDLER
C
      SUBROUTINE DDIMEN(ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
      CHARACTER*8 VARNAM
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
C
      ERROR = .FALSE.
      CALL GETTOK
C
C     PROCESS VARIABLE LIST
70    CONTINUE
      IF (TOKTYP .NE. 2) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
      CALL GETVRN(VARNAM)
      CALL GETTOK
C
C     EXPECT OPENING PARENTHESIS
      IF (TOKTYP .NE. 5 .OR. TOKVAL .NE. 1) THEN
          ERROR = .TRUE.
          RETURN
      END IF
      CALL GETTOK
C
C     GET DIMENSIONS
      D1 = 0
      D2 = 0
      D3 = 0
      NDIMS = 0
C
C     FIRST DIMENSION
      IF (TOKTYP .EQ. 3) THEN
          D1 = TOKVAL
          NDIMS = 1
          CALL GETTOK
      ELSE
          ERROR = .TRUE.
          RETURN
      END IF
C
C     CHECK FOR MORE DIMENSIONS
      IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
          CALL GETTOK
          IF (TOKTYP .EQ. 3) THEN
              D2 = TOKVAL
              NDIMS = 2
              CALL GETTOK
          ELSE
              ERROR = .TRUE.
              RETURN
          END IF
      END IF
C
C     CHECK FOR THIRD DIMENSION
      IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
          CALL GETTOK
          IF (TOKTYP .EQ. 3) THEN
              D3 = TOKVAL
              NDIMS = 3
              CALL GETTOK
          ELSE
              ERROR = .TRUE.
              RETURN
          END IF
      END IF
C
C     EXPECT CLOSING PARENTHESIS
      IF (TOKTYP .NE. 5 .OR. TOKVAL .NE. 2) THEN
          ERROR = .TRUE.
          RETURN
      END IF
      CALL GETTOK
C
C     ADD ARRAY (DEFAULT INTEGER TYPE)
      CALL ADDARY(VARNAM, 1, D1, D2, D3, ERROR)
      IF (ERROR) THEN
          WRITE(*,800)
800       FORMAT('ERROR: CANNOT CREATE ARRAY')
          RETURN
      END IF
C
C     CHECK FOR MORE ARRAYS
      IF (TOKTYP .EQ. 5 .AND. TOKVAL .EQ. 3) THEN
          CALL GETTOK
          GO TO 70
      END IF
      RETURN
      END
C
C     SIMPLE ERROR HANDLER
C
      SUBROUTINE ERRMSG(MSG)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*40 MSG
C
      WRITE(*,700) MSG
700   FORMAT('ERROR: ',A40)
      RETURN
      END
