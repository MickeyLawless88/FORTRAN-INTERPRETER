C     PROGRAM STORAGE AND EXECUTION FOR FORTRAN IV INTERPRETER
C     OPTIMIZED FOR DOS MEMORY CONSTRAINTS
C     MAXIMUM 200 PROGRAM LINES TO CONSERVE MEMORY
C
      SUBROUTINE INITPR
      IMPLICIT INTEGER (A-Z)
      COMMON /PROGMEM/ LINES, LABELS, LINECNT
      CHARACTER*72 LINES(200)
      INTEGER LABELS(200)
C
      LINECNT = 0
      DO 10 I = 1, 200
          LINES(I) = ' '
          LABELS(I) = 0
10    CONTINUE
      RETURN
      END
C
C     CLEAR PROGRAM MEMORY
C
      SUBROUTINE CLRPGM
      IMPLICIT INTEGER (A-Z)
      COMMON /PROGMEM/ LINES, LABELS, LINECNT
      CHARACTER*72 LINES(200)
      INTEGER LABELS(200)
C
      LINECNT = 0
      DO 20 I = 1, 200
          LINES(I) = ' '
          LABELS(I) = 0
20    CONTINUE
      RETURN
      END
C
C     ADD LINE TO PROGRAM
C
      SUBROUTINE ADDLIN(LINE, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINE, LINES(200)
      LOGICAL ERROR
      COMMON /PROGMEM/ LINES, LABELS, LINECNT
      INTEGER LABELS(200)
C
      ERROR = .FALSE.
      IF (LINECNT .GE. 200) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
      LINECNT = LINECNT + 1
      LINES(LINECNT) = LINE
      LABELS(LINECNT) = 0
C
C     CHECK FOR LABEL
      CALL CHKLBL(LINE, LABELS(LINECNT))
      RETURN
      END
C
C     CHECK FOR STATEMENT LABEL
C
      SUBROUTINE CHKLBL(LINE, LABEL)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINE
      CHARACTER*1 CH
C
      LABEL = 0
      IF (LINE(1:1) .EQ. ' ') RETURN
C
C     CHECK FIRST 5 CHARACTERS FOR NUMERIC LABEL
      DO 30 I = 1, 5
          CH = LINE(I:I)
          IF (CH .EQ. ' ') GO TO 40
          IF (CH .GE. '0' .AND. CH .LE. '9') THEN
              DIGIT = ICHAR(CH) - ICHAR('0')
              LABEL = LABEL * 10 + DIGIT
          ELSE
              LABEL = 0
              RETURN
          END IF
30    CONTINUE
C
40    CONTINUE
      RETURN
      END
C
C     LIST PROGRAM
C
      SUBROUTINE LSTPGM
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINES(200)
      COMMON /PROGMEM/ LINES, LABELS, LINECNT
      INTEGER LABELS(200)
C
      IF (LINECNT .EQ. 0) THEN
          WRITE(*,100)
100       FORMAT('NO PROGRAM LOADED')
          RETURN
      END IF
C
      DO 50 I = 1, LINECNT
          WRITE(*,200) I, LINES(I)
200       FORMAT(I3,': ',A72)
50    CONTINUE
      RETURN
      END
C
C     FIND LINE BY LABEL
C
      SUBROUTINE FINDLB(LABEL, LINENUM)
      IMPLICIT INTEGER (A-Z)
      COMMON /PROGMEM/ LINES, LABELS, LINECNT
      CHARACTER*72 LINES(200)
      INTEGER LABELS(200)
C
      LINENUM = 0
      DO 60 I = 1, LINECNT
          IF (LABELS(I) .EQ. LABEL) THEN
              LINENUM = I
              RETURN
          END IF
60    CONTINUE
      RETURN
      END
C
C     PROCESS LINE (PARSE AND HANDLE STATEMENT)
C
      SUBROUTINE PROCLN(LINE, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINE
      LOGICAL ERROR
C
      ERROR = .FALSE.
C
C     SET UP LEXER
      CALL SETLIN(LINE)
      CALL GETTOK
C
C     CHECK FOR NUMBERED LINE (ADD TO PROGRAM)
      IF (LINE(1:1) .GE. '0' .AND. LINE(1:1) .LE. '9') THEN
          CALL ADDLIN(LINE, ERROR)
          IF (ERROR) THEN
              WRITE(*,100)
100           FORMAT('ERROR: PROGRAM MEMORY FULL')
          ELSE
              WRITE(*,200)
200           FORMAT('LINE ADDED')
          END IF
          RETURN
      END IF
C
C     PROCESS IMMEDIATE STATEMENT (NON-NUMBERED ONLY)
      CALL EXECST(ERROR)
      RETURN
      END
C
C     EXECUTE STATEMENT
C
      SUBROUTINE EXECST(ERROR)
      IMPLICIT INTEGER (A-Z)
      LOGICAL ERROR
      COMMON /LEXCOM/ POS, LINLEN, TOKTYP, TOKVAL
C
      ERROR = .FALSE.
C
      IF (TOKTYP .EQ. 1) THEN
C         KEYWORD STATEMENT
          IF (TOKVAL .EQ. 8) THEN
C             PRINT STATEMENT
              CALL DPRINT(ERROR)
          ELSE IF (TOKVAL .EQ. 9 .OR. TOKVAL .EQ. 10 .OR.
     1             TOKVAL .EQ. 11) THEN
C             DECLARATION STATEMENT
              CALL DECLAR(TOKVAL, ERROR)
          ELSE IF (TOKVAL .EQ. 15) THEN
C             DIMENSION STATEMENT
              CALL DDIMEN(ERROR)
          ELSE
              ERROR = .TRUE.
          END IF
      ELSE IF (TOKTYP .EQ. 2) THEN
C         ASSIGNMENT OR DECLARATION
          CALL ASSIGN(ERROR)
      ELSE
          ERROR = .TRUE.
      END IF
      RETURN
      END
C
C     EXECUTE PROGRAM
C
      SUBROUTINE EXECUT
      IMPLICIT INTEGER (A-Z)
      CHARACTER*72 LINES(200)
      COMMON /PROGMEM/ LINES, LABELS, LINECNT
      INTEGER LABELS(200)
      LOGICAL ERROR
C
      IF (LINECNT .EQ. 0) THEN
          WRITE(*,300)
300       FORMAT('NO PROGRAM TO EXECUTE')
          RETURN
      END IF
C
      PC = 1
400   CONTINUE
      IF (PC .GT. LINECNT) GO TO 500
C
C     EXECUTE LINE
      CALL SETLIN(LINES(PC))
      CALL GETTOK
C     SKIP LABEL IF PRESENT
      IF (LABELS(PC) .NE. 0) CALL GETTOK
C
      CALL EXECST(ERROR)
      IF (ERROR) THEN
          WRITE(*,600) PC
600       FORMAT('RUNTIME ERROR AT LINE ',I3)
          RETURN
      END IF
C
      PC = PC + 1
      GO TO 400
C
500   CONTINUE
      WRITE(*,700)
700   FORMAT('PROGRAM COMPLETED')
      RETURN
      END
C
C     LOAD PROGRAM FROM FILE
C
      SUBROUTINE LOADFL(FNAME, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*64 FNAME
      CHARACTER*72 LINE
      LOGICAL ERROR
      COMMON /PROGMEM/ LINES, LABELS, LINECNT
      CHARACTER*72 LINES(200)
      INTEGER LABELS(200)
C
      ERROR = .FALSE.
C
C     OPEN FILE
      OPEN(UNIT=10, FILE=FNAME, STATUS='OLD', ERR=800)
C
C     READ LINES FROM FILE
810   CONTINUE
      READ(10, 900, END=820, ERR=800) LINE
900   FORMAT(A72)
C
C     ADD LINE TO PROGRAM
      CALL ADDLIN(LINE, ERROR)
      IF (ERROR) THEN
          CLOSE(10)
          RETURN
      END IF
      GO TO 810
C
C     END OF FILE
820   CONTINUE
      CLOSE(10)
      RETURN
C
C     FILE ERROR
800   CONTINUE
      ERROR = .TRUE.
      RETURN
      END
