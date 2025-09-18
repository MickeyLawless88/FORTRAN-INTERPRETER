C     FORTRAN IV INTERACTIVE INTERPRETER

C     COMPATIBLE WITH MICROSOFT FORTRAN VERSION 3.31

C     OPTIMIZED FOR DOS MEMORY CONSTRAINTS (640KB)

C     

      PROGRAM INTERP

      IMPLICIT INTEGER (A-Z)

C

C     MAIN VARIABLES - MINIMAL MEMORY USAGE

      CHARACTER*72 LINE

      LOGICAL DONE, ERROR

C

C     INITIALIZE INTERPRETER

      CALL INIT

      DONE = .FALSE.

      ERROR = .FALSE.

C

C     MAIN INTERPRETER LOOP

10    CONTINUE

      IF (DONE) GO TO 999

C

C     PROMPT AND READ INPUT

      WRITE(*,100)

100   FORMAT(1X,'FORTRAN> '\)

      READ(*,200,END=999) LINE

200   FORMAT(A72)

C

C     CHECK FOR SYSTEM COMMANDS

      IF (LINE(1:4) .EQ. 'EXIT') THEN

          DONE = .TRUE.

          GO TO 10

      ELSE IF (LINE(1:4) .EQ. 'HELP') THEN

          CALL HELP

          GO TO 10

      ELSE IF (LINE(1:5) .EQ. 'CLEAR') THEN

          CALL CLEAR

          GO TO 10

      ELSE IF (LINE(1:4) .EQ. 'LIST') THEN

          CALL LIST

          GO TO 10

      ELSE IF (LINE(1:3) .EQ. 'RUN') THEN

          CALL EXECUT

          GO TO 10

      ELSE IF (LINE(1:4) .EQ. 'LOAD') THEN

          CALL DOLOAD(LINE, ERROR)

          IF (ERROR) THEN

              WRITE(*,350)

350           FORMAT(1X,'ERROR: CANNOT LOAD FILE')

          END IF

          GO TO 10

      END IF

C

C     PROCESS FORTRAN STATEMENT

      CALL PROCLN(LINE, ERROR)

      IF (ERROR) THEN

          WRITE(*,300)

300       FORMAT(1X,'ERROR: INVALID STATEMENT')

      END IF

C

      GO TO 10

C

999   CONTINUE

      WRITE(*,400)

400   FORMAT(1X,'INTERPRETER TERMINATED')

      END

C

C     INITIALIZATION SUBROUTINE

C

      SUBROUTINE INIT

      IMPLICIT INTEGER (A-Z)

C

C     INITIALIZE SYMBOL TABLE

      CALL INITSM

C

C     INITIALIZE PROGRAM STORAGE

      CALL INITPR

C

C     INITIALIZE PARSER

      CALL INITPS

C

      WRITE(*,100)

100   FORMAT(1X,'FORTRAN IV INTERACTIVE INTERPRETER V1.0')

      WRITE(*,200)

200   FORMAT(1X,'TYPE HELP FOR AVAILABLE COMMANDS')

      RETURN

      END

C

C     HELP COMMAND

C

      SUBROUTINE HELP

      IMPLICIT INTEGER (A-Z)

C

      WRITE(*,100)

100   FORMAT(1X,'AVAILABLE COMMANDS:')

      WRITE(*,200)

200   FORMAT(1X,'  EXIT     - EXIT INTERPRETER')

      WRITE(*,300)

300   FORMAT(1X,'  HELP     - DISPLAY THIS HELP')

      WRITE(*,400)

400   FORMAT(1X,'  CLEAR    - CLEAR PROGRAM MEMORY')

      WRITE(*,500)

500   FORMAT(1X,'  LIST     - LIST CURRENT PROGRAM')

      WRITE(*,600)

600   FORMAT(1X,'  RUN      - EXECUTE CURRENT PROGRAM')

      WRITE(*,650)

650   FORMAT(1X,'  LOAD "FILE" - LOAD PROGRAM FROM FILE')

      WRITE(*,700)

700   FORMAT(1X,'FORTRAN STATEMENTS:')

      WRITE(*,800)

800   FORMAT(1X,'  INTEGER, REAL, LOGICAL - DECLARATIONS')

      WRITE(*,900)

900   FORMAT(1X,'  IF, GOTO, DO - CONTROL STATEMENTS')

      WRITE(*,1000)

1000  FORMAT(1X,'  READ, WRITE, PRINT - I/O STATEMENTS')

      WRITE(*,1100)

1100  FORMAT(1X,'  ASSIGNMENT STATEMENTS')

      RETURN

      END

C

C     CLEAR PROGRAM MEMORY

C

      SUBROUTINE CLEAR

      IMPLICIT INTEGER (A-Z)

C

      CALL CLRPGM

      WRITE(*,100)

100   FORMAT(1X,'PROGRAM MEMORY CLEARED')

      RETURN

      END

C

C     LIST CURRENT PROGRAM

C

      SUBROUTINE LIST

      IMPLICIT INTEGER (A-Z)

      CALL LSTPGM

      RETURN

      END

C

C     LOAD PROGRAM FROM FILE

C

      SUBROUTINE DOLOAD(LINE, ERROR)

      IMPLICIT INTEGER (A-Z)

      CHARACTER*72 LINE

      CHARACTER*64 FNAME

      LOGICAL ERROR

C

C     PARSE FILENAME FROM LOAD COMMAND

      CALL GETFNM(LINE, FNAME, ERROR)

      IF (ERROR) RETURN

C

C     CLEAR EXISTING PROGRAM

      CALL CLRPGM

      CALL CLRVRS

C

C     LOAD FILE

      CALL LOADFL(FNAME, ERROR)

      IF (.NOT. ERROR) THEN

          WRITE(*,100)

100       FORMAT('OK')

      END IF

      RETURN

      END

C

C     EXTRACT FILENAME FROM LOAD COMMAND

C

      SUBROUTINE GETFNM(LINE, FNAME, ERROR)

      IMPLICIT INTEGER (A-Z)

      CHARACTER*72 LINE

      CHARACTER*64 FNAME

      LOGICAL ERROR

C

      ERROR = .FALSE.

      FNAME = ' '

C

C     FIND OPENING QUOTE

      QSTART = 0

      DO 10 I = 5, 72

          IF (LINE(I:I) .EQ. '"') THEN

              QSTART = I + 1

              GO TO 20

          END IF

10    CONTINUE

      ERROR = .TRUE.

      RETURN

C

C     FIND CLOSING QUOTE

20    CONTINUE

      QEND = 0

      DO 30 I = QSTART, 72

          IF (LINE(I:I) .EQ. '"') THEN

              QEND = I - 1

              GO TO 40

          END IF

30    CONTINUE

      ERROR = .TRUE.

      RETURN

C

C     EXTRACT FILENAME

40    CONTINUE

      IF (QEND .GE. QSTART) THEN

          FLEN = QEND - QSTART + 1

          IF (FLEN .LE. 64) THEN

              FNAME = LINE(QSTART:QEND)

          ELSE

              ERROR = .TRUE.

          END IF

      ELSE

          ERROR = .TRUE.

      END IF

      RETURN

      END

