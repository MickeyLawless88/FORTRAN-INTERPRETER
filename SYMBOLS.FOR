C     SYMBOL TABLE MANAGEMENT FOR FORTRAN IV INTERPRETER
C     OPTIMIZED FOR DOS MEMORY CONSTRAINTS
C     MAXIMUM 50 VARIABLES/ARRAYS TO CONSERVE MEMORY
C
C     VARIABLE TYPES: 1=INTEGER, 2=REAL, 3=LOGICAL
C     ARRAY INFO: DIM1,DIM2,DIM3 = DIMENSIONS (0=SCALAR)
C     STORAGE: 1000 ELEMENT ARRAY POOL
C
      SUBROUTINE INITSM
      IMPLICIT INTEGER (A-Z)
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      CHARACTER*8 NAMES(50)
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      SYMCNT = 0
      POOLPTR = 1
      DO 10 I = 1, 50
          NAMES(I) = '        '
          TYPES(I) = 0
          DIM1(I) = 0
          DIM2(I) = 0
          DIM3(I) = 0
          STARTS(I) = 0
10    CONTINUE
      DO 20 I = 1, 1000
          POOL(I) = 0
20    CONTINUE
      RETURN
      END
C
C     FIND VARIABLE BY NAME
C
      SUBROUTINE FINDVR(NAME, INDEX)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAME, NAMES(50)
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      INDEX = 0
      DO 20 I = 1, SYMCNT
          IF (NAMES(I) .EQ. NAME) THEN
              INDEX = I
              RETURN
          END IF
20    CONTINUE
      RETURN
      END
C
C     ADD NEW VARIABLE
C
      SUBROUTINE ADDVAR(NAME, TYPE, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAME, NAMES(50)
      LOGICAL ERROR
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      ERROR = .FALSE.
C
C     CHECK IF ALREADY EXISTS
      CALL FINDVR(NAME, INDEX)
      IF (INDEX .NE. 0) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
C     CHECK FOR SPACE
      IF (SYMCNT .GE. 50) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
C     ADD SCALAR VARIABLE
      SYMCNT = SYMCNT + 1
      NAMES(SYMCNT) = NAME
      TYPES(SYMCNT) = TYPE
      DIM1(SYMCNT) = 0
      DIM2(SYMCNT) = 0
      DIM3(SYMCNT) = 0
      STARTS(SYMCNT) = 0
      RETURN
      END
C
C     ADD NEW ARRAY
C
      SUBROUTINE ADDARY(NAME, TYPE, D1, D2, D3, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAME, NAMES(50)
      LOGICAL ERROR
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      COMMON /POOLPTR/ POOLPTR
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      ERROR = .FALSE.
C
C     CHECK IF ALREADY EXISTS
      CALL FINDVR(NAME, INDEX)
      IF (INDEX .NE. 0) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
C     CHECK FOR SPACE
      IF (SYMCNT .GE. 50) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
C     CALCULATE ARRAY SIZE
      SIZE = D1
      IF (D2 .GT. 0) SIZE = SIZE * D2
      IF (D3 .GT. 0) SIZE = SIZE * D3
C
C     CHECK POOL SPACE
      IF (POOLPTR + SIZE - 1 .GT. 1000) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
C     ADD ARRAY
      SYMCNT = SYMCNT + 1
      NAMES(SYMCNT) = NAME
      TYPES(SYMCNT) = TYPE
      DIM1(SYMCNT) = D1
      DIM2(SYMCNT) = D2
      DIM3(SYMCNT) = D3
      STARTS(SYMCNT) = POOLPTR
      POOLPTR = POOLPTR + SIZE
      RETURN
      END
C
C     SET VARIABLE VALUE
C
      SUBROUTINE SETVAR(NAME, VALUE, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAME, NAMES(50)
      LOGICAL ERROR
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      COMMON /POOLPTR/ POOLPTR
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      ERROR = .FALSE.
      CALL FINDVR(NAME, INDEX)
      IF (INDEX .EQ. 0) THEN
C         IMPLICIT INTEGER VARIABLE
          CALL ADDVAR(NAME, 1, ERROR)
          IF (ERROR) RETURN
          INDEX = SYMCNT
      END IF
C     ONLY SCALARS CAN BE SET THIS WAY
      IF (DIM1(INDEX) .GT. 0) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C     ALLOCATE POOL SPACE IF NEEDED
      IF (STARTS(INDEX) .EQ. 0) THEN
          STARTS(INDEX) = POOLPTR
          POOLPTR = POOLPTR + 1
      END IF
      POOL(STARTS(INDEX)) = VALUE
      RETURN
      END
C
C     SET ARRAY ELEMENT VALUE
C
      SUBROUTINE SETARR(NAME, I1, I2, I3, VALUE, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAME, NAMES(50)
      LOGICAL ERROR
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      ERROR = .FALSE.
      CALL FINDVR(NAME, INDEX)
      IF (INDEX .EQ. 0) THEN
          ERROR = .TRUE.
          RETURN
      END IF
C
C     CHECK BOUNDS AND CALCULATE OFFSET
      IF (I1 .LT. 1 .OR. I1 .GT. DIM1(INDEX)) THEN
          ERROR = .TRUE.
          RETURN
      END IF
      OFFSET = I1 - 1
C
      IF (DIM2(INDEX) .GT. 0) THEN
          IF (I2 .LT. 1 .OR. I2 .GT. DIM2(INDEX)) THEN
              ERROR = .TRUE.
              RETURN
          END IF
          OFFSET = OFFSET + (I2 - 1) * DIM1(INDEX)
      END IF
C
      IF (DIM3(INDEX) .GT. 0) THEN
          IF (I3 .LT. 1 .OR. I3 .GT. DIM3(INDEX)) THEN
              ERROR = .TRUE.
              RETURN
          END IF
          OFFSET = OFFSET + (I3 - 1) * DIM1(INDEX) * DIM2(INDEX)
      END IF
C
      POOL(STARTS(INDEX) + OFFSET) = VALUE
      RETURN
      END
C
C     GET VARIABLE VALUE
C
      SUBROUTINE GETVAR(NAME, VALUE, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAME, NAMES(50)
      LOGICAL ERROR
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      ERROR = .FALSE.
      CALL FINDVR(NAME, INDEX)
      IF (INDEX .EQ. 0) THEN
          ERROR = .TRUE.
          VALUE = 0
          RETURN
      END IF
C     ONLY SCALARS
      IF (DIM1(INDEX) .GT. 0) THEN
          ERROR = .TRUE.
          VALUE = 0
          RETURN
      END IF
      IF (STARTS(INDEX) .EQ. 0) THEN
          VALUE = 0
      ELSE
          VALUE = POOL(STARTS(INDEX))
      END IF
      RETURN
      END
C
C     GET ARRAY ELEMENT VALUE
C
      SUBROUTINE GETARR(NAME, I1, I2, I3, VALUE, ERROR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAME, NAMES(50)
      LOGICAL ERROR
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      ERROR = .FALSE.
      CALL FINDVR(NAME, INDEX)
      IF (INDEX .EQ. 0) THEN
          ERROR = .TRUE.
          VALUE = 0
          RETURN
      END IF
C
C     CHECK BOUNDS AND CALCULATE OFFSET
      IF (I1 .LT. 1 .OR. I1 .GT. DIM1(INDEX)) THEN
          ERROR = .TRUE.
          VALUE = 0
          RETURN
      END IF
      OFFSET = I1 - 1
C
      IF (DIM2(INDEX) .GT. 0) THEN
          IF (I2 .LT. 1 .OR. I2 .GT. DIM2(INDEX)) THEN
              ERROR = .TRUE.
              VALUE = 0
              RETURN
          END IF
          OFFSET = OFFSET + (I2 - 1) * DIM1(INDEX)
      END IF
C
      IF (DIM3(INDEX) .GT. 0) THEN
          IF (I3 .LT. 1 .OR. I3 .GT. DIM3(INDEX)) THEN
              ERROR = .TRUE.
              VALUE = 0
              RETURN
          END IF
          OFFSET = OFFSET + (I3 - 1) * DIM1(INDEX) * DIM2(INDEX)
      END IF
C
      VALUE = POOL(STARTS(INDEX) + OFFSET)
      RETURN
      END
C
C     LIST ALL VARIABLES (FOR DEBUGGING)
C
      SUBROUTINE LISTVR
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAMES(50)
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      IF (SYMCNT .EQ. 0) THEN
          WRITE(*,100)
100       FORMAT('NO VARIABLES DEFINED')
          RETURN
      END IF
C
      WRITE(*,200)
200   FORMAT('VARIABLES:')
      DO 30 I = 1, SYMCNT
          IF (DIM1(I) .EQ. 0) THEN
C             SCALAR VARIABLE
              IF (STARTS(I) .GT. 0) THEN
                  VALUE = POOL(STARTS(I))
              ELSE
                  VALUE = 0
              END IF
              WRITE(*,300) NAMES(I), VALUE
300           FORMAT('  ',A8,' = ',I8)
          ELSE
C             ARRAY VARIABLE
              IF (DIM3(I) .GT. 0) THEN
                  WRITE(*,400) NAMES(I), DIM1(I), DIM2(I), DIM3(I)
400               FORMAT('  ',A8,' (',I3,',',I3,',',I3,')')
              ELSE IF (DIM2(I) .GT. 0) THEN
                  WRITE(*,500) NAMES(I), DIM1(I), DIM2(I)
500               FORMAT('  ',A8,' (',I3,',',I3,')')
              ELSE
                  WRITE(*,600) NAMES(I), DIM1(I)
600               FORMAT('  ',A8,' (',I3,')')
              END IF
          END IF
30    CONTINUE
      RETURN
      END
C
C     CLEAR ALL VARIABLES
C
      SUBROUTINE CLRVRS
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 NAMES(50)
      COMMON /SYMTAB/ NAMES, TYPES, DIM1, DIM2, DIM3, STARTS, SYMCNT
      COMMON /ARRPOOL/ POOL
      COMMON /POOLPTR/ POOLPTR
      INTEGER TYPES(50), DIM1(50), DIM2(50), DIM3(50), STARTS(50)
      INTEGER POOL(1000)
C
      SYMCNT = 0
      POOLPTR = 1
      DO 40 I = 1, 50
          NAMES(I) = '        '
          TYPES(I) = 0
          DIM1(I) = 0
          DIM2(I) = 0
          DIM3(I) = 0
          STARTS(I) = 0
40    CONTINUE
      DO 50 I = 1, 1000
          POOL(I) = 0
50    CONTINUE
      RETURN
      END
