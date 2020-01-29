      FUNCTION idivmax (ka, kna, kguess)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *idivmax*  - Search function
C
C     Purpose:
C     -------
C     Search the greatest common divisor of all elements of an integer array
C
C**   Interface:
C     ---------
C       *ii =*  *idivmax (ka, kna, kguess)*
C
C     Input:
C     -----
C                ka     : array to be searched (integer 1D)
C                kna    : array dimension (integer)
C                kguess : initial guess for the divisor (integer)
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     None
C
C     Reference:
C     ---------
C     See OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      95/09/01  created
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
C
C* ---------------------------- Argument declarations -------------------
C
      INTEGER idivmax, ka(kna)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C
C*    1. Check if kguess divides all ka elements
C        ---------------------------------------
C
      DO 110 jk = kguess, 1, -1
        IF (mod(kguess,jk) .EQ. 0) THEN
            DO 120 ji = 1, kna
              IF (mod(ka(ji),jk) .NE. 0) GO TO 110
 120        CONTINUE
            GO TO 130
        ENDIF 
 110  CONTINUE
 130  idivmax = jk
C
C
C*    2. End of routine
C        --------------
C
      RETURN 
      END
