      FUNCTION iminim (ka, kna)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *iminim*  - Search function
C
C     Purpose:
C     -------
C     Search the minimum of the elements of an integer array
C
C**   Interface:
C     ---------
C       *ii =*  *iminim (ka, kna)*
C
C     Input:
C     -----
C                ka     : array to be searched (integer 1D)
C                kna    : array dimension (integer)
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
      INTEGER iminim, ka(kna)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Find the minimum 
C        ----------------
C
      itemp = 0
      IF (kna .LT. 1) GO TO 110
      itemp = ka(1)
      IF (kna .LT. 2) GO TO 110
      DO 100 ja = 2, kna
        IF (ka(ja) .LT. itemp) itemp = ka(ja)
 100  CONTINUE
 110  CONTINUE
      iminim = itemp
      RETURN
      END
