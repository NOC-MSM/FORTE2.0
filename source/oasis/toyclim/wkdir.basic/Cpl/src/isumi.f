      FUNCTION isumi (ka, kna)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *isumi*  - Arithmetic function
C
C     Purpose:
C     -------
C     Sum of the elements of an integer array
C
C**   Interface:
C     ---------
C       *ii =*  *isumi (ka, kna)*
C
C     Input:
C     -----
C                ka     : array to be summed (integer 1D)
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
      INTEGER isumi, ka(kna)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Perform the sum
C        ---------------
C
      itemp = 0
      DO 110 ji = 1, kna
        itemp = itemp + ka(ji)
 110  CONTINUE
      isumi = itemp
C
C* End of function
C
      RETURN 
      END

