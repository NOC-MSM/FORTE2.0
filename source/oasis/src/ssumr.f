      FUNCTION ssumr (pa, kna)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *ssumr*  - Arithmetic function
C
C     Purpose:
C     -------
C     Sum of the elements of a real array
C
C**   Interface:
C     ---------
C       *zs =*  *ssumr (pa, kna)*
C
C     Input:
C     -----
C                pa     : array to be summed (real 1D)
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
      REAL ssumr, pa(kna)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Perform the sum
C        ---------------
C
      ztemp = 0.
      DO 110 ji = 1, kna
        ztemp = ztemp + pa(ji)
 110  CONTINUE
      ssumr = ztemp
C
C* End of function
C
      RETURN 
      END

