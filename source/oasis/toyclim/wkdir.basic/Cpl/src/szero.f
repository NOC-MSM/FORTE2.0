      SUBROUTINE szero (pa, kna)
C****
C               ********************************
C               * OASIS SUBROUTINE  -  LEVEL T *
C               * ----------------     ------- *
C               ********************************
C
C**** *szero*  - Utility routine
C
C     Purpose:
C     -------
C     Zero real array from element 1 to element kna
C
C**   Interface:
C     ---------
C       *CALL*  *szero (ka, kna)*
C
C     Input:
C     -----
C                pa     : array to be zeroed (real 1D)
C                kna    : array dimension (integer)
C
C     Output:
C     ------
C                pa     : array filled up with zeros (real 1D)
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
      REAL  pa(kna)
C
C* ---------------------------- Local declarations ----------------------
C
      REAL zero
      DATA zero/0.000000000000000E0/
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Zero the array 
C        ---------------
C
      DO 110 ja = 1, kna
        pa(ja) = zero
  110 CONTINUE
C
C
C*    2. End of routine
C        --------------
C
      RETURN
      END
