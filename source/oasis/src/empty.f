      SUBROUTINE empty (cdchar, klen)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *empty*  - Utility routine
C
C     Purpose:
C     -------
C     Fill up a character string with blanks
C
C**   Interface:
C     ---------
C       *call*  *empty (cdchar, klen)*
C
C     Input:
C     -----
C                cdchar : string to be filled up with blanks (char string)
C                klen   : string length (integer)
C
C     Output:
C     ------
C                cdchar : string filled up with blanks
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
      CHARACTER*1 cdchar
      DIMENSION cdchar(klen)
C
C* ---------------------------- Local declarations -------------------
C
      CHARACTER*1 clblank
      DATA clblank/' '/
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Fill s1 with blanks
C        -------------------
C
      DO 110 ji = 1, klen
        cdchar(ji) = clblank
 110  CONTINUE
C
C
C*    2. End of routine
C        --------------
C
      RETURN
      END
