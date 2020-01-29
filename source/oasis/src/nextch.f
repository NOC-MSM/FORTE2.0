      FUNCTION nextch (cdstr, kistr, knstr)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *nextch*  - Search function
C
C     Purpose:
C     -------
C     Find the first non-blank in a character string
C
C**   Interface:
C     ---------
C       *ii =*  *nextch (cdstr, kistr, knstr)*
C
C     Input:
C     -----
C                cdstr : string to be searched (char string)
C                kistr : initial search position within the string (integer)
C                knstr : final search position within the string (integer)
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
      CHARACTER*1 cdstr
      DIMENSION cdstr(knstr)
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
C
C*    1. Find the first non-blank character
C        ----------------------------------
C
      DO 110 ji = kistr, knstr
        idum = ji
        IF (cdstr(ji) .NE. clblank) GO TO 120
  110 CONTINUE
  120 CONTINUE
      nextch = idum
      IF (idum .GE. knstr) nextch = -1
C
C
C*    2. End of function
C        ---------------
C
      RETURN
      END
