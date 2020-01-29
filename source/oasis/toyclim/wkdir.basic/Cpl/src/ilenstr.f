      FUNCTION ilenstr (cdstr, knstr)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *ilenstr*  - Search function
C
C     Purpose:
C     -------
C     Find the length of a character string (no blank characters)
C
C**   Interface:
C     ---------
C       *ii =*  *ilenstr (cdstr, knstr)*
C
C     Input:
C     -----
C                cdstr : string to be searched (char string)
C                knstr : total length of the string (integer)
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
C     nextch, nextbl
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
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C
C*    1. Find character string length
C        ----------------------------
C
      ii = nextch (cdstr, 1, knstr)
      ii = nextbl (cdstr, ii, knstr)
      ii = ii - 1
      ilenstr = ii
C
C
C*    2. End of function
C        ---------------
C
      RETURN
      END
