      FUNCTION jcoor (kind, kimax)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *jcoor*  - Search function
C
C     Purpose:
C     -------
C     Given an index value, kind, for a 2D array which has kimax rows, 
C     this function returns the column number
C
C**   Interface:
C     ---------
C       *ii =*  *jcoor (kind, kimax)*
C
C     Input:
C     -----
C                kind   : index (integer)
C                kimax  : number of rows (integer)
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
C       1.1       R. Sutton      95/11/25  Created
C       2.0       L. Terray      95/12/26  Modified: to suit OASIS 2.0
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
      INTEGER jcoor
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Find the column number 
C        ----------------------
C
      jcoor = 1 + (kind-1) / kimax
C
C* End of function
C
      RETURN
      END