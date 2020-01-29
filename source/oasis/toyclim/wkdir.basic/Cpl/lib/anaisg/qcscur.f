      SUBROUTINE qcscur (psgr, kngx, kngy, psurf)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL T *
C               * -------------     ------- *
C               *****************************
C
C**** *qcscur* - Arithmetic routine
C                
C
C     Purpose:
C     -------
C     Calculate surface element for orth.& curvil. grid.
C     The input surface contains the Rearth**2 factor.
C
C**   Interface:
C     ---------
C       *CALL*  *qcscur(psgr, kngx, kngy, psurf)*
C
C     Input:
C     -----
C                psurf : grid square surface elements (real 2D)
C                kngx  : number of longitudes
C                kngy  : number of latitudes
C
C     Output:
C     ------
C                psgr  : spheric grid square surface elements (real 2D)
C
C     Workspace:
C     ---------
C     None
C
C     External:
C     --------
C     None
C
C     References:
C     ----------
C     O. Thual, Simple ocean-atmosphere interpolation. 
C               Part A: The method, EPICOA 0629 (1992)
C               Part B: Software implementation, EPICOA 0630 (1992)
C     See also OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      ----------- 
C       1.0       O. Thual       93/04/15  created 
C       1.1       E. Guilyardi   93/11/04  modified
C       2.0       L. Terray      95/10/01  modified: new structure
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
      REAL psurf(kngx,kngy)
      REAL psgr(kngx,kngy)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initializations
C        ---------------
C
      zradi = 6371229.
      zradi2 = zradi**2
      zradinv = 1. / zradi2
C
C
C*    2. Surfaces 
C        --------
C* Surface calculation
C
      DO 210 j2 = 1, kngy
        DO 220 j1 = 1, kngx
          psgr(j1,j2) = psurf(j1,j2) * zradinv 
 220    CONTINUE
 210  CONTINUE
C
C* End of routine
C
      RETURN 
      END







