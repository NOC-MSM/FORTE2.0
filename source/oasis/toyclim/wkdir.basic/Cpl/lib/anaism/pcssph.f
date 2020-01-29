      SUBROUTINE pcssph (px, py, psgr, kngx, kngy)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL T *
C               * -------------     ------- *
C               *****************************
C
C**** *pcssph* -  Arithmetic routine
C
C     Purpose:
C     -------
C     Calculate surface element for a spheric and periodic grid.
C     The coordinates are in degrees, there are no pole points.
C
C**   Interface:
C     ---------
C       *CALL*  *pcssph(px, py, psgr, kngx, kngy)*
C
C     Input:
C     -----
C                px   : grid longitudes (real 2D)
C                py   : grid latitudes (real 2D)
C                kngx : number of longitudes
C                kngy : number of latitudes
C
C     Output:
C     ------
C                psgr : grid surface elements (real 2D)
C
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
C       1.1       O. Thual       93/04/15  created 
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
      REAL px(kngx,kngy), py(kngx,kngy)
      REAL psgr(kngx,kngy)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Degres to radians conversion factor
C        -----------------------------------
C
      zconv = 1.74532925199432957692e-2
C
C
C*    2. Surfaces 
C        --------
C
      DO 210 j2 = 1, kngy
        DO 220 j1 = 1, kngx
C
C* Left or right periodicity
C
          IF (J1 .EQ. 1) THEN
              zx1 = px(kngx,j2) - 360.
              zx2 = px(2,j2)
            ELSE IF (j1 .EQ. kngx)  THEN
              zx1 = px(kngx-1,j2)
              zx2 = px(1,j2) + 360.
            ELSE
              zx1 = px(j1-1,j2)
              zx2 = px(j1+1,j2)
          ENDIF
C
C* Bottom or top treatment
C
          IF (j2 .EQ. 1) THEN
              zy1 = -90.
              zy2 = .5 * (py(j1,j2) + py(j1,j2+1))
            ELSE IF (j2 .EQ. kngy) THEN
              zy1 = .5 * (py(j1,j2) + py(j1,j2-1))
              zy2 = 90.
            ELSE 
              zy1 = .5 * (py(j1,j2) + py(j1,j2-1))
              zy2 = .5 * (py(j1,j2) + py(j1,j2+1))
          ENDIF
C
C* Conversion to radians
C
          zfi1 = zx1 * zconv
          zfi2 = zx2 * zconv
          zth1 = zy1 * zconv
          zth2 = zy2 * zconv
C
C* Calculate grid square surface
C   
          zfac = sin(zth2) - sin(zth1) 
          psgr(j1,j2) = abs(.5 * (zfi2-zfi1) * zfac)      
 220    continue
 210  CONTINUE
C
C* End of routine
C
      RETURN 
      END







