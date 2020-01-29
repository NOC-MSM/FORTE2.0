      SUBROUTINE pmesh (kx, ky, px, py, kngx, kngy, cdper, kper,
     $                  pxi, pxs, pyi, pys)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *pmesh* -  Calculate the mesh controlled by one point on a
C               periodical or non-periodical grid.
C
C     Purpose:
C     -------
C     For one point defined by the coordinates (px(k),py(k)) 
C     on a lat-lon grid, gives the inferior and superior longitudes 
C     (pxi,pxs) and latitudes (pyi,pys) of the associated mesh.
C     
C
C**   Interface:
C     ---------
C       *CALL*  *pmesh(kx, ky, px, py, kngx, kngy, cdper, kper,
C                      pxi, pxs, pyi, pys)*
C
C     Input:
C     -----
C                kx    : point longitude index
C                ky    : point latitude index
C                px    : longitude array (real 2D)
C                py    : latitude array (real 2D)
C                kngx  : number of longitudes
C                kngy  : number of latitudes
C                cdper : grid periodicity
C                kper  : number of overlapped points
C
C     Output:
C     ------
C                pxi  : inferior longitude
C                pxs  : superior longitude
C                pyi  : inferior latitude
C                pys  : superior latitude
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
C       2.3       L. Terray      99/09/15  changed periodicity variables
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
      CHARACTER*8 cdper
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Left or right 
C        -------------
C
C* Periodic grid with kper overlapped points
C 
      IF (cdper .EQ. 'P') THEN
          IF (kx .EQ. 1) THEN
              pxi = 0.5 * (px(kx,ky) + px(kngx-kper,ky) - 360.)
              pxs = 0.5 * (px(kx,ky) + px(kx+1,ky))
             ELSE IF (kx .EQ. kngx) THEN
              pxi = 0.5 * (px(kx-1,ky) + px(kx,ky))
              pxs = 0.5 * (px(kx,ky) + px(1+kper,ky) + 360. )
            ELSE
              pxi = 0.5 * (px(kx-1,ky) + px(kx,ky))
              pxs = 0.5 * (px(kx,ky) + px(kx+1,ky))           
          ENDIF
C      
C* Regional grid
C 
        ELSE IF (cdper .EQ. 'R') THEN
          IF (kx .EQ. 1) THEN
              pxi = px(kx,ky)
              pxs = 0.5 * (px(kx,ky) + px(kx+1,ky))
            ELSE IF (kx .EQ. kngx) THEN
              pxi = 0.5 * (px(kx-1,ky) + px(kx,ky))
              pxs = px(kx,ky)
            ELSE
              pxi = 0.5 * (px(kx-1,ky) + px(kx,ky))
              pxs = 0.5 * (px(kx,ky) + px(kx+1,ky))
          ENDIF
      END IF
C
C
C*    2. Bottom or top (idem for both types of grid) 
C        -------------------------------------------
C True only if it is a global grid in latitude [-90 , 90]
C or as it is the case for regional models if the first and last 
C latitude row are masked ---> then we only go through the else statement.
C
      IF (ky .EQ. 1) THEN
          pyi = -90.
          pys = 0.5 * (py(kx,ky) + py(kx,ky+1))
        ELSE IF (ky .EQ. kngy) THEN
          pyi = 0.5 * (py(kx,ky-1) + py(kx,ky))
          pys = 90.
        ELSE
          pyi = 0.5 * (py(kx,ky-1) + py(kx,ky))
          pys = 0.5 * (py(kx,ky)+ py(kx,ky+1))
      ENDIF
C
C* End of routine
C
      RETURN
      END

