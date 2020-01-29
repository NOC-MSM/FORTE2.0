       FUNCTION sqdis (px1, py1, px2, py2)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *sqdis*  - Arithmetic function
C
C     Purpose:
C     -------
C     Calculate the distance squared between 2 points on a spheric grid
C
C**   Interface:
C     ---------
C       *zs =*  *sqdis (px1, py1, px2, py2)*
C
C     Input:
C     -----
C                px1    : longitude of first point 
C                py1    : latitude of first point
C                px2    : longitude of second point
C                py2    : latitude of second point
C
C     NB: the coordinates must be in degrees
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
C* ---------------------------- Function declarations -------------------
C
      REAL sqdis, fast
      fast(x,y,z) = cos(x)*cos(y)*z + sin(x)*sin(y)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Calculate distance
C        ------------------
C
C* Degree to radians --> zcon = 2. pi / 360.
C 
      zconv = 1.74532925199432957692e-2 
      zfi1 = px1 * zconv
      zth1 = py1 * zconv
      zfi2 = px2 * zconv
      zth2 = py2 * zconv
C
C* Scalar product
C
      ztp1 = fast(zfi1, zfi2, 1.)
      zsca = fast(zth1, zth2, ztp1)
C
C* Angular distance
C
      IF (zsca .GT. 1.) THEN
          WRITE(UNIT = nulan,FMT = *) 
     $        ' Scalar product is greater than 1 '
          WRITE(UNIT = nulan,FMT = *) 
     $        ' for point 1 -->  longitude = ', px1
          WRITE(UNIT = nulan,FMT = *) 
     $        '             -->  latitude  = ', py1
          WRITE(UNIT = nulan,FMT = *) 
     $        ' and point 2 -->  longitude = ', px2
          WRITE(UNIT = nulan,FMT = *) 
     $        '             -->  latitude  = ', py2
          WRITE(UNIT = nulan,FMT = *)  
     $        ' Scalar product  zsca = ', zsca
          zsca =1.
      ENDIF
      IF (zsca .LT. -1.) THEN 
          WRITE(UNIT = nulan,FMT = *) 
     $        ' Scalar product is less than -1 '
          WRITE(UNIT = nulan,FMT = *) 
     $        ' for point 1 -->  longitude = ', px1
          WRITE(UNIT = nulan,FMT = *) 
     $        '             -->  latitude  = ', py1
          WRITE(UNIT = nulan,FMT = *) 
     $        ' and point 2 -->  longitude = ', px2
          WRITE(UNIT = nulan,FMT = *) 
     $        '             -->  latitude  = ', py2
          WRITE(UNIT = nulan,FMT = *)  
     $        ' Scalar product  zsca = ', zsca
          zsca = -1.
      ENDIF
C
C* Get the distance
C              
      zalp = acos(zsca)
      sqdis = zalp * zalp
C
C* End of function
C
      RETURN 
      END
