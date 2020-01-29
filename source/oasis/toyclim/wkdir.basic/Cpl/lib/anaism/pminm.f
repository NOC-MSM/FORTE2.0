      SUBROUTINE pminm (kflag, psurf,
     $                  p2xi, p2xs, p2yi, p2ys,
     $                  p1xi, p1xs, p1yi, p1ys)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *pminm* -  calculate intersection between two meshes
C
C     Purpose:
C     -------
C     For two meshes defined by their inf. and sup. latitude and
C     longitude (p1xi,p1xs,p1yi,p1ys) and (p2xi,p2xs,p2yi,p2ys), checks
C     if they have any intersection and calculates the surface, psurf, 
C     of that intersection. The 2 grids considered may be periodic
C     or non-periodic.
C
C     N.B: Intersection surface is given by the following formula:
C          Si = r*r * (sin(lat2)-sin(lat1)) * (phi2-phi1)
C              with lat2, lat1 sup. and inf. latitudes
C                   phi2, phi1 sup. and inf. longitudes
C          This results from the integration of dS=r*r*cos(lat)*d(lat)*d(phi)
C
C**   Interface:
C     ---------
C       *CALL*  *pminm(kflag, psurf,
C                      p2xi, p2xs, p2yi, p2ys,
C                      p1xi, p1xs, p1yi, p1ys)*
C
C     Input:
C     -----
C                p2xi  : target grid square inferior longitude
C                p2xs  : target grid square superior longitude
C                p2yi  : target grid square inferior latitude
C                p2ys  : target grid square superior latitude
C                p1xi  : source grid square inferior longitude
C                p1xs  : source grid square superior longitude
C                p1yi  : source grid square inferior latitude
C                p1ys  : source grid square superior latitude
C
C     Output:
C     ------
C                psurf  : the intersection's surface
C                kflag  : 1 if intersection is not empty, 0 otherwise
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
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      kflag = 0
      psurf = 0.
      z2pi = 2. * acos(-1.)
      zconv = 1.74532925199432957692e-2
C
C* Assign local variables
C
      zfi1i = p1xi
      zfi1s = p1xs
      zfi2i = p2xi
      zfi2s = p2xs
C
C* Put longitudes in the [0-360] interval to avoid problem in 
C  surface calculation.
C
      IF (p1xi .GE. 360.0) zfi1i = zfi1i - 360.0
      IF (p1xi .LT. 0. ) zfi1i = zfi1i + 360.0
      IF (p1xs .GT. 360.0) zfi1s = zfi1s - 360.0
      IF (p1xs .LE. 0. ) zfi1s = zfi1s + 360.0
      IF (p2xi .GE. 360.0) zfi2i = zfi2i - 360.0
      IF (p2xi .LT. 0. ) zfi2i = zfi2i + 360.0
      IF (p2xs .GT. 360.0) zfi2s = zfi2s - 360.0
      IF (p2xs .LE. 0. ) zfi2s = zfi2s + 360.0
C
C* Conversion degrees to radians
C
      zfi1i = zfi1i * zconv
      zfi1s = zfi1s * zconv
      zth1i = p1yi * zconv
      zth1s = p1ys * zconv
      zfi2i = zfi2i * zconv
      zfi2s = zfi2s * zconv
      zth2i = p2yi * zconv
      zth2s = p2ys * zconv
C
C
C*    2. Check and calculation of intersection
C        -------------------------------------
C
C* Check of latitudinal intersection
C 
      IF (zth2s .GT. zth1i .AND. zth2i .LT. zth1s) THEN
C
C* Calculate the delta(latitude)
C
C* Latitude inf.
C
          zlati = amax1(zth2i,zth1i)
C
C* Latitude sup.
C
          zlats = amin1(zth2s,zth1s)
C
C* sin(lat_sup) - sin(lat_inf)
C
          zdth = sin(zlats) - sin(zlati)
C
C* Check of longitudinal intersection
C
          zfi1ir = zfi1i - z2pi
          zfi2ir = zfi2i - z2pi
          zfi2sr = zfi2s - z2pi
C   
C* Case of a mesh 1 whose minimal longitude zfi1i is less than the
C  maximal one zfi1s (normal case). The opposite can occur only for
C  a periodic grid.
C 
          IF (zfi1i .LT. zfi1s) THEN
C
C - Check if it is similar for mesh 2
C
              IF (zfi2i .LT. zfi2s) THEN
C
C - If yes, find the intersection
C
                  IF (zfi2s .GT. zfi1i .AND. zfi2i .LT. zfi1s) THEN
                      kflag = 1
                      zdfi = amin1(zfi2s,zfi1s) - amax1(zfi2i,zfi1i)
                      psurf = zdth * zdfi
                  ENDIF
C
C - If not, mesh 2 is periodic
C
                ELSE IF (zfi2i .GT. zfi2s) THEN
C
C - Find intersection in that case
C 
                  IF (zfi2s .GT. zfi1i .AND. zfi2ir .LT. zfi1s) THEN
                      kflag = 1
                      zdfi = amin1(zfi2s,zfi1s) - zfi1i
                      psurf = zdth * zdfi
                  ENDIF
                  IF (zfi2s .GT. zfi1ir .AND. zfi2i .LT. zfi1s) THEN
                      kflag = 1
                      zdfi = amin1(zfi2i,zfi1s) - zfi1i
                      psurf = zdth * zdfi
                  ENDIF
              ENDIF
          ENDIF
C
C* Case of a mesh 1 whose minimal longitude zfi1i is located
C  on the right of the grid and whose maximum longitude zfi1s
C  is on the left of the grid. This occurs only for a periodic mesh
C 
          IF (zfi1i .GT. zfi1s) THEN
C
C - Check for mesh 2
C
              IF (zfi2i .LT. zfi2s) THEN
C
C - Find intersection
C
                  IF (zfi2s .GT. zfi1ir .AND. zfi2i .LT. zfi1s) THEN
                      kflag = 1
                      zdfi = amin1(zfi2s,zfi1s) - zfi2i
                      psurf = zdth * zdfi 
                    ELSE IF (zfi2sr .GT. zfi1ir .AND. 
     $                    zfi2ir .LT. zfi1s) THEN
                      kflag = 1
                      zdfi = zfi2s - amin1(zfi2i,zfi1i)
                      psurf = zdth * zdfi
                  ENDIF
C
C - Mesh 2 is periodic
C
                ELSE IF (zfi2i .GT. zfi2s) THEN 
C
C - Find intersection
C
                  IF (zfi2s .GT. zfi1ir .AND. zfi2ir .LT. zfi1s) THEN
                      kflag = 1
                      zdfi = amin1(zfi2s,zfi1s) - amax1(zfi2ir,zfi1ir)
                      psurf = zdth * zdfi 
                  ENDIF
              ENDIF 
          ENDIF
      ENDIF
C
C* End of routine
C
      RETURN 
      END

