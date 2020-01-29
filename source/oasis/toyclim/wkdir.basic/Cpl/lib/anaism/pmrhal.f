      SUBROUTINE pmrhal (pr1to2, k1to2, kw1to2,
     $                   px1, py1, kmsk1, kngx1, kngy1, cdper1, kper1,
     $                   px2, py2, kmsk2, kngx2, kngy2, cdper2, kper2,
     $                   kvma1, kvma2, kmskz2, kvmsz2)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *pmrhal* - Calculate weights and adresses for all the target grid 
C
C     Purpose:
C     -------
C     For each point of a target grid 2 give the kw1to2 closest neighbours 
C     adresses k1to2 in source grid 1 and their weight pr1to2. 
C     Here, neighbours are those in the mesh overlapped by each target point
C     and weights are proportional to the surface mesh intersections. 
C     2D grid assumptions are made here.
C     
C
C**   Interface:
C     ---------
C       *CALL*  *pmrhal(pr1to2, k1to2, kw1to2,
C                       px1, py1, kmsk1, kngx1, kngy1, cdper1, kper1,
C                       px2, py2, kmsk2, kngx2, kngy2, cdper2, kper2,
C                       kvma1, kvma2, kmskz2, kvmsz2)*
C     Input:
C     -----
C                kw1to2  : maximum number of overlapped neighbors
C                px1     : longitudes for source grid (real 2D)
C                py1     : latitudes for source grid (real 2D)
C                kmsk1   : the mask for source grid (integer 2D)
C                kngx1   : number of longitudes for source grid
C                kngy1   : number of latitudes for source grid
C                cdper1  : source grid periodicity 
C                kper1   : number of overlapped points for source grid 
C                px2     : longitudes for target grid (real 2D)
C                py2     : latitudes for target grid (real 2D)
C                kmsk2   : the mask of target grid (integer 2D)
C                kngx2   : number of longitudes for target grid
C                kngy2   : number of latitudes for target grid
C                cdper2  : target grid periodicity
C                kper2   : number of overlapped points for target grid 
C                kvma1   : the value of the mask for source grid
C                kvma2   : the value of the mask for target grid 
C                kvmsz2  : mask value for array kmskz2
C
C     Output:
C     ------
C                pr1to2  : weights for Anaism interpolation (real 3D)
C                k1to2   : source grid neighbors adresses (integer 3D)
C                kmskz2  : number of source grid neighbors (integer 2D)
C
C     Workspace:
C     ---------
C     None
C
C     External:
C     --------
C     pmesh, pmrho
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
      REAL px1(kngx1,kngy1), py1(kngx1,kngy1)
      REAL px2(kngx2,kngy2), py2(kngx2,kngy2)
      REAL pr1to2(kw1to2,kngx2,kngy2)
      INTEGER kmsk1(kngx1,kngy1), kmsk2(kngx2,kngy2)
      INTEGER k1to2(kw1to2,kngx2,kngy2), kmskz2(kngx2,kngy2)
      CHARACTER*8 cdper1, cdper2
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Neighbours determination
C        ------------------------
C 
      DO 110 jy = 1, kngy2
        DO 120 jx = 1, kngx2
C
C* For all target grid points:  zero all weights and set adresses to one
C 
          DO 130 jwg = 1, kw1to2
            pr1to2(jwg,jx,jy) = 0.
            k1to2(jwg,jx,jy) = 1 
 130      CONTINUE
C
C* Calculate the surface of all the target grid squares (masked or not)
C
          CALL pmesh (jx, jy, px2, py2, kngx2, kngy2, cdper2, kper2,
     $                z2xi, z2xs, z2yi, z2ys)
C
C* Calculate the neighbors in the source grid and their weights
C
          CALL pmrho (pr1to2(1,jx,jy), k1to2(1,jx,jy), kw1to2,
     $                z2xi, z2xs, z2yi, z2ys,
     $                px1, py1, kmsk1, kngx1, kngy1, cdper1, kper1,
     $                kvma1, kmskz2(jx,jy), kvmsz2)
C
C* For masked points: 
C
          IF (kmsk2(jx,jy) .EQ. kvma2) THEN
              DO 140 jwg = 1, kw1to2
                pr1to2(jwg,jx,jy) = 0.
                k1to2(jwg,jx,jy) = 1 
 140          CONTINUE
          ENDIF
 120    CONTINUE
 110  CONTINUE 
C
C* End of routine
C
      RETURN 
      END
