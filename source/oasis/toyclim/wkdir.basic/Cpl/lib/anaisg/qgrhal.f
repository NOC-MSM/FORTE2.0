      SUBROUTINE qgrhal (prho2, k1to2, kwg2,
     $                   px2, py2, kmsk2, kngx2, kngy2,
     $                   px1, py1, kmsk1, kngx1, kngy1,
     $                   ps12, kvma1, kvma2)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *qgrhal* - Calculate weights and adresses for all the target grid
C
C     Purpose:
C     -------
C     For each point of a target grid 2 give the lpwg2 nearest neighbours 
C     adresses k1to2 in source grid 1 and their weight which is function of
C     the distance and maybe other grid dependant considerations. 
C 
C     N.B : the calculation is done only over unmasked points
C
C**   Interface:
C     ---------
C       *CALL*  *qgrhal (prho2, k1to2, kwg2,
C                        px2, py2, kmsk2, kngx2, kngy2,
C                        px1, py1, kmsk1, kngx1, kngy1,
C                        ps12, kvma1, kvma2)*    
C
C     Input:
C     -----
C                kwg2    : maximum number of nearest neighbors
C                px1     : longitudes for source grid (real 2D)
C                py1     : latitudes for source grid (real 2D)
C                kmsk1   : the mask for source grid (integer 2D)
C                kngx1   : number of longitudes for source grid
C                kngy1   : number of latitudes for source grid
C                px2     : longitudes for target grid (real 2D)
C                py2     : latitudes for target grid (real 2D)
C                kmsk2   : the mask of target grid (integer 2D)
C                kngx2   : number of longitudes for target grid
C                kngy2   : number of latitudes for target grid
C                ps12    : gaussian variance
C                kvma1   : the value of the mask for source grid
C                kvma2   : the value of the mask for target grid 
C
C     Output:
C     ------
C                prho2   : weights for Anaism interpolation (real 3D)
C                k1to2   : source grid neighbors adresses (integer 3D)
C
C     Workspace:
C     ---------
C     None
C
C     External:
C     --------
C     qgrho: to calculate the weights and adresses at one point
C 
C     References:
C     ----------
C     O. Thual, Simple ocean-atmosphere interpolation. 
C               Part A: The method, Epicoa 0629 (1992)
C               Part B: Software implementation, Epicoa 0630 (1992)
C     See also OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      ----------- 
C       1.0       O. Thual       93/04/15  created 
C       1.1       E. Guilyardi   93/11/23  modified
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
      REAL prho2(kwg2,kngx2,kngy2)
      REAL px1(kngx1,kngy1), py1(kngx1,kngy1)
      REAL px2(kngx2,kngy2), py2(kngx2,kngy2)
      INTEGER kmsk1(kngx1,kngy1), kmsk2(kngx2,kngy2)
      INTEGER k1to2(kwg2,kngx2,kngy2)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Neighbours determination
C        ------------------------
C
      DO 110 j2 = 1, kngy2
        DO 120 j1 = 1, kngx2
C
C* For all target grid points:  zero all weights and set adresses to one
C 
          DO 130 jwg = 1, kwg2
            prho2(jwg,j1,j2) = 0.
            k1to2(jwg,j1,j2) = 1 
 130      CONTINUE
C
C* Calculate weights for unmasked points
C
          IF (kmsk2(j1,j2) .NE. kvma2) THEN 
              CALL qgrho (prho2(1,j1,j2), k1to2(1,j1,j2), kwg2,
     $                    px2(j1,j2), py2(j1,j2),
     $                    px1, py1, kmsk1, kngx1, kngy1,
     $                    ps12, kvma1)
          ENDIF 
 120    CONTINUE
 110  CONTINUE 
C
C* End of routine
C
      RETURN
      END
