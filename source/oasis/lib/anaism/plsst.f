       SUBROUTINE plsst (pqta, prho, kto, kwg, knga, pqtb,
     $                   kngb, kmska, kvma)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *plsst* -  Perform subgrid interpolation through Anaism method 
C
C     Purpose:
C     -------
C     Given the weights prho and indices kto for field pqtb, performs
C     a surface ponderation to generate field pqta on target grid.
C 
C     N.B: Nothing is done for the masked points
C
C**   Interface:
C     ---------
C       *CALL*  *plsst(pqta, prho, kto, kwg, knga, pqtb,
C                      kngb, kmska, kvma)*
C
C     Input:
C     -----
C                kmska : mask for target grid (integer 1D)
C                kvma  : the value of the mask for target grid
C                knga  : number of points for target grid
C                prho  : weights for Anaism interpolation (real 2D)
C                kto   : source grid neighbors adresses (integer 2D)
C                kwg   : maximum number of overlapped neighbors
C                pqtb  : field on source grid (real 1D)
C                kngb  : number of points for source grid
C
C     Output:
C     ------
C                pqta  : field on target grid (real 1D)
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
      REAL pqtb(kngb), pqta(knga), prho(kwg,knga)
      INTEGER kmska(knga), kto(kwg,knga)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*     1. Ponderation
C         -----------
C* Loop over all target grid points
C
      DO 110 j1 = 1, knga
C
C* Nothing happens if it is a continental point
C
        IF (kmska(j1) .NE. kvma) THEN 
            zsum = 0.
C
C* Ponderation over all overlapped source grid neighbors 
C
            DO 120 j2 = 1, kwg
              zsum = zsum + prho(j2,j1) * pqtb(kto(j2,j1))
 120        CONTINUE
            pqta(j1) = zsum
        ENDIF
 110  CONTINUE
C
C* End of routine
C
      RETURN
      END
