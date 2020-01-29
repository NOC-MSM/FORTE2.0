       SUBROUTINE qlsst (pqta, prho, kto, kwg, knga, pqtb,
     $                   kngb, kmska, kvma)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *qlsst* - Interpolate a field with a ponderation technique
C
C     Purpose:
C     -------
C     Given the weights prho and indices kto of field pqtb on a source grid,
C     performs a ponderation to generate pqta on target grid.
C 
C     N.B: Nothing is done for the masked points 
C     
C**   Interface:
C     ---------
C       *CALL* *qlsst*(pqta, prho, kto, kwg, knga, pqtb, kngb,
C                      kmska, pmask, kvma)*
C
C     Input:
C     -----
C               prho: array, the weights
C               kto: array, the indices in source grid
C               kwg: the number of neighbors
C               knga: the target grid size
C               pqtb: array, the field on source grid
C               kngb: the source grid size
C               kmska: mask of the target grid
C               kvma:  value of the mask on target grid
C
C     Output:
C     ------
C               pqta: array, the field to calculate
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
      REAL pqtb(kngb), pqta(knga), prho(kwg,knga)
      INTEGER kmska(knga), kto(kwg,knga)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 
C*    1. Ponderation
C        -----------
C
      DO 110 j1 = 1, knga
C
C* Nothing happens if it is a continental point
C
        IF (kmska(j1) .NE. kvma) THEN
            zsum = 0.
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
