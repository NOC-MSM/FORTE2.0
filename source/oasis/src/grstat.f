      SUBROUTINE grstat (pcorx, pcory, psgr, kmsk, kngx, kngy,
     $                   pdr, psr, phhi, kvma)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL T *
C               * -------------     ------- *
C               *****************************
C
C**** *grstat* - Statistic routine
C
C     Purpose:
C     -------
C     Given a grid, calculate the average grid square size,
C     the total surface and the inverse of sum of surface elements squared.
C
C**   Interface:
C     ---------
C       *CALL*  *grstat (pcorx, pcory, psgr, kmsk, kngx, kngy
C                        pdr, psr, phhi, kvma)*
C
C     Input:
C     -----
C                pcorx : the grid points longitude (real 2D)
C                pcory : the grid points latitude (real 2D)
C                psgr  : surface elements (real 2D)
C                kmsk  : the grid mask (integer 2D)
C                kngx  : the grid size in direction 1
C                kngy  : the grid size in direction 2 
C                kvma  : integer value of the mask 
C
C     Output:
C     ------
C                pdr   : the average grid square size
C                psr   : the total surface
C                phhi  : inverse of sum of surface elements squared
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     sqdis
C
C     Reference:
C     ---------
C     O. Thual, Simple ocean-atmosphere interpolation. 
C               Part A: The method, EPICOA 0629 (1992)
C               Part B: Software implementation, EPICOA 0630 (1992)
C
C     See also OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       1.1       O. Thual       94/01/01  created
C       2.0       L. Terray      95/12/26  modified : to suit OASIS 2.0
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
      REAL pcorx(kngx,kngy), pcory(kngx,kngy), psgr(kngx,kngy)
      INTEGER kmsk(kngx,kngy)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C   
      ivma = kvma
C
C
C*    2. Interdistances only for unmasked points
C        ---------------------------------------
C
C* Average distance squared in x-direction
C
      ztot2 = 0.
      icount = 0
      DO 210 j2 = 1, kngy
      zsum = 0.
      DO 220 j1 = 1, kngx-1
        IF (kmsk(j1,j2) .NE. ivma) THEN
            zdis = sqdis(pcorx(j1,j2), pcory(j1,j2),
     $                   pcorx(j1+1,j2), pcory(j1+1,j2))
            icount = icount + 1
            zsum = zsum + zdis
        ENDIF
 220  CONTINUE
      ztot2 = ztot2 + zsum
 210  CONTINUE
      ztot2 = ztot2 / float(icount)
C
C* Average distance squared in y-direction
C
      ztot1 = 0.
      icount = 0
      DO 230 j1 = 1, kngx
      zsum = 0.
      DO 240 j2 = 1, kngy-1
         IF(kmsk(j1,j2) .NE. ivma) THEN
            zdis = sqdis(pcorx(j1,j2), pcory(j1,j2),
     $                   pcorx(j1,j2+1), pcory(j1,j2+1))
            icount = icount + 1
            zsum = zsum + zdis
         ENDIF
 240   CONTINUE
      ztot1 = ztot1 + zsum
 230  CONTINUE
      ztot1 = ztot1 / float(icount)
C
C* Get average grid square size
C
      pdr = (ztot1 + ztot2) / 2.
C
C
C*    3. Surface and <hh>^-1
C        -------------------
C
      zsurf = 0.
      zhhi = 0.
      DO 310 j2 = 1, kngy
        DO 320 j1 = 1, kngx
          IF (kmsk(j1,j2) .NE. ivma) THEN
              zsurf = zsurf + psgr(j1,j2)
              zhhi = zhhi + psgr(j1,j2) * psgr(j1,j2)
          ENDIF
 320    CONTINUE 
 310  CONTINUE
C
C* Get total surface and inverse of sum of elements squared
C
      psr = zsurf
      phhi = 1. / zhhi
C
C
C*    4. End of routine
C        --------------
C 
      RETURN       
      END
