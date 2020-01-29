      SUBROUTINE qgrho (prho, kto, kwg,
     $                  px, py,
     $                  px1, py1, kmsk1, kngx1, kngy1,
     $                  psig, kvma1)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *qgrho* - Calculate weights and adresses at one location 
C
C     Purpose:
C     -------
C     At one location xgr gives the kwg closest neighbours 
C     adresses kto in grid 1 and their weight, which is function of
C     the distance and may be other grid dependant considerations. 
C
C**   Interface:
C     ---------
C       *CALL*  *qgrho(prho, kto, kwg,
C                      px, py,
C                      px1, py1, kmsk1, kngx1, kngy1,
C                      psig, kvma1)* 
C
C     Input:
C     -----
C                kwg   : maximum number of overlapped neighbors
C                px    : longitude of the current point on target grid
C                py    : latitude of the current point on source grid
C                px1   : longitudes for source grid (real 2D)
C                py1   : latitudes for source grid (real 2D)
C                kmsk1 : the mask for source grid (integer 2D)
C                kngx1 : number of longitudes for source grid
C                kngy1 : number of latitudes for source grid
C                kvma1 : the value of the mask for source grid
C                psig  : variance of the gaussian
C     Output:
C     ------
C                prho  : the neighbors weights 
C                kto   : the neighbors adresses in the source grid 
C
C     Workspace:
C     ---------
C     None
C
C     External:
C     --------
C     qlsort, sqdis, qlins, qlgaus
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
      REAL px1(kngx1,kngy1), py1(kngx1,kngy1)
      REAL prho(kwg)
      INTEGER kmsk1(kngx1,kngy1)
      INTEGER kto(kwg)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     
C*    1. Various initializations
C        -----------------------
C
C* Value of the mask
C
      ivma = kvma1
      ing = kngx1 * kngy1
C
C
C*    2. Nearest neighbors search
C        ------------------------
C
C* Checks
C     
      IF (kwg .GT. ing) THEN
          WRITE (UNIT = nulou,FMT = *) 'WARNING kwg = ',kwg,
     $                  '.GT. kngx1*kngy1 = ',ing
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Number of searched neighbors greater '
          WRITE (UNIT = nulou,FMT = *) 
     $        ' than number of grid points '
          iwg = ing
          WRITE (UNIT = nulou,FMT = *)  'We set  iwg = ing = ',iwg
        ELSE
         iwg = kwg
      ENDIF
C
C
C* Put first iwg unmasked points of source grid in kto and prho
C
      iind = 0
      DO 210 j2 = 1, kngy1
        DO 220 j1 = 1, kngx1
          IF (kmsk1(j1,j2) .NE. ivma) THEN
              iind = iind + 1
              icum = (j2-1) * kngx1 + j1
              prho(iind) = sqdis (px, py, px1(j1,j2), py1(j1,j2))
              kto(iind) = icum
              i1t = j1
              i2t = j2
              IF (iind .EQ. iwg) GO TO 225
          ENDIF
 220    CONTINUE 
 210  CONTINUE
 225  CONTINUE
C
C* Check
C
      IF (iind .NE. iwg) THEN
          WRITE(UNIT = nulou,FMT = *) 
     $        ' WARNING: iind.ne.iwg ===> ',iind, iwg
          WRITE(UNIT = nulou,FMT = *) 
     $        ' *******  ===> Program must stop '
          CALL HALTE ('STOP in qgrho')
      ENDIF
C
C* Sorting of these first iwg points
C
      CALL qlsort (prho, kto, iwg)
C
C* Loop on the remaining points of source grid
C
      IF (iwg .NE. ing) THEN    
C
C* Loop over the remaining longitudes for latitude i2t on source grid 
C  if necessary
C
          IF (i1t .LT. kngx1) THEN	
              DO 230 j1 = i1t+1, kngx1
                IF (kmsk1(j1,i2t) .NE. ivma) THEN
                    icum = (i2t-1) * kngx1 + j1
C
C* Calculate distance between the current source grid point and
C  the given target grid point
C
                    zrnew = sqdis (px, py, px1(j1,i2t), py1(j1,i2t))
C
C* Insert in list at the correct rank
C
                    CALL qlins (prho, kto, iwg, zrnew, icum)
                ENDIF
 230          CONTINUE 
          ENDIF
C
C* Loop over the remaining source grid points
C
          DO 240 j2 = i2t+1, kngy1
            DO 250 j1 = 1, kngx1
              IF (kmsk1(j1,j2) .NE. ivma) THEN
                  icum = (j2-1) * kngx1 + j1
C
C* Calculate distance between the current source grid point and
C  the given target grid point
C
                  zrnew = sqdis (px, py, px1(j1,j2), py1(j1,j2))
C
C* Insert in list at the correct rank
C
                  CALL qlins (prho, kto, iwg, zrnew, icum)
              ENDIF
 250        CONTINUE 
 240      CONTINUE
      ENDIF
C 
C
C*    3. Weight function
C        ---------------
C
C* Gaussian value for the weight function
C
      zsum = 0.
      DO 310 jwg = 1, iwg
          zr = qlgaus (prho(jwg), psig)
          zsum = zsum + zr
          prho(jwg) = zr
 310  CONTINUE
C
C* Normalization
C
      zthres = 1.e-6
C
C* If under threshold
C
      IF (zsum .LT. zthres) THEN
          DO 320 jwg = 1, iwg
            prho(jwg) = 1. / float(iwg)
 320      CONTINUE
C
        ELSE
C
C* If above threshold
C
          DO 330 jwg = 1, iwg
            prho(jwg) = prho(jwg) / zsum
 330      CONTINUE
      ENDIF
C
C* End of routine
C
      RETURN 
      END
