      SUBROUTINE pmrho (prho, kto, kwg,
     $                  p2xi, p2xs, p2yi, p2ys,
     $                  px1, py1, kmsk1, kngx, kngy, cdper, kper,
     $                  kvma1, kmsz2, kvmsz2)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *pmrho* - Calculate weights and adresses at one location 
C     
C     Purpose:
C     -------
C     At one target grid location, gives the kwg closest neighbours 
C     adresses kto in source grid and their weight prho, which is function of
C     the surface intersection (if no overlap, weight is zero).
C
C     N.B: Note that the search is done ONLY over unmasked points. 
C 
C**   Interface:
C     ---------
C       *CALL*  *pmrho(prho, kto, kwg,
C                      p2xi, p2xs, p2yi, p2ys,
C                      px1, py1, kmsk1, kngx, kngy, cdper, kper,
C                      kvma1, kmsz2, kvmsz2)* 
C
C     Input:
C     -----
C                kwg   : maximum number of overlapped neighbors
C                p2xi  : the inf longitude of the target grid square
C                p2xs  : the sup longitude of the target grid square
C                p2yi  : the inf latitude of the target grid square
C                p2ys  : the sup latitude of the target grid square
C                px1   : longitudes for source grid (real 2D)
C                py1   : latitudes for source grid (real 2D)
C                kmsk1 : the mask for source grid (integer 2D)
C                kngx  : number of longitudes for source grid
C                kngy  : number of latitudes for source grid
C                kvma1 : the value of the mask for source grid
C                cdper : source grid periodicity 
C                kper  : number of overlapped points for source grid
C     Output:
C     ------
C                prho  : the neighbors weights 
C                kto   : the neighbors adresses in the source grid 
C                kmsz2 : number of source grid neighbors (integer 2D)
C
C     Workspace:
C     ---------
C     None
C
C     External:
C     --------
C     pmesh, pminm
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
C       2.3       L. Terray      99/09/15  changed periodicity variable
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
      REAL px1(kngx,kngy), py1(kngx,kngy)
      REAL prho(kwg)
      INTEGER kmsk1(kngx,kngy)
      INTEGER kto(kwg)
      CHARACTER*8 cdper
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Various initializations
C        -----------------------
C 
      ivma1 = kvma1
      ing = kngx * kngy
C
C   
C*    2. Neighbors (mesh) determination
C        -------------------------------
C* Checks
C     
      IF (kwg .GT. ing) THEN
          WRITE(UNIT = nulou,FMT = *) 
     $        'WARNING in pmrho -- kwg = ',kwg,' .gt. ing = ',ing
          WRITE(UNIT = nulou,FMT = *) 
     $        ' Number of searched neighbors greater '
          WRITE(UNIT = nulou,FMT = *) 
     $        ' than number of grid points '
          iwg = ing
          WRITE(UNIT = nulou,FMT = *)  'We set  iwg = ing = ',iwg
        ELSE
          iwg = kwg
      ENDIF
C
C* zero the weights and set adresses to one
C
      CALL szero(prho,iwg)
      DO 210 ji = 1, iwg
        kto(ji) = 1
 210  CONTINUE
C
C* Loop on all the points of source grid  
C
      iwco = 0
      DO 220 jy = 1, kngy
        DO 230 jx = 1, kngx
C
C* If it is an unmasked point
C
          IF (kmsk1(jx,jy) .NE. ivma1) THEN
C
C* Calculate its grid square longitude and latitude extrema
C
              CALL pmesh (jx, jy, px1, py1, kngx, kngy, cdper, kper,
     $                    z1xi, z1xs, z1yi, z1ys)
C
C* Determine the intersection with the current target grid square
C
              CALL pminm (iflag, zsurf,
     $                    p2xi, p2xs, p2yi, p2ys,
     $                    z1xi, z1xs, z1yi, z1ys)
C
C* If intersection is not empty
C
              IF (iflag .EQ. 1) THEN
                  iwco = iwco +1
C
C* Check dimensions
C
                  IF (iwco .GT. iwg) THEN 
                      WRITE(UNIT = nulou,FMT = *)  
     $                    ' WARNING: not enough neighbours space'
                      WRITE(UNIT = nulou,FMT = *) 
     $                    ' ******* '
                      WRITE(UNIT = nulou,FMT = *) 
     $                    ' In pmrho : iwco = ',iwco,'.gt.iwg = ',iwg
                      CALL HALTE('STOP in pmrho')
                    ELSE
C
C* Get index adress and surface overlap for neighbor number iwco
C
                      kto(iwco) = jx + (jy-1) * kngx
                      prho(iwco) = zsurf
                  ENDIF
              ENDIF
          ENDIF
 230    CONTINUE 
 220  CONTINUE
C
C
C*    3. Weight function
C        ---------------
C
C* Sum of the surface values
C
      zsum = 0.
      DO 310 jwg = 1, iwg
        zsum = zsum + prho(jwg)
 310  CONTINUE
C
C* Normalization
C
      zthres = 1.e-6
C
C* If no intersection is found, point is masked in overlapping array
C
      IF (iwco .EQ. 0) THEN
          kmsz2 = kvmsz2
C
C* If at least one intersection is found
C     
        ELSE
          kmsz2 = iwco
C
C* If under threshold
C   
          IF (zsum .LT. zthres) THEN
              DO 320 jwg = 1, iwg
                prho(jwg) = 1. / float(iwco)
 320          CONTINUE
C
C* If above threshold, normalize
C
            ELSE
              DO 330 jwg = 1, iwg
                prho(jwg) = prho(jwg) / zsum
 330          CONTINUE
          ENDIF
      ENDIF
C
C* End of routine
C
      RETURN 
      END
