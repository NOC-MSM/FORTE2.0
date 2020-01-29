      SUBROUTINE extrap (pfild, pmask, pwork, 
     $                   kmask, kxlon, kylat, 
     $                   knbor, cdextmet, cdper, kper, 
     $                   krdwt, knb)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *extrap* - Extrapolation routine
C
C     Purpose:
C     -------
C     Extrapolate field on land from sea values using nearest 
C     neighbors filling.
C
C**   Interface:
C     ---------
C       *CALL*  *extrap (pfild, pmask, pwork, 
C    $                   kmask, kxlon, kylat,
C    $                   knbor, cdextmet, cdper, kper, 
C    $                   krdwt, knb)
C
C     Input:
C     -----
C                pfild    : field to be extrapolated (real 2D)
C                pmask    : mask value (real)
C                pwork    : work array (real 2D)
C                kmask    : mask array (integer 2D)
C                kxlon    : number of longitudes (integer)
C                kylat    : number of latitudes (integer)
C                knbor    : nbr of neighbors for extrapolation (integer)
C                cdextmet : extrapolation method (character)
C                cdper    : grid period.:P-period.,R-region. (character)
C                kper     : number of overlapping grid points (if any)
C                krdwt    : read/write flag for the weights and address
C                knb  : flag to identify appropriate NINENN dataset
C
C     Output:
C     ------
C                pfild    : field extrapolated
C
C     Workspace:
C     ---------
C     zwork, zmask, ix, iy
C
C     Externals:
C     ---------
C     None
C
C     Reference:
C     ---------
C     See OASIS manual (1998)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       1.0       L. Terray      94/01/01  created
C       2.0       L. Terray      95/12/20  modified: new structure
C       2.2       L. Terray      97/12/31  modified: general cleaning
C       2.3       L. Terray      99/03/01  corrected: bug with 
C                                          latitudes fully masked
C       2.3       S. Valcke      99/03/30  add READ/WRITE for NINENN 
C                                          weights
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       S. Valcke      99/10/01  corrected: perodic 
C                                          overlapping grid
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'printing.h'
      INCLUDE 'extrapol.h'
C
C* ---------------------------- Argument declarations -------------------
C
      REAL pfild(kxlon,kylat), pwork(kxlon,kylat)
      INTEGER kmask(kxlon,kylat), kper
      CHARACTER*8 cdextmet, cdper
C
C* ---------------------------- Local declarations ----------------------
C
      REAL zmask(9)
      INTEGER ix(9), iy(9)
      INTEGER iaddress(jpnfn,9,jpgrd)
      REAL zweights(jpnfn,9,jpgrd)
      INTEGER iincre(jpnfn,jpgrd)
      SAVE iaddress, zweights, iincre
      CHARACTER*8 cladress, clweight, clincrem
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      iloop=0
      idoitold=0
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE extrap  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Extrapolation on land with sea values'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      incre = 0
C
C* To avoid problems in floating point tests
C
      zwmsk = pmask - 1.0
C
C
C*    2. Calculating weights and filling masked values 
C        ---------------------------------------------
C
      IF (cdextmet .EQ. 'NINENN') THEN
          IF (nlogprt .GE. 2) THEN          
              CALL prtout
     $        ('NINENN Dataset number for this field is:',knb,1)
          ENDIF
C
C* Define locators
C
          WRITE(clweight,'(''WEIGHTS'',I1)') knb
          WRITE(cladress,'(''ADRESSE'',I1)') knb
          WRITE(clincrem,'(''INCREME'',I1)') knb
C
C
          IF (krdwt .EQ. 1 .AND. .NOT. lweight(knb)) THEN
C
C* For krdwt=1, the weights and addresses are calculated by Oasis
C
C* Initialize iteration number, weight and address variables to zero
              DO 201 jj = 1, kylat
                DO 202 ji = 1, kxlon
                  iind=(jj-1)*kxlon + ji
                  iincre(knb,iind) = 0

                  DO 203 jl = 1, 9
                    iaddress(knb,jl, iind) = 0
                    zweights(knb,jl, iind) = 0.
 203              CONTINUE
 202            CONTINUE
 201          CONTINUE
C
 200          CONTINUE
              incre = incre + 1
              DO 210 jj = 1, kylat
C
C* Test if latitude 1 or kylat or n and n+1 are fully masked 
C
                iinf = iminim (kmask(1, jj),kxlon)
                isup = iminim (kmask(1, jj+1),kxlon)
                IF (jj .eq. 1 .OR. jj .eq. kylat) isup=iinf 
C
C* If yes, set number of neighbors to 3
C
                IF (iinf .EQ. 1 .AND. isup .EQ. 1) THEN
                    ivoisin = MIN0(3,knbor)
                ELSE 
                    ivoisin = knbor
                ENDIF 
                DO 220 ji = 1, kxlon
                  iind=(jj-1)*kxlon + ji
                  pwork(ji,jj) = pfild(ji,jj)
C
C* Case 1:  value is not masked
C
                  IF (pfild(ji,jj) .LT. zwmsk) THEN
                      GOTO 220
                  ELSE
C
C* Case 2:  value at point P is masked
C
C
C  We search over the eight closest neighbors
C
C            j+1  7  8  9
C              j  4  5  6    Current point 5 --> (i,j)
C            j-1  1  2  3
C
C                i-1 i i+1 
C  ix and iy are index arrays for the neighbors coordinates
C
                      inbor = 0
                      ideb = 1
                      ifin = 9
C
C* Fill up ix array
C
                      ix(1) = MAX0 (1,ji - 1)
                      ix(2) = ji
                      ix(3) = MIN0 (kxlon,ji + 1)
                      ix(4) = MAX0 (1,ji - 1)
                      ix(5) = ji
                      ix(6) = MIN0 (kxlon,ji + 1)
                      ix(7) = MAX0 (1,ji - 1)
                      ix(8) = ji
                      ix(9) = MIN0 (kxlon,ji + 1)
C
C* Fill up iy array
C
                      iy(1) = MAX0 (1,jj - 1)
                      iy(2) = MAX0 (1,jj - 1)
                      iy(3) = MAX0 (1,jj - 1)
                      iy(4) = jj
                      iy(5) = jj
                      iy(6) = jj
                      iy(7) = MIN0 (kylat,jj + 1)
                      iy(8) = MIN0 (kylat,jj + 1)
                      iy(9) = MIN0 (kylat,jj + 1)
C
C* Account for periodicity in longitude
C
                      IF (cdper .EQ. 'P') THEN
                          IF (ji .EQ. kxlon) THEN
                              ix(3) = 1 + kper
                              ix(6) = 1 + kper
                              ix(9) = 1 + kper
                          ELSE IF (ji .EQ. 1) THEN
                              ix(1) = kxlon - kper
                              ix(4) = kxlon - kper
                              ix(7) = kxlon - kper
                          ELSE 
                              CONTINUE 
                          ENDIF
                      ENDIF
C
C* Correct latitude bounds if southernmost or northernmost points
C
                      IF (jj .EQ. 1) ideb = 4
                      IF (jj .EQ. kylat) ifin = 6
C
C* Grid not periodic 
C
                          IF (cdper .EQ. 'R') THEN
C* ji = 1
                          IF (ji .EQ. 1) THEN
                              ix(1) = ji
                              ix(2) = ji + 1
                              ix(3) = ji
                              ix(4) = ji + 1
                              ix(5) = ji
                              ix(6) = ji + 1
                          ENDIF 
C* ji = kxlon
                          IF (ji .EQ. kxlon) THEN
                              ix(1) = ji -1
                              ix(2) = ji
                              ix(3) = ji - 1
                              ix(4) = ji
                              ix(5) = ji - 1
                              ix(6) = ji
                          ENDIF
C
C* Latitude index in both cases
C
                          IF (ji .EQ. 1 .OR. ji .EQ. kxlon) THEN 
                              ideb = 1
                              ifin = 6
                              iy(1) = MAX0 (1,jj - 1)
                              iy(2) = MAX0 (1,jj - 1)
                              iy(3) = jj
                              iy(4) = jj
                              iy(5) = MIN0(kylat,jj + 1)
                              iy(6) = MIN0(kylat,jj + 1)
C
C* Correct latitude bounds if southernmost or northernmost points
C
                              IF (jj .EQ. 1) ideb = 3
                              IF (jj .EQ. kylat) ifin = 4
                          ENDIF
                      ENDIF 
C
C* Find unmasked neighbors
C
                      DO 230 jl = ideb, ifin
                        zmask(jl) = 0.
                        ilon = ix(jl)
                        ilat = iy(jl)
                        IF (pfild(ilon,ilat) .LT. zwmsk) THEN
                            zmask(jl) = 1.
                            inbor = inbor + 1
                        ENDIF
 230                  CONTINUE
C
C* Not enough points around point P are unmasked; interpolation on P 
C  will be done in a future call to extrap.
C
                      IF (inbor .LT. ivoisin) THEN
                          GOTO 220
                      ELSE
C
C* Some points around P are not masked so we use them to extrapolate
C* and define the iteration number, weight and address variables
C
                          pwork(ji,jj) = 0.
                          iincre(knb, iind) = incre
                          DO 240 jl = ideb, ifin
                           ilon = ix(jl)
                           ilat = iy(jl)
                           pwork(ji,jj) = pwork(ji,jj)
     $                        + pfild(ilon,ilat) * zmask(jl)
     $                        / FLOAT(inbor)
                           iaddress(knb,jl,iind)=(ilat-1)*kxlon+ilon
                           zweights(knb,jl,iind)=zmask(jl)/FLOAT(inbor)
 240                      CONTINUE
C
                      ENDIF
                  ENDIF
 220            CONTINUE
 210          CONTINUE
C
C*    3. Writing back unmasked field in pfild and writing weights to file
C        ----------------------------------------------------------------
C
C* pfild then contains:
C     - Values which were not masked
C     - Interpolated values from the inbor neighbors
C     - Values which are not yet interpolated
C
              idoit = 0
              DO 310 jj = 1, kylat
                DO 320 ji = 1, kxlon
                  IF (pwork(ji,jj) .GT. zwmsk) THEN
                      idoit = idoit + 1
                  ENDIF
                  pfild(ji,jj) = pwork(ji,jj)
 320            CONTINUE
 310          CONTINUE
C
              if (idoit.eq.idoitold) iloop=iloop+1
              idoitold=idoit
              if (iloop.eq.2) then
                iloop=0
                knbor=knbor-1
              end if
              IF (idoit .ne. 0) GOTO 200
C
C* Write weights, addresses and iteration numbers in file
C
C* Weights
C
              CALL locwrite (clweight, zweights, jpnfn*9*kxlon*kylat,
     $                   nulgn, iflag)
              IF (iflag .NE. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in writing on UNIT = ', nulgn
                  WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', clweight
                  CALL HALTE ('Stop in extrap writing weitghts')
              ENDIF
C 
C* Adresses
C
              CALL locwrint (cladress, iaddress, jpnfn*9*kxlon*kylat,
     $                   nulgn, iflag)
              IF (iflag .NE. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in writing on UNIT = ', nulgn
                  WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', cladress
                  CALL HALTE ('Stop in extrap writing addresses')
              ENDIF
C
C* Iteration numbers
C
              CALL locwrint (clincrem, iincre, jpnfn*kxlon*kylat,
     $                   nulgn, iflag)
              IF (iflag .NE. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in writing on UNIT = ', nulgn
                  WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', clincrem
                  CALL HALTE ('Stop in extrap writing iteration number')
              ENDIF
C
C* Printing
C
              IF (nlogprt .GE. 2) THEN
                  CALL prtout('Wrote weights,addresses and iteration
     $ numbers on unit = ', nulgn, 1)
              ENDIF
          ENDIF
C
          IF (krdwt .EQ. 0 .AND. .NOT. lweight(knb)) THEN
C
C
C*    4. Reading weights and filling masked values 
C        ---------------------------------------------
C
C* For krdwt=0, the weights, addresses and iteration numbers are read by Oasis
C
              CALL locread (clweight, zweights, jpnfn*9*kxlon*kylat, 
     $                   nulgn, iflag)
              IF (iflag .NE. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in reading on UNIT = ', nulgn
                  WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', clweight
                  CALL HALTE ('Stop in extrap reading weights')
              ENDIF
C
              CALL locrint (cladress, iaddress, jpnfn*9*kxlon*kylat,
     $                   nulgn, iflag)
              IF (iflag .NE. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in reading on UNIT = ', nulgn
                  WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', cladress
                  CALL HALTE ('Stop in extrap reading addresses')
              ENDIF
C
              CALL locrint (clincrem, iincre, jpnfn*kxlon*kylat,
     $                   nulgn, iflag)
              IF (iflag .NE. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in reading on UNIT = ', nulgn
                  WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', clincrem
                  CALL HALTE('Stop in extrap reading iteration numbers')
              ENDIF
          ENDIF
C
C* Extrapolation
C
          IF (krdwt .EQ. 0 .OR. lweight(knb)) THEN
 400          CONTINUE
              incre = incre + 1
              DO 410 jj = 1, kylat
                DO 420 ji = 1, kxlon
                  iind=(jj-1)*kxlon + ji
                  pwork(ji,jj) = pfild(ji,jj)
C
C* Case 1:  value is not masked
C
                  IF (pfild(ji,jj) .LT. zwmsk) THEN
                      GOTO 420
                    ELSE
C
C* Case 2:  value at point P is maskedC
                      IF (iincre(knb,iind) .EQ. incre) THEN
                          pwork(ji,jj) = 0.
                          DO 250 jl = 1,9
                            IF (zweights(knb,jl,iind).EQ.0.) GO TO 250
                            idivi=iaddress(knb, jl, iind)/kxlon
                            imult=idivi*kxlon
                            IF (iaddress(knb,jl, iind) .EQ. imult) THEN
                                ilat=idivi
                                ilon=kxlon
                              ELSE
                                ilat=idivi+1
                                ilon=iaddress(knb,jl, iind)-imult
                            ENDIF
                            pwork(ji,jj) = pwork(ji,jj)
     $                          + pfild(ilon,ilat)
     $                          *zweights(knb,jl,iind)
 250                      CONTINUE
                      ENDIF
                  ENDIF
 420            CONTINUE
 410          CONTINUE
C
C*    5. Writing back unmasked field in pfild
C        ------------------------------------
C
C* pfild then contains:
C     - Values which were not masked
C     - Interpolated values from the inbor neighbors
C     - Values which are not yet interpolated
C
              idoit = 0
              DO 510 jj = 1, kylat
                DO 520 ji = 1, kxlon
                  IF (pwork(ji,jj) .GT. zwmsk) THEN
                      idoit = idoit + 1
                  ENDIF
                  pfild(ji,jj) = pwork(ji,jj)
 520            CONTINUE
 510          CONTINUE
              IF (idoit .ne. 0) GOTO 400
          ENDIF
C
C* Printing
C
          IF (nlogprt .GE. 2) THEN
              CALL prtout
     $        ('Number of extrapolation steps incre =', incre, 1)
c             CALL FLUSH(nulou)
          ENDIF
      ENDIF 
C
C* Put flag indicating that weights have been read or calculated to 1
C
      lweight(knb)= .TRUE.
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine extrap ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
