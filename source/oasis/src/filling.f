      SUBROUTINE filling (pfild, pworka, pworkb, pworkc,
     $                    plon, plat, klon, klat, kmask, kmesh,
     $                    kunit, cdfic, cdmet)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *filling* - Fill up (and smooth or not) regional field with climatology 
C
C
C     Purpose:
C     -------
C     Performs smooth or abrupt blending of a regional data set with a  
C     global one for a Sea Surface Temperature or Sea Ice Extent field.
C     The frequency of the global set can be interannual monthly, 
C     climatological monthly or yearly. Sea-ice transitions are treated
C     specifically: the ice is supposed to form or disappear on the first 
C     day of the month.
C
C     N.B : The SST climatology must be in celsius. If not
C           one has to change the value of local variable zmerg
C
C**   Interface:
C     ---------
C       *CALL*  *filling(pfild, pworka, pworkb, pworkc, klon, klat, 
C                        kmask, kmesh, kunit, cdfic, cdmet)*
C
C     Input:
C     -----
C                pfild  : field to be completed (real 2D) 
C                plon   : grid longitude array (real 2D)
C                plat   : grid latitude array (real 2D)
C                klon   : number of longitude (integer)
C                klat   : number of latitude (integer)
C                kmask  : mask array (integer 2D)
C                kmesh  : overlapped neighbors array (integer 2D)
C                kunit  : climatological data filename logical unit (integer)
C                cdfic  : climatological data filename  (character)
C                cdmet  : filling method (character)
C
C     Output:
C     ------
C                pfild  : field completed (real 2D) 
C
C     Workspace:
C     ---------
C     These are work arrays passed as arguments:
C     pworka, pworkb, pworkc
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
C       1.0       L. Terray      94/01/01  created
C       1.1       L. Terray      94/10/01  modified: lecture of climagrd
C       2.0beta   L. Terray      95/12/10  modified: new structure
C       2.0       L. Terray      96/02/01  modified: change on rewind()
C       2.1       L. Terray      96/09/09  modified: flux correction term
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'parameter.h'
      INCLUDE 'unit.h'
      INCLUDE 'analysis.h'
      INCLUDE 'coast.h'
      INCLUDE 'calendar.h'
      INCLUDE 'smooth.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations----------------------
C
      REAL pfild(klon, klat), plon(klon, klat), plat(klon, klat)
      REAL pworka(klon, klat),  pworkb(klon, klat) 
      REAL pworkc(klon, klat)
      INTEGER kmask(klon,klat), kmesh(klon, klat)
      CHARACTER*8 cdmet, cdfic
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*3 clsmooth, clfield
      CHARACTER*2 clclim
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN 
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE filling  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $     ' Filling and smoothing of field with climatology'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* initialize error flag for error routine
C
      iflag = 0
C
C* Get smoothing and time info
C
      clsmooth = cdmet(1:3)
      clfield = cdmet(4:6)
      clclim = cdmet(7:8)
C
C* Get congelation temperature for SST and SIE treatment.
C
      zmerg = -1.93
C
C
C*    2. Getting climatological or interannual field
C        -------------------------------------------
C
C* Monthly climatology
C
      IF (nlogprt .GE. 2) THEN
          CALL prcout
     $    ('Complementary data file is file =', cdfic, 1)
      ENDIF
      IF (clclim .EQ. 'SE' .OR. clclim .EQ. 'MO') THEN 
          IF (nlogprt .GE. 2) THEN
              WRITE (UNIT = nulou,FMT = *) ' '
          ENDIF
C
C* Monthly climatology
C
          IF (clclim .EQ. 'SE') THEN 
              IF (nlogprt .GE. 2) THEN
                  CALL prtout
     $            ('Monthly records skipped in climatology file 
     $            nsrec =', nsrec, 1)
              ENDIF
C
C* Read the right records
C
              REWIND kunit 
              IF (nsrec .eq. 1) THEN
                  READ (kunit) pworka
                  READ (kunit) pworkb
                ELSEIF (nsrec .gt. 1 .and. nsrec .lt. 12) THEN
                  DO 210 ji = 1, nsrec - 1
                    READ (kunit) pworka
 210              CONTINUE
                  READ (kunit) pworka
                  READ (kunit) pworkb
                ELSEIF (nsrec .eq. 12 .or. nsrec .eq. 0) THEN
                  DO 220 ji = 1, 11
                    READ (kunit) pworka
 220              CONTINUE
                  READ (kunit) pworka
                  REWIND kunit 
                  READ (kunit) pworkb
                ELSE
                  WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' ===>>> : wrong value for nsrec '
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' ======                   ===== '
                  WRITE (UNIT = nulou,FMT = *) ' '
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' Variable nsrec = ',nsrec
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' We STOP !!! Check value for variable nsrec '
                  CALL HALTE ('STOP in filling')
              ENDIF
C
C* Monthly interannual data
C
            ELSE IF(clclim .EQ. 'MO') THEN 
                IF (nlogprt .GE. 2) THEN
                    CALL prtout
     $                  ('Monthly records skipped in interannual file 
     $                  nmrec =', nmrec, 1)
                ENDIF
C
C* Read the right records
C
              REWIND kunit
              IF (nmrec .eq. 0) THEN
                  READ (kunit) pworka
                  READ (kunit) pworkb
                ELSEIF (nmrec .ge. 1) THEN
                  DO 230 ji = 1, nmrec
                    READ (kunit) pworka
 230              CONTINUE
                  READ (kunit) pworka
                  READ (kunit) pworkb
              ENDIF
          ENDIF  
C
C* Get all coefficients for time linear interpolation
C
          zpoid1 = FLOAT(njtwo - njnow) / FLOAT(njtwo - njone)
          zpoid2 = 1.0 - zpoid1
          zpoib1 = FLOAT(ndtwo - njnow) / FLOAT(ndtwo - ndone)
          zpoib2 = 1.0 - zpoib1
C
C* Get climatological field for current day
C
C* Specific treatment for SST and SIE due to freezing temperature
C  In both cases, one reads the temperature file and looks at 
C  sea-ice transitions. One makes the assumption that the ice is
C  formed or disappears on the first of the month.
C
          DO 240 jj = 1, klat
            DO 250 ji = 1, klon
C
C* General case
C
              pworkc(ji,jj) = zpoid1 * pworka(ji,jj) +
     $                        zpoid2 * pworkb(ji,jj)
C
C* If field is SST
C
              IF (clfield .EQ. 'SST') THEN
C
C* Sea Ice to Sea
C
                  IF (pworka(ji,jj) .LE. zmerg .AND. 
     $                pworkb(ji,jj) .GT. zmerg) THEN
                      IF (njnow .GT. 15) THEN
                          CONTINUE
                      ELSE
                          pworkc(ji,jj) = zmerg * zpoib1 +
     $                                    pworkb(ji,jj) * zpoib2
                      ENDIF 
                  ENDIF
C
C* Sea to Sea Ice
C
                  IF (pworka(ji,jj) .GT. zmerg .AND. 
     $                pworkb(ji,jj) .LE. zmerg) THEN
                      IF (njnow .GT. 15) THEN
                          pworkc(ji,jj) = pworka(ji,jj) * zpoib1 +
     $                                    zmerg * zpoib2
                      ENDIF
                  ENDIF
C
C* Sea to Sea
C
                  IF (pworka(ji,jj) .GT. zmerg .AND. 
     $                pworkb(ji,jj) .GT. zmerg) THEN
                      pworkc(ji,jj) = zpoid1 * pworka(ji,jj) +
     $                                zpoid2 * pworkb(ji,jj)
                  ENDIF
              ENDIF
C
C* If field is SIE
C 
              IF (clfield .EQ. 'SIE') THEN             
C
C* Sea Ice to Sea Ice
C
                  IF (pworka(ji,jj) .LE. zmerg .AND. 
     $                pworkb(ji,jj) .LE. zmerg) THEN
                      pworkc(ji,jj) = 1.0
                  ENDIF
C
C* Sea Ice to Sea
C
                  IF (pworka(ji,jj) .LE. zmerg .AND. 
     $                pworkb(ji,jj) .GT. zmerg) THEN
                      IF (njnow .gt. 15) THEN
                          pworkc(ji,jj) = 1.0
                        ELSE
                          pworkc(ji,jj) = 0.0
                      ENDIF
                  ENDIF
C
C* Sea to Sea Ice
C
                  IF (pworka(ji,jj) .GT. zmerg .AND. 
     $                pworkb(ji,jj) .LE. zmerg) THEN
                      IF (njnow .gt. 15) THEN
                          pworkc(ji,jj) = 0.0
                      ELSEIF (njnow .eq. 1) THEN
                          pworkc(ji,jj) = 2.0
                      ELSE
                          pworkc(ji,jj) = 1.0
                      ENDIF
                  ENDIF
C
C* Sea to Sea
C
                  IF (pworka(ji,jj) .GT. zmerg .AND. 
     $                pworkb(ji,jj) .GT. zmerg) THEN
                      pworkc(ji,jj) = 0.0
                  ENDIF
              ENDIF 
 250        CONTINUE
 240      CONTINUE
C
C* Annual climatology
C
      ELSE IF (clclim .EQ. 'AN') THEN
          READ (kunit) pworkc
      ELSE 
          CALL prcout ('This type of climatology cannot be used  -
     $                 clim = ', clclim, 1)
      ENDIF
C
C* Blending and smoothing
C
C* Storing of climatology field interpolated in intermediate array pworka
C
      DO 260 jj = 1, klat
        DO 270 ji = 1, klon
          pworka(ji,jj) = pworkc(ji,jj)
 270    CONTINUE
 260  CONTINUE
C* SST case
      IF (clfield .EQ. 'SST') THEN
C
C* Put interpolated values in intermediate array if:
C     - the point is an atmosphere sea point
C     - there is at least one underlying ocean sea point
C
          DO 280 jj = 1, klat
            DO 290 ji = 1, klon
              IF (kmesh(ji,jj) .NE. 0 .AND. kmask(ji,jj) .EQ. 0) THEN
                  pworka(ji,jj) = pfild(ji,jj)
              ENDIF
 290        CONTINUE
 280      CONTINUE
C
C* If needed, do coast mismatch correction
C  --> Special treatment for coastal points where there is a
C  --> a mismatch between ocean and atmosphere land sea masks
C  --> The array npcoast is calculated in routine coasts.f
C  --> icoor and jcoor are externally defined functions
C
          IF(nfcoast .EQ. 1) THEN
C* If first iteration, initialize coast mismatch data
              IF (lcoast) THEN
                  itmsq = 1
                  CALL coasts (plon, plat, kmask, itmsq,
     $                         klon, klat, kmesh)
C* Switch off flag
                  lcoast = .FALSE.
              ENDIF
              IF (nlogprt .GE. 2) THEN
                  WRITE (UNIT = nulan,FMT = *) ' '
                  WRITE (UNIT = nulan,FMT = *) 
     $            '               * Model run date * '
                  WRITE (UNIT = nulan,FMT = *) 
     $            '               * -------------- * '
                  WRITE (UNIT = nulan,FMT = *) ' '
                  WRITE (UNIT = nulan,FMT = *) 
     $            '        Year  --->>> ',nanow
                  WRITE (UNIT = nulan,FMT = *) 
     $            '       Month  --->>> ',nmnow
                  WRITE (UNIT = nulan,FMT = *) 
     $            '         Day  --->>> ',njnow
                  WRITE (UNIT = nulan,FMT = *) ' '
                  WRITE (UNIT = nulan,FMT = *) 
     $            '      Correct for coast mismatch '
                  WRITE (UNIT = nulan,FMT = *) 
     $            '      ************************** '
                  WRITE (UNIT = nulan,FMT = *) ' '
              ENDIF
              DO 291 jc = 1, ncoast
                iic = icoor(npcoast(jc,1), klon)
                ijc = jcoor(npcoast(jc,1), klon)
                ztmptot = 0.
                DO 292 jn = 3, 2 + npcoast(jc,2)
                  iiic = icoor(npcoast(jc,jn), klon)
                  ijjc = jcoor(npcoast(jc,jn), klon)
                  ztmptot = ztmptot + pfild(iiic,ijjc)
 292            CONTINUE
C
C* Print old and new values
C
                IF (nlogprt .GE. 2) THEN
                    WRITE (UNIT = nulan,FMT = *) 
     $              ' Resetting SST at point (i,j)= ',iic,ijc
                    WRITE (UNIT = nulan,FMT = *) 
     $              ' Old SST value : SST = ', pfild(iic,ijc)
                    WRITE (UNIT = nulan,FMT = *) 
     $              ' Clim SST value : SST = ', pworka(iic,ijc)
                ENDIF
C
C* Get new value
C
                pfild(iic,ijc) = ztmptot / float(npcoast(jc,2))
                IF (nlogprt .GE. 2) THEN
                    WRITE (UNIT = nulan,FMT = *) 
     $              ' New SST value : SST = ', pfild(iic,ijc)
                ENDIF
C
C* Copy new value into intermediate array
C
                pworka(iic,ijc) = pfild(iic,ijc)
 291          CONTINUE  
          ENDIF              
C
C* SIE case : we assign the new field to climatology i.e we do nothing
C
        ELSE IF (clfield .EQ. 'SIE') THEN
          CONTINUE 
C
C* General case: put interpolated value if there is an
C                underlying grid point (whether land or sea)
C
        ELSE  
          DO 293 jj = 1, klat
            DO 294 ji = 1, klon
              IF (kmesh(ji,jj) .NE.  0) THEN
                  pworka(ji,jj) = pfild(ji,jj)
              ENDIF 
 294        CONTINUE
 293      CONTINUE
      ENDIF
C
C
C*    3. Do the smoothing on  borders (only for SST)
C        ----------------------------
C
      IF (clfield .EQ. 'SST' .AND. clsmooth .EQ. 'SMO') THEN 
C
C* First South and North borders
C
          DO 310 jk = nsltb, nslte
            DO 320 ji = 1, klon
              IF (kmesh(ji,jk) .NE. 0 .AND. kmask(ji,jk) .EQ. 0) THEN
                  pworka(ji,jk) = qalfa * (jk - nsltb) * pfild(ji,jk)
     $                + (1.-qalfa * (jk - nsltb)) * pworkc(ji,jk)
              ENDIF
 320        CONTINUE
 310      CONTINUE
          DO 330 jk = nnltb, nnlte, -1
            DO 340 ji = 1, klon
              IF (kmesh(ji,jk) .NE. 0 .AND. kmask(ji,jk) .EQ. 0) THEN
                  pworka(ji,jk) = qalfa * (nnltb - jk) * pfild(ji,jk)
     $                +  (1.-qalfa * (nnltb - jk)) * pworkc(ji,jk)
              ENDIF
 340        CONTINUE
 330      CONTINUE
C
C* Western border
C
          DO 350 jj = nslte + 1, nnlte - 1
            DO 360 ji = 1, nwlgmx
              IF (kmesh(ji,jj) .EQ. 0 .AND. 
     $            kmesh(ji + 1,jj) .NE. 0) THEN
                  DO 370 jk = 1, nliss
                    pworka(ji + jk - 1,jj) = qbeta*(jk - 1) *
     $                                       pfild(ji + jk - 1,jj) +
     $                                       pworkc(ji + jk - 1,jj) *
     $                                       (1.-qbeta*(jk - 1))
 370              CONTINUE
              ENDIF
 360        CONTINUE
 350      CONTINUE
C
C* Eastern border
C
          DO 380 jj = nslte + 1, nnlte - 1
            DO 390 ji = klon, nelgmx, -1
              IF (kmesh(ji,jj) .EQ. 0 .AND. 
     $            kmesh(ji - 1,jj) .NE. 0) THEN
                  DO 391 jk = 1, nliss
                    pworka(ji - jk + 1,jj) = qbeta*(jk - 1) *
     $                                       pfild(ji - jk + 1,jj) +
     $                                       pworkc(ji - jk + 1,jj) *
     $                                       (1.-qbeta*(jk - 1))
 391              CONTINUE 
              ENDIF
 390        CONTINUE
 380      CONTINUE 
C
C* Calculate and write flux correction term cfldcor on unit nlucor
C
          CALL prcout('Calculate flux correction term', cfldcor, 1)
          CALL szero(pworkb,klon*klat)
          DO 392 jj = 1, klat
            DO 393 ji = 1, klon
C
C* The correction term is estimated only for atmosphere sea points with 
C  underlying ocean sea points
C
              IF (kmesh(ji,jj) .NE. 0 .AND. kmask(ji,jj) .EQ. 0) THEN
                  pworkb(ji,jj) = pfild(ji,jj) - pworka(ji,jj)
              ENDIF 
 393        CONTINUE
 392      CONTINUE
C
C* Rewind unit nlucor and write
C
          IF (nlogprt .GE. 2) THEN
              CALL prtout('Write it on logical unit ', nlucor, 2)
          ENDIF
          REWIND nlucor
          CALL locwrite(cfldcor, pworkb, klon*klat, nlucor, iflag)
          IF (iflag .NE. 0) THEN 
              CALL prcout
     $            ('WARNING: problem in writing field',
     $            cfldcor, 1)
              CALL prtout
     $            ('Error in writing logical unit', nlucor, 2)
              CALL HALTE('STOP in filling')
          ENDIF 
      ENDIF 
C
C* Assign intermediate array pworka values to final array pfild
C
      DO 394 jj = 1, klat
        DO 395 ji = 1, klon
          pfild(ji,jj) = pworka(ji,jj)
 395    CONTINUE
 394  CONTINUE
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine filling ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
