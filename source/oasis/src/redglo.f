      SUBROUTINE redglo 
     $    (pzgg, pzgr, kredu, kinip, klon, klat, 
     $     kmask, kland, ktronca, pmask, cdmsk)
C*****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *redglo* - Global gaussian grid transformation: reduced --> full grid
C
C     Purpose:
C     -------
C     Linear interpolation of field on reduced grid to full grid 
C
C     ***NOTE***
C     ----------
C     The flag cdmsk asks for extrapolation or not on the reduced
C     gaussian grid. If cdmsk  = SEALAND sea values are extended
C     to continental areas using the reduced grid sea-land mask.
C     If cdmsk = LANDSEA, the opposite is performed. If cdmsk = 
C     NOEXTRAP, THEN no extrapolation is performed.
C
C**   Interface:
C     ---------
C       *CALL*  *redglo (pzgg, pzgr, kredu, kinip, klon, klat,
C                        kmask, kland, ktronca, pmask)
C
C     Input:
C     -----
C                pzgg    : field on reduced grid (real 2D)
C                kredu   : number of points on reduced grid
C                kinip   : number of longitudes per latitude value (integer 1D)
C                klon    : number of longitudes of global grid
C                klat    : number of latitudes of both grids
C                pmask   : reduced grid mask value
C                ktronca : truncature of gaussian grid
C                cdmsk   : extrapolation flag
C  
C     Output:
C     ------
C                pzgg    : field on global grid  (real 2D)
C                kmask   : mask on reduced grid (integer 1D)
C
C     Workspace:
C     ---------
C                pzgr    : work array to store field on reduced grid (real 1D)
C                kland   : number of masked points per latitude circle on
C                          reduced grid (integer 1D)
C
C     External:
C     --------
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
C       2.0       L. Terray      95/10/10  modified: new structure
C       2.1       L. Terray      95/11/11  modified: choice of extrapolation
C       2.3       S. Valcke      99/03/29  modified: error message for maskr
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'gauss.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      REAL pzgg(klon,klat), pzgr(klon*klat)
      INTEGER kmask(kredu), kland(klat), kinip(klat)
      CHARACTER*8 cdmsk
C
C* ---------------------------- Local declarations -------------------
C
      CHARACTER*8 clname
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
     $    '           ROUTINE redglo  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Go from reduced to global gaussian grid'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      iflag = 0
C
C* Zeroes work array
C
      CALL szero(pzgr,klon*klat)
C
C* Zeroes some argument arrays
C
      CALL izero(kmask,kredu)
      CALL izero(kland,klat)
      zmask = pmask - 1.0
C
C* Read appropriate mask for reduced grid
C
      WRITE(clname,'(''MSKRD'',I3.3)') ktronca
      iunit = nulrd
      CALL locrint(clname, kmask, kredu, iunit, iflag)
      IF (iflag .NE. 0) THEN
          CALL prtout('WARNING: problem in reading unit =', iunit, 2)
          CALL prcout('Error in reading field linked to string =',
     $                clname, 1)
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Note that since version 2.3, the locator for the'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'reduced mask has to be MSKRDxxx WHERE xxx' 
                  WRITE (UNIT = nulou,FMT = *) 
     $            'is half the number of latitude lines.'
      ENDIF
C
C* Assign reduced grid values to array pzgr
C
      DO 110 jj = 1, klat
        DO 120 ji = 1, klon
          pzgr(klon*(jj-1)+ji) = pzgg(ji,jj)
 120    CONTINUE 
 110  CONTINUE
C
C* Test if extrapolation is required
C
      IF (cdmsk .EQ. 'SEALAND' .OR. cdmsk .EQ. 'LANDSEA') THEN 
C
C
C*    2. Check if there is any sea point and mask land points
C        ----------------------------------------------------
C
C* First get number of masked points on each latitude circle
C
          indice = 0
C* Extrapolation of land values from sea values, we set mask equal 1
          IF (cdmsk .EQ. 'SEALAND') imask = 1
C* Extrapolation of sea values from land values, we set mask equal 0
          IF (cdmsk .EQ. 'LANDSEA') imask = 0
          DO 210 jk = 1, klat
            DO 220 ji = 1, kinip(jk)
              IF (kmask(indice + ji) .EQ. imask) THEN
                  kland(jk) = kland(jk) + 1
              ENDIF
 220        CONTINUE
            indice = indice + kinip(jk)
 210      CONTINUE
C
C* Then fill up continental or sea values unless all points are land or sea
C
          indice = 0
          DO 230 jk = 1, klat
            IF (kland(jk) .EQ. kinip(jk)) GO TO 250
            DO 240 ji = 1, kinip(jk)
              IF (kmask(indice + ji) .EQ. imask) 
     $            pzgr(indice + ji) = pmask
 240        CONTINUE
 250        indice = indice + kinip(jk)
 230      CONTINUE
C
C
C*    3. Assign sea values to land points or land values to sea points
C        -------------------------------------------------------------
C
          indice = 0
          DO 310 jk = 1, klat
            IF (kland(jk) .EQ. kinip(jk)) GOTO 315
            DO 320 ji = 1, kinip(jk)
              inow = indice + ji
              IF (pzgr(inow) .GT. zmask) THEN
                  iinf = indice + 1
                  isup = indice + kinip(jk)
                  ieast = inow + 1
                  iwest = inow - 1
                  icont = 0
 325              CONTINUE
                  IF (ieast .GT. isup) ieast = ieast - kinip(jk)
                  IF (iwest .LT. iinf) iwest = iwest + kinip(jk)
                  IF (pzgr(ieast) .LT. zmask .and.
     $                pzgr(iwest) .LT. zmask) THEN
                      pzgr(inow) = (pzgr(ieast) + pzgr(iwest))/2.
                  ELSEIF (pzgr(ieast) .GT. zmask .and.
     $                    pzgr(iwest) .LT. zmask) THEN
                      pzgr(inow) = pzgr(iwest)
                  ELSEIF (pzgr(ieast) .LT. zmask .and.
     $                    pzgr(iwest) .GT. zmask) THEN
                      pzgr(inow) = pzgr(ieast)
                  ELSE
                      icont = icont + 1
                      ieast = ieast + 1
                      iwest = iwest - 1
                      if (icont .GT. kinip(jk)) 
     $                    CALL HALTE ('STOP in redglo')
                      GOTO 325
                  ENDIF
              ENDIF
 320        CONTINUE
 315        indice = indice + kinip(jk)
 310      CONTINUE
C* No extrapolation case
        ELSE IF (cdmsk .EQ. 'NOEXTRAP') THEN
          CONTINUE 
      ENDIF 
C
C
C*    2. Reduced to full linear interpolation  
C        ------------------------------------
C
C* Interpolate from reduced to global
C 
      indice = 0
      DO 270 jk = 1, klat
        DO 280 ji = 1, klon
          zxi = 1 + ((ji - 1) * kinip(jk)) / FLOAT(klon)
          im = INT(zxi)
          zdx = zxi - im
          im = 1 + MOD(im + kinip(jk) - 1,kinip(jk))
          ip = 1 + MOD(im,kinip(jk))
          pzgg(ji,jk) = pzgr(indice + im) * (1.-zdx) 
     $                + pzgr(indice + ip) * zdx
 280    CONTINUE
        indice = indice + kinip(jk)
 270  CONTINUE
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine redglo ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
