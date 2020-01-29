      SUBROUTINE preproc (kindex, kfield)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *preproc* - preprocess routine
C
C
C     Purpose:
C     -------
C     Do the field preprocessing
C
C**   Interface:
C     ---------
C       *CALL*  *preproc (kindex, kfield)*
C
C     Input:
C     -----
C                kindex : field identificator array (integer 1D)
C                kfield : number of fields for current iteration (integer)
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C                zcocoef : additional field coefficients for correct (real 1D)
C                inip    : array for reduced grid data for redglo (integer 1D)
C                clcofld : additional field names for correct (character 1D)
C                clcofic : array to handle data file names (character 1D)
C                iunit   : array to handle I/O units of data files (integer 1D)
C
C     Externals:
C     ---------
C     correct, extrap, extraw, invert, masq, redglo, chkfld
C
C     Reference:
C     ---------
C     See OASIS manual (1998)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      95/09/01  created
C       2.1       L. Terray      96/09/25  modified: Call to chkfld
C       2.2       L. Terray      97/12/16  Added: new extrapolation
C                                          and change call to extrap
C       2.3       L. Terray      99/03/01  modified: call to extrap
C       2.3       S. Valcke      99/03/16  modified for T213 and T319
C       2.3       S. Valcke      99/03/16  modified for T213 and T319
C       2.3       S. Valcke      99/03/26  changed troncature for number of 
C                                          latitude between equator and pole
C       2.3       S. Valcke      99/03/30  changed arguments in CALL to extrap
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       L. Terray      99/09/15  changed periodicity variables
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'field.h'
      INCLUDE 'string.h'
      INCLUDE 'analysis.h'
      INCLUDE 'memory.h'
      INCLUDE 'gauss.h'
      INCLUDE 'label.h'
      INCLUDE 'extrapol.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      INTEGER kindex(kfield)
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER inip(320), iunit(jpcomb)
      REAL zcocoef(jpcomb)
      CHARACTER*8 clxordbf, clyordbf, clextmet, clname, clmsk, clper
      CHARACTER*8 clcofic(jpcomb), clcofld(jpcomb), clfic, clstrg
      CHARACTER*32 clabel
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE preproc  -  Level 1'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Preprocessing of coupling fields'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Zeroes work array
C
      CALL szero (work,jpmax)
      CALL izero (nwork,jpmax)
C
C
C*    2. Do the job
C        ----------
C
      DO 210 jf = 1, kfield
C
C* Assign local variables
C
        ifield = kindex(jf)
        ilabel = numlab(ifield)
        clname = cnaminp(ifield)
        clabel = cfldlab(ilabel)
        iadrold = nadrold(ifield)
        isizold = nsizold(ifield)
        ilonbf = nlonbf(ifield)
        ilatbf = nlatbf(ifield)
        iintflx = nintflx(ifield)
C
C* Print field name
C
        IF (nlogprt .GE. 1) THEN
            CALL prcout('Treatment of field :', clname, 2)
        ENDIF
C
C* - Do preprocessing analysis
C
        DO 220 ja = 1, ntrans(ifield)
          IF (canal(ja,ifield) .EQ. 'MASK') THEN
C
C* --->>> Mask
C
              zmskval = amskval(ifield)
              CALL masq (fldold(iadrold), isizold, zmskval,
     $                   mskold(iadrold))
C
C* --->>> Invert
C
            ELSE IF (canal(ja,ifield) .EQ. 'INVERT') THEN 
              clxordbf = cxordbf(ifield)
              clyordbf = cyordbf(ifield)
              CALL invert (fldold(iadrold), ilonbf,
     $                     ilatbf, clxordbf, clyordbf)
C
C* --->>> Checkin: perform basic checks on input field
C
            ELSE IF (canal(ja,ifield) .EQ. 'CHECKIN') THEN 
              CALL chkfld(clname, clabel, 
     $            fldold(iadrold), mskold(iadrold), surold(iadrold),
     $            isizold, ilonbf, iintflx)
C
C* --->>> Flux correction
C
            ELSE IF (canal(ja,ifield) .EQ. 'CORRECT') THEN
C
C* Assign local variables to main field coefficient
C
              zfldcoef = afldcoef(ifield)
C
C* Get loop index to read additional fields, coefficients, filenames
C  and related logical units
C
              icofld = ncofld(ifield)
              DO 230 jc = 1, icofld
                clcofld(jc) = ccofld(jc,ifield)
                zcocoef(jc) = acocoef(jc,ifield)
                clcofic(jc) = ccofic(jc,ifield)
                iunit(jc)   = nludat(jc,ifield)
 230          CONTINUE
C
C* Zero work array
C
              CALL szero (work, jpmax)
C
C* Do the job
C 
              CALL correct (fldold(iadrold), isizold,
     $                      zfldcoef, icofld, zcocoef(1),
     $                      work(1), iunit(1), clcofic(1), 
     $                      clcofld(1))
C
C* --->>> Extrap
C
            ELSE IF (canal(ja,ifield) .EQ. 'EXTRAP') THEN
              clextmet = cextmet(ifield)
              ineibor = neighbor(ifield)
C
C* 8-nearest neighbors extrapolation
C
              IF (clextmet .EQ. 'NINENN') THEN 
                  zmskval = amskval(ifield)
                  clper = csper(ifield)
                  iper = nosper(ifield)
C
C* Zero work array
C
                  CALL szero (work, jpmax)
C
C* Do it now
C
                  CALL extrap (fldold(iadrold), zmskval, work(1),
     $                         mskold(iadrold), ilonbf, ilatbf, 
     $                         ineibor, clextmet, clper, iper,  
     $                         niwtn(ifield), nninnfl(ifield)) 
C
C* N-weighted neighbors extrapolation
C
                ELSE IF (clextmet .EQ. 'WEIGHT') THEN 
                  clfic = cgrdext(ifield)
                  ilun  = nluext(ifield)
                  iloc  = nextfl(ifield)
                  clstrg = cficbf(ifield)//cficbf(ifield)
                  ipdeb = (nextfl(ifield)-1)*jpext*jpgrd+1
C
C* Do it now
C
                  CALL extraw (fldold(iadrold), mskold(iadrold),
     $                isizold, clfic, ilun, clstrg, iloc,  
     $                aextra(ipdeb), nextra(ipdeb), ineibor,
     $                lextra(ifield))
              ENDIF 
C
C* --->>> Redglo
C
            ELSE IF (canal(ja,ifield) .EQ. 'REDGLO') THEN
              itronca = ntronca(ifield)
              clmsk = cmskrd(ifield)
              zmskval = amskred
C
C* get number of longitudes by latitude circle and total number of points
C  reduced grid
C
C* Zero work array
C
              CALL szero (work, jpmax)
              CALL izero (nwork, jpmax)
              CALL izero (inip, 320)
              IF (itronca .EQ. 16) THEN
                  DO 240 ji = 1, itronca
                    inip(ji) = ninip16(ji)
 240              CONTINUE
                  iredu = nredu16
                ELSE IF (itronca .EQ. 24)  THEN 
                  DO 250 ji = 1, itronca
                    inip(ji) = ninip24(ji)
 250              CONTINUE 
                  iredu = nredu24
                ELSE IF (itronca .EQ. 32)  THEN 
                  DO 260 ji = 1, itronca
                    inip(ji) = ninip32(ji)
 260              CONTINUE 
                  iredu = nredu32
                ELSE IF (itronca .EQ. 48)  THEN 
                  DO 270 ji = 1, itronca
                    inip(ji) = ninip48(ji)
 270              CONTINUE
                   iredu = nredu48   
                ELSE IF (itronca .EQ. 80)  THEN 
                  DO 280 ji = 1, itronca 
                    inip(ji) = ninip80(ji)
 280              CONTINUE
                  iredu = nredu80
                ELSE IF (itronca .EQ. 160)  THEN 
                  DO 285 ji = 1, itronca
                    inip(ji) = ninip160(ji)
 285              CONTINUE
                  iredu = nredu160
                ELSE
                  CALL prtout
     $          ('WARNING!!! Oasis cannot treat this grid with 2*NO 
     $          latitude lines with NO = ', itronca, 2)
                  CALL prtout
     $                ('Implement data for NO =', itronca, 2)
                  CALL HALTE('STOP in preproc')
              ENDIF
              DO 290 ji = ilatbf/2 + 1, ilatbf
                inip(ji) = inip(ilatbf - ji + 1)
 290          CONTINUE
              CALL redglo (fldold(iadrold), work(1), iredu, inip,
     $                     ilonbf, ilatbf, nwork(1), nwork(1+iredu),
     $                     itronca, zmskval, clmsk)
            ELSE
              CONTINUE 
          END IF
 220      CONTINUE
 210    CONTINUE 
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN 
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine preproc ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
