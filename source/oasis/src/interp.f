      SUBROUTINE interp (kindex, kfield)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *interp* - Control routine for interpolation
C
C
C     Purpose:
C     -------
C     Monitor the field interpolation and auxilary analysis
C
C**   Interface:
C     ---------
C       *CALL*  *interp (kindex, kfield)*
C
C     Input:
C     -----
C                kindex : current active fields index array
C                kfield : current active fields total number
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     fiasco, blasold, filling, mozaic
C
C     Reference:
C     ---------
C     See OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0beta   L. Terray      95/09/01  created
C       2.0       L. Terray      96/02/01  modified: mozaic interpolation
C       2.1       L. Terray      96/08/05  modified: Add new arrays for
C                                          mapping data(weight, adresses)
C                                          addition of no interpolation
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       L. Terray      99/09/15  changed: periodicity variables
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
      INCLUDE 'anais.h'
      INCLUDE 'rainbow.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      INTEGER kindex(kfield)
C
C* ---------------------------- Local declarations ----------------------
C
      REAL zbocoef(jpcomb)
      INTEGER iaddr(jpcomb), isize(jpcomb), iflag(jpcomb)
      CHARACTER*8 clintmet, clgrdtyp, clfldtyp, clfilfic, clfilmet
      CHARACTER*8 clbofld(jpcomb), clfic, clstrg, clname
      CHARACTER*8 clsper, cltper
      LOGICAL llchk
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
     $    '           ROUTINE interp  -  Level 1'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Monitor field interpolation'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
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
        iadrold = nadrold(ifield)
        isizold = nsizold(ifield)
        iadrnew = nadrnew(ifield)
        isiznew = nsiznew(ifield)
        ilonbf = nlonbf(ifield)
        ilatbf = nlatbf(ifield)
        ilonaf = nlonaf(ifield)
        ilataf = nlataf(ifield)
        clname = cnaminp(ifield)
        clsper = csper(ifield)
        cltper = ctper(ifield)
        isper = nosper(ifield)
        itper = notper(ifield)
C
C* Print field name
C
        IF (nlogprt .GE. 1) THEN
            CALL prcout('Treatment of field : ', clname, 2)
        ENDIF
C
C* - Do interpolation
C
        DO 220 ja = 1, ntrans(ifield)
          IF (canal(ja,ifield) .EQ. 'INTERP') THEN
C
C* --->>> Interp
C
C* Assign local variables
              clintmet = cintmet(ifield)
              clgrdtyp = cgrdtyp(ifield)
              clfldtyp = cfldtyp(ifield)
C
C* Zero work array
C
              CALL szero (work, jpmax)
              CALL fiasco
C* Data about fields, grids, masks and surfaces
     $            (fldnew(iadrnew),
     $            xgrnew(iadrnew), ygrnew(iadrnew), surnew(iadrnew),
     $            msknew(iadrnew), ilonaf, ilataf, cltper, itper,
     $            fldold(iadrold),
     $            xgrold(iadrold), ygrold(iadrold), surold(iadrold),
     $            mskold(iadrold), ilonbf, ilatbf, clsper, isper,
C* Work arrays for the different interpolators
     $            work(1), 
     $            work(1+ilonaf), 
     $            work(1+ilonaf+ilataf),
     $            work(1+ilonaf+ilataf+isizold),
     $            work(1+ilonaf+ilataf+isizold+isiznew),
     $            work(1+ilonaf+ilataf+2*isizold+isiznew),
C* Define field number and type, grid and interpolation 
     $            ifield, clintmet, clgrdtyp, clfldtyp)
C
C* --->>> Nointerp
C
            ELSE IF (canal(ja,ifield) .EQ. 'NOINTERP') THEN
C
C* output field is equal to input field
C
C* First, check dimension
C
                llchk = isiznew .EQ. isizold
                IF (.NOT. llchk) CALL prcout('WARNING: size mismatch
     $              in NOINTERP between old and new field ',clname,2)
                IF (.NOT. llchk) CALL HALTE('STOP in interp')
C* Do the assign
                DO 230 jk = 1, isiznew
                  fldnew(iadrnew - 1 + jk) = fldold(iadrold - 1 + jk)
 230            CONTINUE 
C
C* --->>> Mozaic
C
            ELSE IF (canal(ja,ifield) .EQ. 'MOZAIC') THEN
C
C* assign local variables and get pointer for mapping interpolation
C
                clfic = cgrdmap(ifield)
                iunit = nlumap(ifield)
                iloc = nmapfl(ifield)
                ipdeb = (nmapfl(ifield)-1)*jpmoa*jpgrd+1
                ivoisin = nmapvoi(ifield)
                clstrg = cficbf(ifield)//cficaf(ifield)
                CALL mozaic (fldnew(iadrnew), isiznew,
     $                       fldold(iadrold), isizold,
     $                       clfic, iunit, clstrg, iloc,
     $                       amapp(ipdeb), nmapp(ipdeb), 
     $                       ivoisin, lmapp(ifield))
C
C* --->>> Blasold
C
            ELSE IF (canal(ja,ifield) .EQ. 'BLASOLD') THEN
C
C* Assign local variables
C
              zfldcobo = afldcobo(ifield)
              ibofld = nbofld(ifield)
              DO 240 jc = 1, ibofld
                clbofld(jc) = cbofld(jc,ifield)
                zbocoef(jc) = abocoef(jc,ifield)
 240          CONTINUE
C
C* Get the additional fields (pointers and sizes)
C
              CALL szero( work, jpmax)
              DO 250 jc = 1, ibofld
                IF (clbofld(jc) .EQ. 'CONSTANT') THEN
                    isize(jc) = isizold
                  ELSE 
                    DO 260 jb = 1, nfield
C
C* Check field names input list
C
                      IF (clbofld(jc) .EQ. cnaminp(jb)) THEN
                          iflag(jc) = jb
                      ENDIF 
 260                CONTINUE
                    ipointer  = nadrold(iflag(jc))
                    isize(jc) = nsizold(iflag(jc))
                ENDIF 
                IF (jc .EQ. 1) THEN
                    iaddr(jc) = 1
                  ELSE
                    iaddr(jc) = 1 + isize(jc-1)
                ENDIF
C
C* Assign values to temporary array work
C
                IF (clbofld(jc) .EQ. 'CONSTANT') THEN
                    DO 270 jd = 1, isize(jc)
                      work(iaddr(jc)+jd-1) = 1.0
 270                CONTINUE 
                  ELSE 
                    DO 280 jd = 1, isize(jc)
                      work(iaddr(jc)+jd-1) = fldold(ipointer+jd-1)
 280                CONTINUE 
                ENDIF 
 250          CONTINUE
C
C* Get total size for array work ( sum of additional fields sizes)
C 
              isiztot = iaddr(ibofld) + isize(ibofld) - 1
              CALL blasold (fldold(iadrold), isizold, ifield,
     $                      zfldcobo, ibofld, iaddr, isize,
     $                      zbocoef, isiztot, work)
C
C* --->>> Filling
C
            ELSE IF (canal(ja,ifield) .EQ. 'FILLING') THEN
C
C* Assign local variables
C
              clfilfic = cfilfic(ifield)
              iunit = nlufil(ifield)
              clfilmet = cfilmet(ifield)
C
C* Zero work array
C
              CALL szero (work, jpmax)
C
C* Address of overlapping grids array used in Anaism 
C
              ipointer = (naismfl(ifield)-1)*jpgrd + 1
              CALL filling (fldnew(iadrnew), work(1), work(isiznew+1),
     $                      work(2*isiznew+1),
     $                      xgrnew(iadrnew), ygrnew(iadrnew),
     $                      ilonaf, ilataf, 
     $                      msknew(iadrnew), nmesh(ipointer), iunit,
     $                      clfilfic, clfilmet)
            ELSE
              CONTINUE
          END IF
 220    CONTINUE
 210  CONTINUE 
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine interp ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
