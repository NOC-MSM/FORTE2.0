      SUBROUTINE postpro (kindex, kfield)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *postpro* - postprocessing routine
C
C
C     Purpose:
C     -------
C     Do the field postprocessing
C
C**   Interface:
C     ---------
C       *CALL*  *postpro (kindex, kfield)*
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
C     reverse, revmsk, masq, extrap, glored, chkfld
C
C     Reference:
C     ---------
C     See OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      95/09/01  created
C       2.1       L. Terray      96/09/25  modified: call to chkfld and
C                                          addition of amskred
C       2.2       L. Terray      97/12/31  modified: call to extrap
C       2.3       L. Terray      99/03/01  modified: call to extrap
C       2.3       S. Valcke      99/04/15  modified: CALL to extrap
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       L. Terray      99/09/15  changed periodicity variables
C       2.3       S. Valcke      99/10/14  CALL to extrap corrected
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
      CHARACTER*8 clxordaf, clyordaf, clextmet, clname, clper
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
     $    '           ROUTINE postpro  -  Level 1'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Postprocessing of coupling fields'
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
        iadrnew = nadrnew(ifield)
        isiznew = nsiznew(ifield)
        clname = cnamout(ifield)
        ilabel = numlab(ifield)
        clabel = cfldlab(ilabel)
        ilonaf = nlonaf(ifield)
        ilataf = nlataf(ifield)
        clxordaf = cxordaf(ifield)
        clyordaf = cyordaf(ifield)
        iintflx = nintflx(ifield)
C
C* Print field name
C
        IF (nlogprt .GE. 1) THEN
            CALL prcout('Treatment of field : ', clname, 2)
        ENDIF
C
C* - Do postprocessing analysis
C
        DO 220 ja = 1, ntrans(ifield)
C
C* --->>> Reverse
C
          IF (canal(ja,ifield) .EQ. 'REVERSE') THEN 
              CALL reverse (fldnew(iadrnew), ilonaf,
     $                      ilataf, clxordaf, clyordaf)
C
C* --->>> Checkout: perform basic checks on the field to be exported
C
            ELSE IF (canal(ja,ifield) .EQ. 'CHECKOUT') THEN
              CALL chkfld(clname, clabel, 
     $              fldnew(iadrnew), msknew(iadrnew), surnew(iadrnew),
     $              isiznew, ilonaf, iintflx)
C
C* --->>> Glored
C
            ELSE IF (canal(ja,ifield) .EQ. 'GLORED') THEN
C
C* Do extrapolation on full grid to assign sea values to land points
C
C* - First we mask the field
C    We use a predefined value for the mask
C    If necessary, we reorder the mask as the field might have been already
C    reversed ( while array msknew is ordered along OASIS conventions). 
C
C* Put mask in work array
C
              CALL izero (nwork, jpmax)
              DO 230 ji = 1, isiznew
                nwork(ji) = msknew(iadrnew + ji -1)
 230          CONTINUE 
C
C* Reverse mask if necessary
C
              CALL revmsk (nwork(1), ilonaf, ilataf,
     $                     clxordaf, clyordaf)
              zmskval = amskred
              CALL masq (fldnew(iadrnew), isiznew, zmskval,
     $                   nwork(1))
C
C* - Then we extrapolate
C    We use predefined values for extrapolation parameters
C
              clextmet = 'NINENN'
              ineibor = neighborg(ifield)
C
C* Grid periodicity
C
              clper = ctper(ifield)
              iper = notper(ifield)
C
C* Zero work array
C
              CALL szero (work, jpmax)
C
C* Do it now
C
              CALL extrap (fldnew(iadrnew), zmskval, work(1), 
     $            nwork(1), ilonaf, ilataf,
     $            ineibor, clextmet, clper, iper,
     $            niwtng(ifield), nninnflg(ifield))
C
C* Do the interpolation full to reduced gaussian grid
C
              itronca = ntronca(ifield)
              CALL szero (work, jpmax)
              CALL glored (fldnew(iadrnew), work(1),
     $                     ilonaf, ilataf, itronca)
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
     $    '          --------- End of routine postpro ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
