      SUBROUTINE cookart (kindex, kfield)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *cookart* - More or less clever stuff
C
C
C     Purpose:
C     -------
C     Do subgrid variability, flux conservation or
C     basic linear algebra stuff
C
C**   Interface:
C     ---------
C       *CALL*  *cookart (kindex, kfield)*
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
C                zbncoef : additional field coefficients for blasnew (real 1D)
C                iaddr   : memory allocation of the work array (integer 1D)
C                isize   : memory allocation of the work array (integer 1D)
C                iflag   : memory allocation of the work array (integer 1D)
C                clbnfld : additional field names for blasnew (character 1D)
C
C     Externals:
C     ---------
C     conserv, subgrid, blasnew
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
C       2.1       L. terray      96/08/05  modified: subgrid analysis
C       2.3       S. Valcke      99/04/30  added: printing levels
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
      INCLUDE 'rainbow.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      INTEGER kindex(kfield)
C
C* ---------------------------- Local declarations ----------------------
C
      REAL zbncoef(jpcomb)
      INTEGER iaddr(jpcomb), isize(jpcomb), iflag(jpcomb)
      CHARACTER*8 clconmet, clbnfld(jpcomb), clname,  cldqdt
      CHARACTER*8 clfldcoa, clfldfin, clfic
      LOGICAL lli, llj, llk, llt
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
     $    '            ROUTINE cookart  -  Level 1'
          WRITE (UNIT = nulou,FMT = *) 
     $    '            ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Subgrid variability, blas and flux conservation '
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
        clname = cnamout(ifield)
C
C* Print field name
C
        IF (nlogprt .GE. 1) THEN 
            CALL prcout('Treatment of field : ', clname, 2)
        ENDIF
C
C* - Do additional analysis
C
        DO 220 ja = 1, ntrans(ifield)
C
C* --->>> Flux conservation
C
          IF (canal(ja,ifield) .EQ. 'CONSERV') THEN
C
C* Zero work array
C
              CALL szero (work, jpmax)
C
C* Get conservation method and do the job
C
              clconmet = cconmet(ifield)
              CALL conserv (fldold(iadrold), isizold,
     $                      mskold(iadrold), surold(iadrold),
     $                      fldnew(iadrnew), isiznew,
     $                      msknew(iadrnew), surnew(iadrnew),
     $                      work(1), work(1+isizold), clconmet)
C
C* --->>> Subgrid variability
C
C* We have to estimate  Fo = sum_a{ B(o,a)*(Fa + dFa/dTa * (To - Ta))}
C  a and o mean coarse and fine grid respectively 
C  with To on the fine grid initially
C
            ELSE IF (canal(ja,ifield) .EQ. 'SUBGRID') THEN
C
C* Zero work array
C
              CALL szero (work, jpmax)
C
C* Get names for fields on both fine and coarse grids
C
              clfldcoa = cfldcoa(ifield)
              clfldfin = cfldfin(ifield)
C
C* No coupling ratio needed unless subgrid deals with non solar flux
C  Initialize anyway local variable cldqdt
C
              cldqdt = 'NONE'
              IF (ctypsub(ifield) .EQ. 'NONSOLAR') THEN 
                  cldqdt = cdqdt(ifield)
              ENDIF
C
C* In order not to have problems for solar flux for the dimension checks
C 
              itot3 = isizold
C
C* Look for field data
C
              DO 230 jn = 1, nfield
C
C* Find Ta field: initially on coarse grid
C  The Ta field is in fact the To field 
C  which has been interpolated at the previous timestep
C
                IF (cnaminp(jn) .EQ. clfldcoa) THEN
                    iadr1 = nadrold(jn)
                    itot1 = nsizold(jn)
C
C* Find additional field To on fine grid
C
                  ELSE IF (cnaminp(jn) .EQ. clfldfin) THEN
                    iadr2 = nadrold(jn)
                    itot2 = nsizold(jn)
C
C* Find dFa/dTa field :initially on coarse grid, corresponding to 
C  the previous timestep
C
                  ELSE IF (cnaminp(jn) .EQ. cldqdt) THEN
                    iadr3 = nadrold(jn)
                    itot3 = nsizold(jn)
                ENDIF
 230          CONTINUE
C* Get Ta
              DO 231 ji = 1, itot1
                work(ji) = fldold(iadr1-1+ji)
 231          CONTINUE 
C* Get To
              DO 232 ji = 1, itot2
                work(itot1+ji) = fldold(iadr2-1+ji)
 232          CONTINUE
C* Get dFa/dTa only if we deal with non solar flux
              IF (ctypsub(jf) .EQ. 'NONSOLAR') THEN 
                  DO 234 ji = 1, itot3
                    work(itot1+itot2+ji) = fldold(iadr3-1+ji)
 234              CONTINUE
              ENDIF 
C
C* Check sizes
C
              lli = isizold .EQ. itot1
              llj = isiznew .EQ. itot2
              llk = isizold .EQ. itot3
              IF (.NOT. lli) CALL prcout('WARNING: size mismatch
     $            between coarse and initial field',clname,2)
              IF (.NOT. llj) CALL prcout('WARNING: size mismatch
     $            between final and fine field',clname,2)
              IF (.NOT. llk) CALL prcout('WARNING: size mismatch
     $            between coarse and dqdt field',clname,2)
              llt = lli .AND. llj .AND. llk
              IF (.NOT. llt) CALL HALTE('STOP in cookart')
C
C* Do the subgrid interpolation
C
C* assign local variables and get pointer for subgrid interpolation
C
              clfic = cgrdsub(ifield)
              iunit = nlusub(ifield)
              iloc = nsubfl(ifield)
              ivoisin = nsubvoi(ifield)
              ipdeb = (nsubfl(ifield)-1)*jpsoa*jpgrd+1
              CALL subgrid (fldnew(iadrnew), fldold(iadrold),
     $                      isiznew, isizold,
     $                      work(1), work(1+isizold), 
     $                      work(1+isizold+isiznew),
     $                      clfic, iunit, iloc, clname,
     $                      asubg(ipdeb), nsubg(ipdeb),
     $                      ivoisin, lsubg(ifield), ctypsub(ifield))
C
C* --->>> Blasnew
C
            ELSE IF (canal(ja,ifield) .EQ. 'BLASNEW') THEN
C
C* Assign local variables to main field coefficient and number of extra fields
C
              zfldcobn = afldcobn(ifield)
              ibnfld = nbnfld(ifield)
C
C* Read in additional field names and related coefficients
C
              DO 240 jc = 1, ibnfld
                clbnfld(jc) = cbnfld(jc,ifield)
                zbncoef(jc) = abncoef(jc,ifield)
 240          CONTINUE
C
C* - Get the additional fields parameters (pointers and sizes)
C
              CALL szero( work, jpmax)
              DO 250 jc = 1, ibnfld
C
C* Constant fields
C
                IF (clbnfld(jc) .EQ. 'CONSTANT') THEN
                    isize(jc) = isiznew
                  ELSE
C
C* Others
C 
                    DO 260 jb = 1, nfield
C
C* Check field names input list
C
                      IF (clbnfld(jc) .EQ. cnamout(jb)) THEN
                          iflag(jc) = jb
                      ENDIF 
 260                CONTINUE
                    ipointer  = nadrnew(iflag(jc))
                    isize(jc) = nsiznew(iflag(jc))
                ENDIF 
C
C* Get memory adresses for array work
C
                IF (jc .EQ. 1) THEN
                    iaddr(jc) = 1
                  ELSE
                    iaddr(jc) = 1 + isize(jc-1)
                ENDIF
C
C* Assign values to temporary array work
C
                IF (clbnfld(jc) .EQ. 'CONSTANT') THEN
                    DO 270 jd = 1, isize(jc)
                      work(iaddr(jc)+jd-1) = 1.0
 270                CONTINUE 
                  ELSE 
                    DO 280 jd = 1, isize(jc)
                      work(iaddr(jc)+jd-1) = fldnew(ipointer+jd-1)
 280                CONTINUE 
                ENDIF 
 250          CONTINUE
C
C* Get total size for array work ( sum of additional fields sizes)
C 
              isiztot = iaddr(ibnfld) + isize(ibnfld) - 1
              CALL blasnew (fldnew(iadrnew), isiznew, ifield,
     $                      zfldcobn, ibnfld, iaddr, isize,
     $                      zbncoef, isiztot, work)
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
     $    '          --------- End of routine cookart ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
