      SUBROUTINE chkpar
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *chkpar*  - Parameter checking
C
C     Purpose:
C     -------
C     Checks option compatibility between oasis and remote models
C     as well as some basic dimension checks
C
C**   Interface:
C     ---------
C       *CALL*  *chkpar*
C
C     Input:
C     -----
C     None
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C
C     Externals:
C     ---------
C     imaxim
C
C     Reference:
C     ---------
C     See OASIS manual (1995) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      95/10/01  created
C       2.2       L. Terray      97/11/13  added: check nitfn if cchan
C                                                 equals NONE
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       S. Valcke      99/10/12  check if nninnfl and nninnflg
C                                          are different
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'timestep.h'
      INCLUDE 'experiment.h'
      INCLUDE 'field.h'
      INCLUDE 'string.h'
      INCLUDE 'hardware.h'
      INCLUDE 'printing.h'
      INCLUDE 'extrapol.h'
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER itime(jpmodel), iexch(jpmodel)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initializations
C        ---------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE chkpar  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' control of run time options'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Initialization of remote models time variables.
C
      DO 110 jm = 1, nmodel
        itime(jm) = mstep(jm) * mdt(jm)
        iexch(jm) = mfcpl(jm) * mdt(jm)
 110  CONTINUE
C
C
C*    2. Basic checks
C        ------------
C
C* Check number of fields
C
      IF (nfield .GT. jpfield) THEN
          CALL prtout
     $        ('WARNING: number of fields greater than jpfield = ',
     $         jpfield, 2)
          CALL prtout
     $        ('         Number of fields nfield = ', nfield, 2)
          CALL HALTE('STOP in chkpar')
      ENDIF
C
C* Check number of models
C
      IF (nmodel .GT. jpmodel) THEN
          CALL prtout
     $        ('WARNING: number of models greater than jpmodel = ',
     $         jpmodel, 2)
          CALL prtout
     $        ('         Number of models nmodel = ', nmodel, 2)
          CALL HALTE('STOP in chkpar')
      ENDIF
C
C* Check maximum number of analysis
C
      imaxana = imaxim (ntrans, nfield)
      IF (imaxana .GT. jpanal) THEN
          CALL prtout
     $        ('WARNING: number of analysis greater than jpanal = ',
     $         jpanal, 2)
          CALL prtout
     $        ('         Number of analysis imaxana = ', imaxana, 2)
          CALL HALTE('STOP in chkpar')
      ENDIF
C
C
C*    3. Time compatibility checks
C        -------------------------
C
      DO 310 jm = 1, nmodel
        IF (itime(jm) .NE. ntime) THEN
            WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
            WRITE (UNIT = nulou,FMT = *) 
     $          ' ===>>> : total runtime for model',jm
            WRITE (UNIT = nulou,FMT = *)
     $         '          incompatible with coupler total time '
            WRITE (UNIT = nulou,FMT = *) 
     $          ' ======         =======              '
            WRITE (UNIT = nulou,FMT = *) ' '
            WRITE (UNIT = nulou,FMT = *) 
     $          ' The current simulation uses asynchroneous coupling'
            WRITE (UNIT = nulou,FMT = *) 
     $          '                             -------------'
            WRITE (UNIT = nulou,FMT = *) ' '
        ENDIF
C
C* The exchange frequency data sent from remote models is the
C  smallest frequency used in a given model. This is a crude 
C  check of the compatibility of coupling options.
C
        IF (MOD(iexch(jm),nstep) .NE. 0) THEN
            WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
            WRITE (UNIT = nulou,FMT = *) 
     $          ' ===>>> : exchange time incompatible with model',jm
            WRITE (UNIT = nulou,FMT = *) 
     $          ' ======            =====          '
            WRITE (UNIT = nulou,FMT = *) ' '
            WRITE (UNIT = nulou,FMT = *) 
     $          ' We STOP        !!! rerun with new  parameters'
            WRITE (UNIT = nulou,FMT = *) 
     $          '                               ---------------'
            WRITE (UNIT = nulou,FMT = *) ' '
            CALL HALTE ('STOP in chkpar')
        ENDIF
 310  CONTINUE 
C
C
C*    4. No message passing mode: ensure one iteration only
C        -----------------------
C
      IF (cchan .EQ. 'NONE') THEN 
          IF (nitfn .NE. 0) THEN 
              WRITE (UNIT = nulou,FMT = *)
     $            ' ===>>> : - No message passing - mode '
              WRITE (UNIT = nulou,FMT = *)
     $            ' Requires only one iteration !!! nitfn = 0 '
              nitfn = 0
          ENDIF 
      ENDIF 
C
C
C*    5. Ensure that datasets for EXTRAP/NINENN called by preproc.f are
C*       different from datasets for EXTRAP/NINENN called by postpro.f
C
C
      DO 510 jfext=1,jpfield
        IF (nninnfl(jfext) .NE. 0) THEN
            DO 520 jfint=1,jpfield
              IF (nninnfl(jfext) .EQ. nninnflg(jfint)) THEN
                CALL prtout
     $          ('WARNING: the EXTRAP/NINENN dataset number for field ',
     $           jfext, 2)
                CALL prtout
     $          ('is the same as the GLORED dataset number for field ',
     $           jfint, 2)
                CALL HALTE('STOP in chkpar')
              ENDIF
 520        CONTINUE
        ENDIF
 510  CONTINUE
C
C
C*    6. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          ---------- End of routine chkpar --------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


