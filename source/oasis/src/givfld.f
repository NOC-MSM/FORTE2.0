      SUBROUTINE givfld (kindex, kfield, kiter)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *getfld* - writing routine
C
C
C     Purpose:
C     -------
C     Write out coupling fields for iteration kiter
C
C**   Interface:
C     ---------
C       *CALL*  *givfld (kindex, kfield, kiter)*
C
C     Input:
C     -----
C                kindex : current active fields index array
C                kfield : current active fields total number
C                kiter  : iteration number
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
C     PIPE_Send, CLIM_Export, SVIPC_write
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
C       2.1       L. Terray      96/08/07  modified: addition of cstate
C                                          to prevent field transfer
C       2.2       S. Valcke      97/08/22  added: introduction of SVIPC
C       2.2       L. Terray      97/12/14  added: test on mode info +
C                                          general cleaning
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       L. Terray      99/09/15  added: GMEM branch
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'hardware.h'
      INCLUDE 'clim.h'
      INCLUDE 'field.h'
      INCLUDE 'string.h'
      INCLUDE 'analysis.h'
      INCLUDE 'memory.h'
      INCLUDE 'label.h'
      INCLUDE 'sipc.h'
      INCLUDE 'nproc.h'
      INCLUDE 'experiment.h'
      INCLUDE 'calendar.h'
      INCLUDE 'timestep.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      INTEGER kindex(kfield)
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER info(jpfield), iflag(jpfield)
      CHARACTER*8 clname, clfic, clstat
      CHARACTER*32 clabel
      INTEGER itime(3)
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
     $    '           ROUTINE givfld  -  Level 1'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Give coupling fields'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      infos = CLIM_Ok
      iflags = 0
      istop = 0
      imrca = 0
      imrcb = 0
      CALL izero (iflag, jpfield)
      CALL izero (info, jpfield)
C
C
C*    2. Loop on active fields for iteration kiter
C        -----------------------------------------
C
      DO 210 jf = 1, kfield
C
C* Assign local variables
C
        iloc = kindex(jf)
        iadrnew = nadrnew(iloc)
        isiznew = nsiznew(iloc)
        clname = cnamout(iloc)
        ilabel = numlab(iloc)
        clabel = cfldlab(ilabel)
        iunit = nluout(iloc)
        clstat = cstate(iloc)
C
C* Test if field must be exported
C
        IF (clstat .EQ. 'EXPORTED') THEN 
C
C* Print field name
C
            IF (nlogprt .GE. 1) THEN
                CALL prcout('Writing of field : ', clname, 2)
                CALL prcout('Field definition : ', clabel, 2)
            ENDIF
C
C* Define header if info mode is on :
C  experiment name, initial date, iteration number, time since start
C
            IF (lmodinf) THEN 
                itime(1) = ndate
                itime(2) = kiter
                itime(3) = kiter * nstep
                IF (nlogprt .GE. 2) THEN
                    WRITE (UNIT = nulou,FMT = *) 
     $              ' Encapsulated data for current field is :'
                    CALL prcout ('Experiment name', cjobnam, 1)
                    CALL prtout ('Initial date', itime(1), 2)
                    CALL prtout ('Iteration number', itime(2), 2)
                    CALL prtout ('Time since start', itime(3), 2)
                ENDIF
            ENDIF 
C
C* - Give coupling fields
C
C* PIPE or NONE case
C
            IF (cchan .EQ. 'PIPE' .OR. cchan .EQ. 'NONE') THEN
C
C* Write new field on unit iunit with header if required
C 
                IF (lmodinf) THEN 
                    CALL locwrith 
     $                  (clname, cjobnam, itime, fldnew(iadrnew),  
     $                  isiznew, iunit, iflag(jf))
                  ELSE 
                    CALL locwrite 
     $                    (clname, fldnew(iadrnew), isiznew, 
     $                    iunit, iflag(jf))
                ENDIF 
                iflags = iflags + iflag(jf)
C
C* SIPC case
C
              ELSE IF (cchan .EQ. 'SIPC' .OR. 
     $              cchan .EQ. 'GMEM') THEN
C
C* Write encapsulated infos in field-specific shared memory pool
C
                IF (lmodinf) THEN 
                    isizeout = 3*jpbytecha
                    CALL SVIPC_write(mpoolidou(iloc), cjobnam 
     $                  , isizeout, imrca)
                    isizeout = 3*jpbyteint
                    CALL SVIPC_write(mpoolidou(iloc), itime 
     $                  ,isizeout,imrcb)
C
C* Find error if any
C
                    IF (imrca .LT. 0 .OR. imrcb .LT. 0) THEN
                        CALL prcout 
     $ ('Problem in writing encapsulated infos for field', clname, 1)
                        istop = 1
                    ENDIF 
                    IF (nlogprt .GE. 2) THEN
                        CALL prcout
     $ ('Wrote encapsulated infos in pool for field',clname,1)
                    ENDIF 
                ENDIF 
C
C* Write part of macro array in field-specific shared memory pool
C
                isizeout = isiznew * jpbyterea
                IF (nlogprt .GE. 2) THEN
                    WRITE(UNIT = nulou, FMT = *) 
     $                  'Writing field data to pool = ',mpoolidou(iloc)
                ENDIF
                CALL SVIPC_write(mpoolidou(iloc),
     $              fldnew(iadrnew), isizeout, imrc)
C
C* Find error and stop if any
C
                IF (imrc .LT. 0) THEN
                    CALL prcout
     $                  ('Problem in writing field in SHM pool:', 
     $                  clname, 1)
                    istop = 1
                  ELSE IF (nlogprt .GE. 2) THEN
                    CALL prcout
     $                    ('Wrote field in SHM pool:', clname, 1)
                ENDIF
C
C* CLIM case
C
              ELSE IF (cchan .EQ. 'CLIM') THEN
                IF (lmodinf) THEN 
                    CALL CLIM_Export 
     $                  (clname, kiter, cjobnam, imrca) 
                    CALL CLIM_Export 
     $                  (clname, kiter, itime, imrcb) 
C
C* Find error if any
C
                    IF (imrca .NE. CLIM_Ok .OR. 
     $                  imrcb .NE. CLIM_Ok) THEN
                        CALL prcout
     $   ('Problem in writing encapsulated infos for field', clname, 1)
                      ELSE IF (nlogprt .GE. 2) THEN 
                        CALL prcout
     $   ('Wrote encapsulated infos in CLIM channel for field', 
     $                        clname, 1)
                    ENDIF 
                ENDIF
C
C* Write new field on port clname
C

c          write(81,*)'before clim_export '   ! mmj
c          write(81,*)clname, kiter, fldnew(iadrnew), info(jf), jf

                CALL CLIM_Export 
     $                (clname, kiter, fldnew(iadrnew), info(jf))

c          write(81,*)'after clim_export '    ! mmj
c          write(81,*)clname, kiter, fldnew(iadrnew), info(jf), jf


                infos = infos + info(jf)
              ELSE
                CALL prcout 
     $                ('Wrong CHANNEL option for field', clname, 1)
            ENDIF
          ELSE IF (clstat .EQ. 'AUXILARY') THEN 
            iflag(jf) = 0
            info(jf) = 0
        ENDIF
 210  CONTINUE
C
C* Stop if problem in writing in CLIM case
C
      IF (infos .NE. CLIM_Ok) THEN 
          DO 220 jf = 1, kfield
            IF (info(jf) .NE. CLIM_Ok) THEN
                CALL prcout
     $              ('WARNING: problem in writing field on port',
     $              clname, 1)
                CALL prtout
     $              ('CLIM error code number', info(jf), 2)
            ENDIF
 220      CONTINUE
          CALL HALTE ('STOP in givfld') 
      ENDIF
C
C* Stop if problem in writing in PIPE or NONE case
C
      IF (iflags .NE. 0) THEN 
          DO 230 jf = 1, kfield
            IF (iflag(jf) .NE. 0) THEN
                iloc = kindex(jf)
                iunit = nluout(iloc)
                clname = cnamout(iloc)
                CALL prcout
     $              ('WARNING: problem in writing field',
     $              clname, 1)
                CALL prtout
     $              ('Error writing on logical unit', iunit, 2)
            ENDIF
 230      CONTINUE
          CALL HALTE ('STOP in givfld')
      ENDIF
C
C* Stop if problem in writing in SIPC or GMEM case
C
      IF (istop .NE. 0) CALL HALTE ('STOP in givfld')
C
C
C*    3. PIPE Case: flush data files and send message
C        --------------------------------------------
C
      IF (cchan .EQ. 'PIPE') THEN 
          DO 310 jf = 1, kfield
C
C* Assign local variables
C
            iloc = kindex(jf)
            clname = cnamout(iloc)
            iunit = nluout(iloc)
            clfic = cficout(iloc)
            clstat = cstate(iloc)
C
C* Test if field must be exported
C
            IF (clstat .EQ. 'EXPORTED') THEN 
C
C* Flush data file
C
                CLOSE(UNIT = iunit, ERR = 3010, IOSTAT = ios)
                IF (nlogprt .GE. 2) THEN
                    WRITE(UNIT = nulou, FMT = 3100) iunit, clfic
                ENDIF
 3010           CONTINUE
                IF (ios .NE. 0) THEN
                    CALL prtout('WARNING: problem in closing unit',
     $                  iunit, 2)
                    CALL prtout('Error message number is = ', ios, 2)
                    CALL HALTE('STOP in givfld')
                ENDIF 
                OPEN(UNIT=iunit,FILE=clfic,FORM='UNFORMATTED',
     $              STATUS='UNKNOWN',ERR = 3020, IOSTAT = ios)
                IF (nlogprt .GE. 2) THEN
                    WRITE(UNIT = nulou, FMT = 3200) iunit, clfic
                ENDIF
 3020           CONTINUE
                IF (ios .NE. 0) THEN
                    CALL prtout('WARNING: problem in connecting unit',
     $                  iunit, 2)
                    CALL prtout('Error message number is = ', ios, 2)
                    CALL HALTE('STOP in givfld')
                ENDIF 
C
C* Send message on pipe clname
C
                CALL PIPE_Send (clname, kiter)
              ELSE IF (clstat .EQ. 'AUXILARY' 
     $              .AND. nlogprt .GE. 2) THEN 
                WRITE(UNIT = nulou, FMT = 3300) clname, clfic
            ENDIF 
 310      CONTINUE
      ENDIF
 3100 FORMAT(/,5X,' Unit ',I2,' has been disconnected from file ',A8)
 3200 FORMAT(/,5X,' Unit ',I2,' has been reconnected to file ',A8)
 3300 FORMAT(/,5X,' Auxilary field ',A8,' is not written on file ',A8)
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine givfld ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END

