      SUBROUTINE getfld (kindex, kfield, kiter)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *getfld* - reading routine
C
C
C     Purpose:
C     -------
C     Read coupling fields for iteration kiter
C
C**   Interface:
C     ---------
C       *CALL*  *getfld (kindex, kfield, kiter)*
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
C     PIPE_Recv, CLIM_Import, SVIPC_read
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
C       2.1       L. Terray      96/08/05  modified: correct printing of
C                                          diagnostic (variable clname)
C       2.2       S. Valcke      97/08/22  added: introduction of SVIPC
C       2.2       L.Terray       97/09/20  added: extra test in delay mode
C                                          + general cleaning + test on
C                                          info mode
C       2.3       L. Terray      99/03/01  modified: reading in delay mode
C                                          (negative values mean one must
C                                           read files, 0 means channel)
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
      INCLUDE 'timestep.h'
      INCLUDE 'experiment.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      INTEGER kindex(kfield)
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER info(jpfield), iflag(jpfield)
      CHARACTER*8 clname, clfic
      CHARACTER*32 clabel
      INTEGER itime(3)
      CHARACTER*3 cljobnam
      LOGICAL llfit, lldel
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
     $    '           ROUTINE getfld  -  Level 1'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Get coupling fields'
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
C* Get maximum delay for all coupled fields
C
      IF (kiter .EQ. 0) THEN
          nmxdel = imaxim (nfinit, nfield)
      ENDIF
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
        iadrold = nadrold(iloc)
        isizold = nsizold(iloc)
        ilabel = numlab(iloc)
        clabel = cfldlab(ilabel)
        clname = cnaminp(iloc)
        clfic = cficinp(iloc)
        iunit = nluinp(iloc)
        iseqn = nseqn(iloc)
        idelay = nfinit(iloc)
C
C* Print field name
C
        IF (nlogprt .GE. 1) THEN
            CALL prcout('Reading of field : ', clname, 2)
        ENDIF
        IF (nlogprt .GE. 2) THEN
            CALL prcout('Field definition : ', clabel, 2)
        ENDIF
C
C* - Get coupling fields
C
C* Specific treatment for first iteration or subsequent ones
C  for initial coupled field while active delay mode is on for another one
C 
        llfit = iseqn .EQ. 1 .AND. kiter .EQ. 0
        lldel = iseqn .EQ. 1 .AND. kiter .LE. nmxdel .AND. 
     $      idelay .LT. 0
        IF (llfit .OR. lldel) THEN
C
C* Reconnect data file to appropriate unit
C
            OPEN(UNIT = iunit, FILE = clfic, FORM = 'UNFORMATTED',
     $          STATUS = 'UNKNOWN', ERR = 2010, IOSTAT = ios)
            IF (nlogprt .GE. 2) THEN
                WRITE(UNIT = nulou, FMT = 2100) iunit, clfic
            ENDIF
 2010       CONTINUE 
            IF (ios .NE. 0) THEN
                CALL prtout('An error occurred while connecting
     $              unit = ',iunit,2)
                CALL prtout('The error message number is = ',
     $              ios, 2)
                CALL prcout('The file to be connected was ',
     $              clfic, 1)
                CALL HALTE('STOP in getfld')
            ENDIF
C
C* Read header information or not
C 
            IF (lmodinf) THEN 
                CALL locreadh (clname, cljobnam, itime,  
     $              fldold(iadrold), isizold, iunit, iflag(jf)) 
              ELSE 
                CALL locread (clname, fldold(iadrold), isizold,
     $                iunit, iflag(jf))
            ENDIF 
            iflags = iflags + iflag(jf)
C
C* Close unit
C
            CLOSE (UNIT = iunit, ERR = 2020, IOSTAT = ios)
            IF (nlogprt .GE. 2) THEN
                WRITE(UNIT = nulou, FMT = 2200) iunit, clfic
            ENDIF
 2020       CONTINUE
            IF (ios .NE. 0) THEN
                CALL prtout('WARNING: problem in closing unit',
     $              iunit, 2)
                CALL prtout('Error message number is = ', ios, 2)
                CALL HALTE('STOP in getfld')
            ENDIF
C
C* Next iterations
C
        ELSE
C
C* PIPE case
C
            IF (cchan .EQ. 'PIPE') THEN
C
C* Wait for message
C
                CALL PIPE_Recv (clname, kiter)
C
C* Reconnect data file to appropriate unit
C
                OPEN(UNIT = iunit, FILE = clfic, FORM = 'UNFORMATTED',
     $               STATUS = 'UNKNOWN', ERR = 2030, IOSTAT = ios)
                IF (nlogprt .GE. 2) THEN
                    WRITE(UNIT = nulou, FMT = 2100) iunit, clfic
                ENDIF
 2030           CONTINUE 
                IF (ios .NE. 0) THEN
                    CALL prtout('An error occurred while connecting
     $                  unit = ',iunit,2)
                    CALL prtout('The error message number is = ',
     $                  ios, 2)
                    CALL prcout('The file to be connected was ',
     $                  clfic, 1)
                    CALL HALTE('STOP in getfld')
                ENDIF 
C
C* Read header info (if required) and data
C
                IF (lmodinf) THEN 
                    CALL locreadh (clname, cljobnam, itime,  
     $                  fldold(iadrold), isizold, iunit, iflag(jf)) 
                  ELSE
                    CALL locread 
     $                  (clname, fldold(iadrold), isizold, iunit, 
     $                  iflag(jf))
                ENDIF 
                iflags = iflags + iflag(jf)
C
C* Close unit
C
                CLOSE (UNIT = iunit, ERR = 2040, IOSTAT = ios)
                IF (nlogprt .GE. 2) THEN
                    WRITE(UNIT = nulou, FMT = 2200) iunit, clfic
                ENDIF
 2040           CONTINUE
                IF (ios .NE. 0) THEN
                    CALL prtout('WARNING: problem in closing unit',
     $                  iunit, 2)
                    CALL prtout('Error message number is = ', ios, 2)
                    CALL HALTE('STOP in getfld')
                ENDIF 
C
C* SIPC or GMEM case
C
              ELSE IF (cchan .EQ. 'SIPC'.OR. cchan .EQ. 'GMEM') THEN
C
C* Read encapsulated infos in field-specific shared memory pool
C  (experiment name, initial date, time since start, iteration number)
C
                IF (lmodinf) THEN 
                    isizeinp = 3*jpbytecha
                    CALL SVIPC_read(mpoolidin(iloc), cljobnam 
     $                  , isizeinp, imrca)
                    isizeinp = 3*jpbyteint
                    CALL SVIPC_read(mpoolidin(iloc), itime 
     $                  , isizeinp, imrcb)
C
C* Find error if any
C
                    IF (imrca .LT. 0 .OR. imrcb .LT. 0) THEN
                        CALL prcout
     $   ('Problem in reading encapsulated infos for field', clname, 1)
                        istop = 1
                      ELSE
                        CALL prcout
     $   ('Read encapsulated infos in SHM pool for field', clname, 1)
                    ENDIF 
                ENDIF
C
C* Read part of macro array in field-specific shared memory pool
C
                isizeinp = isizold * jpbyterea
                WRITE(UNIT = nulou, FMT = *) 
     $   'Reading field data from pool = ',mpoolidin(iloc) 
                CALL SVIPC_read(mpoolidin(iloc),
     $                        fldold(iadrold), isizeinp, imrc)
C
C* Find error if any
C
                IF (imrc .LT. 0) THEN
                    CALL prcout
     $            ('Problem in reading the field in SHM pool:',clname,1)
                    istop = 1
                ELSE
                    CALL prcout
     $            ('Read field in SHM pool:', clname, 1)
                ENDIF
C
C* CLIM case
C
              ELSE IF (cchan .EQ. 'CLIM') THEN
                IF (lmodinf) THEN 
                    CALL CLIM_Import 
     $                  (clname, kiter, cljobnam, imrca) 
                    CALL CLIM_Import 
     $                  (clname, kiter, itime, imrcb) 
C
C* Find error if any
C
                    IF (imrca .NE. CLIM_Ok .OR. 
     $                  imrcb .NE. CLIM_Ok) THEN
                        CALL prcout
     $   ('Problem in reading encapsulated infos for field', clname, 1)
                      ELSE
                        CALL prcout
     $   ('Read encapsulated infos in CLIM channel for field', 
     $                        clname, 1)
                    ENDIF
                ENDIF 
                CALL CLIM_Import 
     $              (clname, kiter, fldold(iadrold), info(jf))
                infos = infos + info(jf)
              ELSE
                CALL prcout 
     $              ('Wrong channel option for field', clname, 1)
            ENDIF
            IF (lmodinf .AND. nlogprt .GE. 2) THEN 
                WRITE (UNIT = nulou,FMT = *) 
     $              ' Encapsulated data for current field is :' 
                CALL prcout ('Experiment name', cljobnam, 1)
                CALL prtout ('Initial date', itime(1), 2)
                CALL prtout ('Iteration number', itime(2), 2)
                CALL prtout ('Time since start', itime(3), 2)
            ENDIF 
        ENDIF 
 210  CONTINUE
C
C* Stop if problem in reading in CLIM case
C
      IF (infos .NE. CLIM_Ok) THEN 
          DO 220 jf = 1, kfield
            IF (info(jf) .NE. CLIM_Ok) THEN
                iloc = kindex(jf)
                clname = cnaminp(iloc)
                CALL prcout
     $              ('WARNING: problem in reading field on port',
     $              clname, 1)
                CALL prtout
     $              ('CLIM error code number', info(jf), 2)
            ENDIF
 220      CONTINUE
          CALL HALTE ('STOP in getfld') 
      ENDIF
C
C* Stop if problem in reading in PIPE case
C
      IF (iflags .NE. 0) THEN 
          DO 230 jf = 1, kfield
            IF (iflag(jf) .NE. 0) THEN
                iloc = kindex(jf)
                iunit = nluinp(iloc)
                clname = cnaminp(iloc)
                CALL prcout
     $              ('WARNING: problem in reading field',
     $              clname, 1)
                CALL prtout
     $              ('Error reading logical unit', iunit, 2)
            ENDIF
 230      CONTINUE
          CALL HALTE ('STOP in getfld')
      ENDIF
C
C* Stop if problem in reading in SIPC or GMEM case
C
      IF (istop .NE. 0) CALL HALTE ('STOP in getfld')
C
C* Formats
C
 2100 FORMAT(/,5X,' Unit ',I2,' has been connected to file ',A8)
 2200 FORMAT(/,5X,' Unit ',I2,' has been disconnected from file ',A8)
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN 
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine getfld ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END

