      SUBROUTINE inicmc
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL C *
C               * -------------     ------- *
C               *****************************
C
C**** *inicmc*  - Initialize coupled mode communication for coupler
C
C     Purpose:
C     -------
C     Use either PIPE, CLIM, SVIPC or GMEM library to start communication with
C     the models being coupled. The PIPE library uses named pipes(fifo)
C     while CLIM uses sockets based on PVM 3.3.
C     The SVIPC library uses shared memory segments based on system V IPC.
C     The GMEM library uses the global memory concept of NEC machines.
C     In cases SVIPC, GMEM or PIPE, signal handling is implemented to trap 
C     oasis or child status changes.
C
C**   Interface:
C     ---------
C       *CALL*  *inicmc*
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
C     None
C
C     Externals:
C     ---------
C     (CLIM-PIPE-SIPC)_Init, (CLIM-PIPE-SIPC)_Define, (CLIM-PIPE-SIPC)_Stepi, 
C     CLIM_Start, chksgc
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
C       2.0       L. Terray      95/09/01  modified : new structure
C       2.1       L. Terray      96/09/03  mofified : norm DOCTOR (loop
C                                          index jp --> jl)
C       2.2       S. Valcke      97/06/20  added: introduction of SVIPC
C       2.2       L. Terray      97/09/20  general cleaning + call chksgc
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       L. Terray      99/09/15  added: GMEM branch
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'parameter.h'
      INCLUDE 'unit.h'
      INCLUDE 'experiment.h'
      INCLUDE 'field.h'
      INCLUDE 'clim.h'
      INCLUDE 'parallel.h'
      INCLUDE 'string.h'
      INCLUDE 'hardware.h'
      INCLUDE 'timestep.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER info(jpfield), iparal(jpparal)
      CHARACTER*8 clpinp, clpout
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
     $    '           ROUTINE inicmc  -  Level C'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Process stuff initialization'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Set up signal handling
C
      CALL chksgc
C
C* Initialize error codes
C
      CALL izero (info, jpfield)
C
C* Total number of models is nmodel + coupler = nmodel + 1
C
      IF (cchan .EQ. 'CLIM') THEN
          ione = 1
C
C* CLIM initialization
C
          CALL CLIM_Init (cjobnam, 'Oasis', nmodel+1, nultr, niter,
     $                    ione , nstep, ntiret, ntiogp, ntiout,
     $                    infos)
          IF (infos .NE. CLIM_Ok) THEN 
              WRITE (UNIT = nulou,FMT = *) 
     $            ' WARNING : Problem with CLIM initialization '
              WRITE (UNIT = nulou,FMT = *)
     $            ' =======   Error code number = ',infos
              CALL HALTE('STOP in inicmc')
          ENDIF
        ELSE IF (cchan .EQ. 'PIPE' .AND. cmach .EQ. 'CRAY') THEN
C
C* Open pipes between models to exchange checking data
C
          infos = 0
          CALL PIPE_Init (cjobnam, cmodnam, nmodel, infos)
          IF (infos .NE. 0) THEN 
              WRITE (UNIT = nulou,FMT = *) 
     $            ' WARNING : Problem with model pipe initialization'
              WRITE (UNIT = nulou,FMT = *)
     $            ' =======   Error code number = ',infos
              CALL HALTE('STOP in inicmc')
          ENDIF
C    
C* SVIPC or GMEM initialization
C
        ELSE IF ((cchan .EQ. 'SIPC' .OR. cchan .EQ. 'GMEM')
     $        .AND. cmach .EQ. 'IEEE') THEN
C
C* Open shared memory pools used by models to exchange initial infos 
C
          infos = 0
          CALL SIPC_Init (cjobnam, cmodnam, nmodel, infos)
          IF (infos .NE. 0) THEN 
              WRITE (UNIT = nulou,FMT = *)  
     $ 'WARNING : Problem with model shared memory pool initialization'
              WRITE (UNIT = nulou,FMT = *)
     $ ' =======   Error code number = ',infos
              CALL HALTE('STOP in inicmc')
          ENDIF
C
C* No message passing case
C
        ELSE IF (cchan .EQ. 'NONE') THEN 
            IF (nlogprt .GE. 1) THEN
                WRITE (UNIT = nulou, FMT = *) 
     $          ' NOTE : No message passing used in this run'
                WRITE (UNIT = nulou,FMT = *)
     $          ' ======= '
            ENDIF
        ELSE
          CALL prcout ('WARNING: options incompatible 
     $                 for simulation', cjobnam, 1)
          CALL prcout ('CHANNEL option is :', cchan, 1)
          CALL prcout ('MACHINE option is :', cmach, 1)
      ENDIF 
C
C
C*    2. Define ports or files for data exchange
C        ---------------------------------------
C
C* CLIM case: define ports
C
      IF (cchan .EQ. 'CLIM') THEN
C
C* Input ports for incoming fields
C
          infos = CLIM_Ok
          DO 210 jf = 1, nfield
            DO 220 jl = 1, CLIM_ParSize
              iparal(jl) = nparal(jl,jf)
 220        CONTINUE
            CALL CLIM_Define (cnaminp(jf), CLIM_In, CLIM_Double,
     $          iparal, info(jf))
            infos = infos + info(jf)
 210      CONTINUE
          IF (infos .NE. CLIM_Ok) THEN
              DO 230 jf = 1, nfield
                IF (info(jf) .NE. CLIM_Ok) THEN
                    WRITE (UNIT = nulou,FMT = *) 
     $                  ' WARNING : Problem with port ', cnaminp(jf)
                    WRITE (UNIT = nulou,FMT = *)
     $                  ' =======   Error code number = ',info(jf)
                ENDIF 
 230          CONTINUE 
              CALL HALTE ('STOP in inicmc')
          ENDIF 
C
C* Output ports for outgoing fields : serial decomposition
C
          iparal(CLIM_Strategy) = CLIM_Serial
          iparal(CLIM_Offset) = 0
          DO 240 jf = 1, nfield
            iparal(CLIM_Length) = nlonaf(jf) * nlataf(jf)
            CALL CLIM_Define (cnamout(jf), CLIM_Out, CLIM_Double,
     $          iparal, info(jf))
            infos = infos + info(jf)
 240      CONTINUE 
          IF (infos .NE. CLIM_Ok) THEN
              DO 250 jf = 1, nfield
                IF (info(jf) .NE. CLIM_Ok) THEN
                    WRITE (UNIT = nulou,FMT = *) 
     $                  ' WARNING : Problem with port ', cnamout(jf)
                    WRITE (UNIT = nulou,FMT = *)
     $                  ' =======   Error code number = ',info(jf)
                ENDIF 
 250          CONTINUE 
              CALL HALTE ('STOP in inicmc')
          ENDIF
      ENDIF
C
C* PIPE case: define and open pipe files (FIFO)
C
      IF (cchan .EQ. 'PIPE') THEN
          infos = 0
          DO 260 jf = 1, nfield
C
C* Define pipes actual names for each field
C
C - Reading
C
            clpinp = cnaminp(jf)
C - Writing
            clpout = cnamout(jf)
C
C* Create pipes
C
            CALL PIPE_Define (cnaminp(jf), cnamout(jf), 
     $                        clpinp, clpout , info(jf))
            infos = infos + info(jf)
 260      CONTINUE
          IF (infos .NE. 0) THEN 
              WRITE (UNIT = nulou,FMT = *) 
     $            ' WARNING : Problem with field pipe initialization'
              WRITE (UNIT = nulou,FMT = *)
     $            ' =======   Error code number = ',infos
              CALL HALTE('STOP in inicmc')
          ENDIF
      ENDIF
C
C* SIPC case
C
      IF (cchan .EQ. 'SIPC' .OR. cchan .EQ. 'GMEM') THEN
          infos = 0
          DO 270 jf = 1, nfield
C
C* Create two shared memory pools for each field
C
            CALL SIPC_Define (jf, info(jf))  
            infos = infos + info(jf)
 270      CONTINUE
          IF (infos .NE. 0) THEN 
              WRITE (UNIT = nulou,FMT = *) 
     $  'WARNING : Problem with field shared memory pool initialization'
              WRITE (UNIT = nulou,FMT = *)
     $  ' =======   Error code number = ',infos
              CALL HALTE('STOP in inicmc')
            ELSE
C
C* If everything went alright, open one dummy file signaling  
C  that pools for exchange are opened
C
              OPEN (UNIT = nudum, FILE = 'DUMMY_SIPC', STATUS 
     $              = 'NEW')
              CLOSE (UNIT = nudum)
              IF (nlogprt .GE. 1) THEN
                  WRITE (UNIT = nulou,FMT = *)
     $              '* ===>>> : file DUMMY_SIPC created'
                  WRITE (UNIT = nulou,FMT = *) 
     $              '  ------   -----------------------'
              ENDIF
          ENDIF
      ENDIF
C
C
C*    3. Start the communication and get timestep information
C        ----------------------------------------------------
C* CLIM CASE
C
      IF(cchan .EQ. 'CLIM') THEN 
          CALL CLIM_Start (imxtag, infos)
          IF (infos .NE. CLIM_Ok) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            ' WARNING : Problem in starting CLIM '
              WRITE (UNIT = nulou,FMT = *)
     $            ' =======   Error code number = ',infos
          ELSE
              IF (nlogprt .GE. 1) THEN 
                  WRITE (UNIT = nulou,FMT = *)
     $            ' CLIM starts OK. PVM Max tag = ',imxtag
              ENDIF
          ENDIF
          DO 310 jm = 1, nmodel
            CALL CLIM_Stepi (cmodnam(jm), istep, ifcpl, idt, infos)
            IF (infos .NE. CLIM_Ok) THEN
                WRITE (UNIT = nulou,FMT = *) 
     $              ' WARNING : Problem in getting step info 
     $                from model ', cmodnam(jm)
                WRITE (UNIT = nulou,FMT = *)
     $              ' =======   Error code number = ',infos
            ELSE
                IF (nlogprt .GE. 1) THEN  
                    WRITE (UNIT = nulou,FMT = *)
     $              ' Got step information from model ', cmodnam(jm)
                ENDIF
            ENDIF
            mstep(jm) = istep
            mfcpl(jm) = ifcpl
            mdt(jm) = idt
 310      CONTINUE 
      ENDIF
      IF (cchan .EQ. 'PIPE') THEN
          infos = 0
          DO 320 jm = 1, nmodel
            CALL PIPE_Stepi (cmodnam(jm), jm, 
     $                       istep, ifcpl, idt, infos)
            IF (infos .NE. 0) THEN
                WRITE (UNIT = nulou,FMT = *) 
     $              ' WARNING : Problem in getting step info 
     $                from model ', cmodnam(jm)
                WRITE (UNIT = nulou,FMT = *)
     $              ' =======   Error code number = ',infos
                CALL HALTE('STOP in inicmc')
            ELSE
                IF (nlogprt .GE. 1) THEN
                    WRITE (UNIT = nulou,FMT = *)
     $              ' Got step information from model ', cmodnam(jm)
                ENDIF
            ENDIF
            mstep(jm) = istep
            mfcpl(jm) = ifcpl
            mdt(jm) = idt
 320      CONTINUE 
      ENDIF 
C
C* SIPC or GMEM Case
C
      IF (cchan .EQ. 'SIPC' .OR. cchan .EQ. 'GMEM') THEN
          infos = 0
          DO 330 jm = 1, nmodel
            CALL SIPC_Stepi (cmodnam(jm), jm, 
     $                       istep, ifcpl, idt, infos)
            IF (infos .NE. 0) THEN
                CALL prcout
     $           ('WARNING: Problem in getting step info from model', 
     $            cmodnam(jm), 1)
            CALL prtout('Error code number = ',infos,1)
                CALL HALTE('STOP in inicmc')
            ELSE
            CALL prcout
     $           ('Got step information from model ',
     $            cmodnam(jm), 1)
            ENDIF
            mstep(jm) = istep
            mfcpl(jm) = ifcpl
            mdt(jm) = idt
 330      CONTINUE 
      ENDIF
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine inicmc ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
