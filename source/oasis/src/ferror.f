      SUBROUTINE ferror
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL C *
C               * -------------     ------- *
C               *****************************
C
C**** *ferror*  - Signal handler
C
C     Purpose:
C     -------
C     ferror is executed each time signal sigcld is caught.
C     It stops execution of run due to child process death.
C
C**   Interface:
C     ---------
C       *CALL*  *fsigctl('register','sigcld',ferror)* (PIPE case)
C       *CALL*  *signal(nsigcld,ferror,ncatch)* (SIPC or GMEM case)
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
C     wait, waitcld
C
C     Reference:
C     ---------
C     See OASIS manual (1997)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       1.0       L. Terray      94/01/01  created
C       2.0       L. Terray      95/12/20  modified: new structure
C       2.2       L. Terray      97/10/10  added: Include nproc.h
C       2.2       S. Valcke, L.T 97/12/24  added: call wait and waitcld 
C       2.3       L. Terray      99/09/15  added: GMEM branch
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'experiment.h'
      INCLUDE 'nproc.h'
      INCLUDE 'hardware.h'
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER wait
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Signal sigcld has been caught; check which process is dead
C        ----------------------------------------------------------
C
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' ROUTINE ferror  -  Level C'
      WRITE (UNIT = nulou,FMT = *) ' **************     *******'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' Catch child process death'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      IF (cchan .EQ. 'PIPE' .AND. cmach .EQ. 'CRAY') THEN 
          iwone = wait(isone)
        ELSE IF ((cchan .EQ. 'SIPC' .OR. cchan .EQ. 'GMEM')
     $        .AND. cmach .EQ. 'IEEE') THEN 
          CALL waitcld (isone, iwone)
      ENDIF 
      DO 110 jm = 1, nmodel
        IF (iwone .EQ. nproc(jm)) THEN 
            WRITE (UNIT = nulou,FMT = *) 
     $          ' WARNING: sigcld has been caught by coupler'
            WRITE (UNIT = nulou,FMT = *) 
     $          ' =======  ======                    ======='
            WRITE (UNIT = nulou,FMT = *) 
     $          ' model ', cmodnam(jm), 
     $          ' process pid = ',nproc(jm),' is dead'
            WRITE (UNIT = nulou,FMT = *) ' '
            WRITE (UNIT = nulou,FMT = *) 
     $          ' We STOP the simulation now !!!'
            WRITE (UNIT = nulou,FMT = *) ' '
        ENDIF 
 110  CONTINUE 
c     CALL FLUSH (nulou)
C
C
C*    2. End of simulation if abnormal termination
C        -----------------------------------------
C
      CALL HALTE ('STOP in ferror')
C
C
C*    3. End of routine
C        --------------
C
      RETURN
      END
