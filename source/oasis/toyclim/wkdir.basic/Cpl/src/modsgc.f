      SUBROUTINE modsgc
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL C *
C               * -------------     ------- *
C               *****************************
C
C**** *modsgc*  - Change sigcld handler
C
C     Purpose:
C     -------
C     Modify sigcld handler before signals are sent for the last time.
C     Use fsigctl (PIPE-CRAY case) or signal (SIPC case)
C     calls to change sigcld signal handler.
C     This is done to avoid a recursive I/O error.
C
C**   Interface:
C     ---------
C       *CALL*  *modsgc*
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
C     fsigctl, signal
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
C       2.0       L. Terray      95/10/01  modified: new structure
C       2.2       S. Valcke, L.T 97/11/13  Added: call signal
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'hardware.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- External declarations -------------------
C
      EXTERNAL ferror
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
     $    '           ROUTINE modsgc  -  Level C'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Change sigcld handler     '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C
C*    2. Modify sigcld signal handler in PIPE or SIPC case
C        -------------------------------------------------
C
      IF (cchan .EQ. 'PIPE' .AND. cmach .EQ. 'CRAY') THEN 
          CALL fsigctl ('IGNORE','SIGCLD',0)
        ELSE IF (cchan .EQ. 'SIPC' 
     $        .AND. cmach .EQ. 'IEEE') THEN
          CALL signal (nsigcld, ferror, nignore)
      ENDIF 
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine modsgc ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
