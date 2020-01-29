      SUBROUTINE chkmem (kneed)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *chkmem*  - Check job memory size
C
C     Purpose:
C     -------
C     Check if required storage does not exceed available memory
C
C**   Interface:
C     ---------
C       *CALL*  *chkmem (kneed)*
C
C     Input:
C     -----
C                kneed : needed amount of memory (integer 1D)
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
C     None
C
C     Reference:
C     ---------
C     See OASIS manual (1995) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      95/08/23  created 
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'memory.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      INTEGER kneed(2)
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER icore(2)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Check memory dynamic allocation
C        -------------------------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE chkmem  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Check job memory size'
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF

C
C* Get current requested memory
C
C - Initial field
C
      icore(1) = nused(1) + kneed(1)
C
C - Final field
C
      icore(2) = nused(2) + kneed(2)
C
C*    Print out required memory
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) kneed(1), navail(1), 
     $                                kneed(2), navail(2)
      ENDIF
C
C* If too much core memory needed, we stop
C      
      IF (icore(1) .GT. memtot(1) .OR. icore(2) .GT. memtot(2)) THEN
          WRITE (UNIT = nulou,FMT = 1002)
          CALL HALTE ('STOP in chkmem')
      END IF
C
C* Otherwise add this request to storage and return
C
      nused(1) = icore(1)
      nused(2) = icore(2)
      navail(1) = memtot(1) - nused(1)
      navail(2) = memtot(2) - nused(2)
C
C* Formats
C
 1001 FORMAT(1H ,5X,'Amount of INITIAL field memory REQUESTED',I10,/,
     $       1H ,5X,'Amount of INITIAL field memory AVAILABLE',I10,
     $     /,1H ,5X,'Amount of  FINAL  field memory REQUESTED',I10,/,
     $       1H ,5X,'Amount of  FINAL  field memory AVAILABLE',I10)
 1002 FORMAT(20X,' ******************** STOP ********************** ',
     $    /, 20X,' ************ NOT ENOUGH CORE STORAGE *********** ',
     $    /, 20X,' ******************** STOP ********************** ')
C
C
C*    2. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine chkmem ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


