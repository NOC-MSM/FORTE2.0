      SUBROUTINE locwrith (cdfldn, cdjob, ktime, pfield, 
     $                     kdimax, knulre, kflgre)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *locwrith*  - Write binary field and header on unit knulre
C
C     Purpose:
C     -------
C     Write string cdfldn, header and array pfield on unit knulre
C
C**   Interface:
C     ---------
C       *CALL*  *locwrith (cdfldn, pfield, cdjob, ktime,
C                          kdimax, knulre, kflgre)*
C
C     Input:
C     -----
C                cdfldn : character string locator
C                cdjob  : experiment name
C                ktime  : header array (integer 1D)
C                kdimax : dimension of field to be written 
C                knulre : logical unit to be written
C                pfield : field array (real 1D) 
C
C     Output:
C     ------
C                kflgre : error status flag
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
C     See OASIS manual (1997) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.2       L. Terray      97/12/14  created
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      REAL pfield(kdimax)
      INTEGER ktime(3)
      CHARACTER*8 cdfldn
      CHARACTER*3 cdjob
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
     $    '           ROUTINE locwrith  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ****************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = 1001) knulre
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Formats
C
 1001 FORMAT(5X,' Write binary file connected to unit = ',I3)
C
C     2. Write header and field to file
C        ------------------------------
C
C* Write string and header
      WRITE (UNIT = knulre, ERR = 210) cdfldn, cdjob, ktime
C* Write associated field
      WRITE (UNIT = knulre, ERR = 210) pfield
C* Writing done and ok
      kflgre = 0
      GO TO 220
C* Problem in Writing
 210  kflgre = 1
 220  CONTINUE
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine locwrith ---------'
          WRITE (UNIT = nulou,FMT = *) ' '
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


