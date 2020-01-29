      SUBROUTINE locreadh (cdfldn, cdjob, ktime, pfield, 
     $                     kdimax, knulre, kflgre)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *locreadh*  - Read header and binary field on unit knulre 
C
C     Purpose:
C     -------
C     Find string cdfldn on unit knulre and read header and array pfield
C
C**   Interface:
C     ---------
C       *CALL*  *locreadh (cdfldn, cdjob, ktime, pfield, 
C                          kdimax, knulre, kflgre)*
C
C     Input:
C     -----
C                cdfldn : character string locator
C                kdimax : dimension of field to be read 
C                knulre : logical unit to be read 
C
C     Output:
C     ------
C                cdjob  : experiment name
C                ktime  : header array (integer 1D)
C                pfield : field array (real 1D)
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
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*8 clecfl
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
     $    '           ROUTINE locreadh  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ****************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = 1001) knulre
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Formats
C
 1001 FORMAT(5X,' Read binary file connected to unit = ',I3)
C
C     2. Find field in file
C        ------------------
C
      REWIND knulre
 200  CONTINUE
C* Find string
      READ (UNIT = knulre, ERR = 200, END = 210) clecfl, cdjob, ktime
      IF (clecfl .NE. cdfldn) GO TO  200
C* Read associated field
      READ (UNIT = knulre, ERR = 210, END = 210) pfield
C* Reading done and ok
      kflgre = 0
      GO TO 220
C* Problem in reading
 210  kflgre = 1
 220  CONTINUE
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine locreadh ---------'
          WRITE (UNIT = nulou,FMT = *) ' '
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


