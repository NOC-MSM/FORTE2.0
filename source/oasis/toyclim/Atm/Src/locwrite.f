      SUBROUTINE locwrite (cdfldn, pfield, kdimax, knulre, kflgre, kout)
      IMPLICIT none
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *locwrite*  - Write binary field on unit knulre
C
C     Purpose:
C     -------
C     Write string cdfldn and array pfield on unit knulre
C
C**   Interface:
C     ---------
C       *CALL*  *locwrite (cdfldn, pfield, kdimax, knulre, kflgre, kout)*
C
C     Input:
C     -----
C                cdfldn : character string locator
C                kdimax : dimension of field to be written 
C                knulre : logical unit to be written
C                pfield : field array (real 1D) 
C                kout   : logical unit to write messages
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
C     See OASIS manual (1995) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      95/09/01  created
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C
C* ---------------------------- Argument declarations -------------------
C
      INTEGER kdimax, knulre, kflgre, kout
      REAL pfield(kdimax)
      CHARACTER*8 cdfldn
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      WRITE (UNIT = kout,FMT = 1001) knulre
C
C* Formats
C
 1001 FORMAT(5X,' Write binary file connected to unit = ',I3)
C
C     2. Find field in file
C        ------------------
C
C* Write string
      WRITE (UNIT = knulre, ERR = 210) cdfldn
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
      WRITE (UNIT = kout,FMT = *) 'Locwrite : done'
      CALL FLUSH (kout)
      RETURN
      END


