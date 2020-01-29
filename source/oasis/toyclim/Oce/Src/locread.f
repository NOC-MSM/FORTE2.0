      SUBROUTINE locread ( cdfldn, pfield, kdimax, knulre, kflgre
     $                   , kout)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *locread*  - Read binary field on unit knulre
C
C     Purpose:
C     -------
C     Find string cdfldn on unit knulre and read array pfield
C
C**   Interface:
C     ---------
C       *CALL*  *locread (cdfldn, pfield, kdimax, knulre, kflgre, kout)*
C
C     Input:
C     -----
C                cdfldn : character string locator
C                kdimax : dimension of field to be read 
C                knulre : logical unit to be read 
C                kout   : logical unit to write messages
C
C     Output:
C     ------
C                pfield : field array (real 1D)
C                kflgre : error status flag
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
      IMPLICIT CHARACTER(c)
      IMPLICIT LOGICAL(l)
C
C* ---------------------------- Argument declarations -------------------
C
      REAL pfield(kdimax)
      CHARACTER*8 cdfldn
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
      WRITE (UNIT = kout,FMT = 1001) knulre
C
C* Formats
C
 1001 FORMAT('Locread : Read binary file connected to unit = ',I3)
C
C     2. Find field in file
C        ------------------
C
      REWIND knulre
 200  CONTINUE
C* Find string
      READ (UNIT = knulre, ERR = 200, END = 210) clecfl
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
      WRITE (UNIT = kout,FMT = *) 'Locread : done'
      CALL FLUSH (kout)
      RETURN
      END


