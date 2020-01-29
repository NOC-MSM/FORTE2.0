      SUBROUTINE revmsk (kfild ,kxlon ,kylat, cdxord, cdyord)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *revmsk* - Ordering routine
C
C     Purpose:
C     -------
C     Reorder integer field according to cdxord and cdyord information
C
C**   Interface:
C     ---------
C       *CALL*  *revmsk (kfild ,kxlon ,kylat, cdxord, cdyord)*
C
C     Input:
C     -----
C                kfild : field to be reordered (integer 2D)
C                kxlon : number of longitudes
C                kylat : number of latitudes
C                cdxord : longitude ordering
C                cdyord : latitude ordering
C
C     Output:
C     ------
C                kfild : field reordered (integer 2D)
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
C       2.0       L. Terray      95/10/10  created
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
      INTEGER kfild(kxlon,kylat)
      CHARACTER*8 cdxord, cdyord
C
C* ---------------------------- Local declarations ----------------------
C
      LOGICAL llxord, llyord
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
     $    '           ROUTINE revmsk     Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Reorder mask field North ---> South '
          WRITE (UNIT = nulou,FMT = *) 
     $    '      and from Greenwhich ---> West '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' if necessary for glored analysis'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      llxord = cdxord .EQ. 'SUDNOR'
      llyord = cdyord .EQ. 'WSTEST'
C
C
C*    2. Reorder field 
C        -------------
C* South-North buisness
C
      IF ( .NOT. llxord) THEN
          ijmed = kylat/2 
          DO 210 jj = 1, ijmed
            DO 220 ji = 1, kxlon
              ifild = kfild(ji,kylat + 1 - jj)
              kfild(ji,kylat + 1 - jj) = kfild(ji,jj)
              kfild(ji,jj) = ifild
 220        CONTINUE
 210      CONTINUE
      ENDIF 
C
C* East-West one
C
      IF ( .NOT. llyord) THEN
          iimed = kxlon/2 
          DO 230 jj = 1, kylat
            DO 240 ji = 1, iimed
              ifild = kfild(kxlon + 1 - ji,jj)
              kfild(kxlon + 1 - ji,jj) = kfild(ji,jj)
              kfild(ji,jj) = ifild
 240        CONTINUE
 230      CONTINUE
      ENDIF
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine revmsk ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
