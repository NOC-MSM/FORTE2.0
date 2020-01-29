      SUBROUTINE updtim (kiter)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *updtim*  - Update time
C
C     Purpose:
C     -------
C     Increment date since run start
C
C**   Interface:
C     ---------
C       *CALL*  *updtim (kiter)*
C
C     Input:
C     -----
C                kiter : iteration number
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C                ilmoi  : work array for calendar months length (integer 1D)
C
C     Externals:
C     ---------
C     calend
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
C       2.0       L. Terray      95/09/01  modified: new structure
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'timestep.h'
      INCLUDE 'calendar.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER ilmoi(12)
C
C* ---------------------------- Local functions -------------------------
C
C* - Time functions
C            ncth  --->>> turn seconds into hours
C
      ncth(ksec) = ksec / 3600
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
     $    '           ROUTINE updtim  -  Level 1'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Increment model date since run start '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Check if we go to next day
C
C* Simulated time in seconds
C
      isecond = kiter * nstep
      zsecond = float (isecond)
C
C* Get number of days completed so far
C
      iday = int (zsecond / 86400.)
C
C* Get seconds gone for current day
C
      idaysec = iday * 86400
      ihour = ncth (isecond - idaysec)
C
C
C*    2. Get time data
C        -------------
C
      CALL calend (njini, nmini, naini, iday, 
     $             njnow, nmnow, nanow, ilmoi)
C
C
C*    3. Print current date and time
C        ---------------------------
C
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    '               ****************** '
      WRITE (UNIT = nulou,FMT = *) 
     $    '               * Model run date * '
      WRITE (UNIT = nulou,FMT = *) 
     $    '               * -------------- * '
      WRITE (UNIT = nulou,FMT = *) 
     $    '               ****************** '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) '            Year  --->>> ',nanow
      WRITE (UNIT = nulou,FMT = *) '           Month  --->>> ',nmnow
      WRITE (UNIT = nulou,FMT = *) '             Day  --->>> ',njnow
      WRITE (UNIT = nulou,FMT = *) '            Hour  --->>> ',ihour
      WRITE (UNIT = nulou,FMT = *) ' '
      IF (njnow .GT. 15) THEN
          nmone = nmnow
          nmtwo = 1 + MOD(nmnow,12)
          njone = 15
          njtwo = 15 + ilmoi(nmone)
          ndone = 15
          ndtwo = ilmoi(nmone)
          nsrec = nmnow
          IF (njdeb .LT. 15) THEN
              nmrec = nmnow + 12 * (nanow - nadeb) - nmdeb + 1
            ELSE 
              nmrec = nmnow + 12 * (nanow - nadeb) - nmdeb
          ENDIF 
        ELSE
          nmone = 1 + MOD(nmnow + 10,12)
          nmtwo = nmnow
          njone = 15 - ilmoi(nmone)
          njtwo = 15
          ndone = 0
          ndtwo = 15
          nsrec = nmnow - 1
          IF (njdeb .LT. 15) THEN 
              nmrec = nmnow + 12 * (nanow - nadeb) - nmdeb
            ELSE 
              nmrec = nmnow - 1 + 12 * (nanow - nadeb) - nmdeb
          ENDIF
      ENDIF
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine updtim ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


