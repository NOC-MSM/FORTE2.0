      SUBROUTINE calend (kdini, kmini, kyini, kinc, 
     $                   kdfin, kmfin, kyfin, klmo)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *calend*  - Calendar routine
C
C     Purpose:
C     -------
C     Updates the calendar values. Date is of the form YYYYMMDD.
C     Increases day by day the date, updates if necessary the month 
C     and the year.
C
C**   Interface:
C     ---------
C       *CALL*  *calend (kdini, kmini, kyini, kinc, 
C                        kdfin, kmfin, kyfin, klmo)*
C
C     Input:
C     -----
C                kdini,kmini,kyini : initial date (day,month,year)
C                kinc              : number of days to increment
C
C     Output:
C     ------
C                kdfin,kmfin,kyfin : final date
C                klmo(12)          : length of the 12 months
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     None
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      95/10/01  created
C       2.3       S. Valcke      99/03/15  year is leap if divible by 4 but
C                                          not by 100, leap if div. by 400
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
      INTEGER klmo(12)
C
C* ---------------------------- Local declarations -------------------
C
      LOGICAL lleap
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Length of the months
C        --------------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE calend  -  Level 1'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Updates calendar values'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Calculate month lengths for current year
C
      DO 110 jm = 1, 12
        klmo(jm) = 31
        IF (jm .eq. 4 .or. jm .eq. 6 .or.
     $      jm .eq. 9 .or. jm .eq. 11) THEN
            klmo(jm) = 30
        ENDIF
        IF (jm .eq. 2) THEN
C
C* Leap years
C
            lleap = .FALSE.
            IF (MOD(kyini,4) .eq. 0) lleap = .TRUE.
            IF (MOD(kyini,100) .eq. 0) lleap = .FALSE.
            IF (MOD(kyini,400) .eq. 0) lleap = .TRUE.
            IF (lleap) THEN
                klmo(jm) = 29
              ELSE
                klmo(jm) = 28
            ENDIF
        ENDIF
 110  CONTINUE
      kdfin = kdini
      kmfin = kmini
      kyfin = kyini
C
C
C*    2. Loop on the days
C        ----------------
C
      DO 210 jd = 1, kinc
        kdfin = kdfin + 1
        IF (kdfin .le. klmo(kmfin)) GOTO 210
        kdfin = 1
        kmfin = kmfin + 1
        IF (kmfin .le. 12) GOTO 210
        kmfin = 1
        kyfin = kyfin + 1
C
C* Leap years
C
        lleap = .FALSE.
        IF (MOD(kyfin,4) .eq. 0) lleap = .TRUE.
        IF (MOD(kyfin,100) .eq. 0) lleap = .FALSE.
        IF (MOD(kyfin,400) .eq. 0) lleap = .TRUE.
        IF (lleap) THEN
            klmo(2) = 29
          ELSE
            klmo(2) = 28
        ENDIF
 210  CONTINUE
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine calend ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
