      SUBROUTINE getfpe
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL C *
C               * -------------     ------- *
C               *****************************
C
C**** *getfpe*  - Signal handler
C
C     Purpose:
C     -------
C     getfpe is executed each time signal sigfpe is caught.
C     Then code stops due to floating point exception error.
C
C**   Interface:
C     ---------
C       *CALL*  *fsigctl ('register', 'sigfpe', getfpe)*
C       *CALL*  *signal(nsigfpe,getfpe,ncatch)* (SIPC or GMEM case)
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
C     None
C
C     Reference:
C     ---------
C     Epicoa 920203 (1992) and OASIS manual (1995) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       1.0       L. Terray      94/01/01  created
C       2.0       L. Terray      95/09/10  modified: new structure
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE getfpe  -  Level C'
      WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' catch fpe error '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
C
C
C*    2. Abort simulation in case of floating point error
C        ------------------------------------------------
C
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *) 
     $       ' ===>>> : fpe error has occurred in coupler'
      WRITE (UNIT = nulou,FMT = *) 
     $       ' ======   === =====                 ======='
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $       ' We STOP !!! Check non initialized variables'
c     CALL FLUSH (nulou)
      CALL HALTE ('STOP in getfpe')
C
C
C*    3. End of routine
C        --------------
C
      RETURN
      END
