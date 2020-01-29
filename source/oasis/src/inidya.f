      SUBROUTINE inidya
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *inidya*  - Initialize simulated dynamic allocation
C
C     Purpose:
C     -------
C     Set up pseudo-dynamic allocation of arrays.
C     Define adresses and lengths of small arrays in big arrays
C
C**   Interface:
C     ---------
C       *CALL*  *inidya*
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
C     ineed
C
C     Externals:
C     ---------
C     chkmem
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
      INCLUDE 'field.h'
      INCLUDE 'string.h'
      INCLUDE 'memory.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER ineed(2)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Set up memory dynamic allocation
C        --------------------------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE inidya  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Set up memory dynamic allocation'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Zero size and adress arrays
C
      CALL izero (nsizold, jpfield)
      CALL izero (nsiznew, jpfield)
      CALL izero (nadrold, jpfield)
      CALL izero (nadrnew, jpfield)
C
C* Zero main storage area
C
C* - Integer arrays: masks
C
      CALL izero (mskold, jpmxold)
      CALL izero (msknew, jpmxnew)
C
C* - Real arrays: fields, grids, surfaces
C
      CALL szero (fldold, jpmxold)
      CALL szero (fldnew, jpmxnew)
      CALL szero (xgrold, jpmxold)
      CALL szero (ygrold, jpmxold)
      CALL szero (xgrnew, jpmxnew)
      CALL szero (ygrnew, jpmxnew)
      CALL szero (surold, jpmxold)
      CALL szero (surnew, jpmxnew)

C
C* Get the sizes for each small array
C
      DO 110 jf = 1, nfield
        nsizold(jf) = nlonbf(jf) * nlatbf(jf)
        nsiznew(jf) = nlonaf(jf) * nlataf(jf)
 110  CONTINUE
C
C* Get the pointers for each small array
C
      nadrold(1) = 1
      nadrnew(1) = 1
      DO 120 jf = 2, nfield
        nadrold(jf) = nadrold(jf-1) + nsizold(jf-1)
        nadrnew(jf) = nadrnew(jf-1) + nsiznew(jf-1)
 120  CONTINUE
C
C* Initialize main memory
C
      navail(1) = memtot(1)
      navail(2) = memtot(2)
C
C* Check if enough memory
C
      ineed(1) = nadrold(nfield) + nsizold(nfield) - 1
      ineed(2) = nadrnew(nfield) + nsiznew(nfield) - 1
      CALL chkmem (ineed)
C
C
C*    2. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine inidya ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


