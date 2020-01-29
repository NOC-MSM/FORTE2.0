      SUBROUTINE masq (pfild, ksize, pmask, kmask)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *masq* - Mask routine
C
C     Purpose:
C     -------
C     Mask field with chosen value
C
C     NB: Note that there is no include on file doctor.h in order
C         to compile on a workstation (cvmgp)
C
C**   Interface:
C     ---------
C       *CALL*  *masq (pfild, ksize, pmask, kmask)*
C
C     Input:
C     -----
C                pfild : field to be masked (real 1D)
C                ksize : array size
C                pmask : mask value given from input
C                kmask : mask array : 0 ---> ocean , 1 ---> land (integer 1D)
C
C     Output:
C     ------
C                pfild : field masked (real 1D)
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     cvmgp
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
C       2.0beta   L. Terray      95/10/01  modified: new structure
C       2.0       L. Terray      96/02/01  modified: suppression of file
C                                                    doctor.h
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'unit.h'
      INCLUDE 'hardware.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      REAL pfild(ksize)
      INTEGER kmask(ksize)
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
     $    '           ROUTINE masq  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Mask field with input value'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C
C*    2. Mask field with value
C        ---------------------
C
      IF (cmach .EQ. 'CRAY') THEN 
          DO 210 jk = 1, ksize
            pfild(jk) = cvmgp (pfild(jk),pmask,
     $                  float(1 - kmask(jk)) -.5)
 210      CONTINUE
        ELSE 
          DO 220 jk = 1, ksize
            ztest = float(1 - kmask(jk)) -.5
            IF (ztest .LT. 0.) pfild(jk) = pmask
 220      CONTINUE 
      ENDIF 
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine masq ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
