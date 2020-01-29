      SUBROUTINE halte (cdtext)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL C *
C               * -------------     ------- *
C               *****************************
C
C**** *halte*  - Abort the program
C
C     Purpose:
C     -------
C     Print an error message to standard output and abort the coupler
C
C**   Interface:
C     ---------
C       *CALL*  *halte (cdtext)*
C
C     Input:
C     -----
C                cdtext   : character string to be printed
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
C     See OASIS 2.2 manual (1997) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.2       S. Valcke      97/11/18  created
C       2.3       L. Terray      99/02/24  modified: X format for NEC
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
C
C* ---------------------------- Argument declarations ----------------------
C
      CHARACTER*(*) cdtext
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER cpbase
      CHARACTER*10 cprpt, cpdots
      CHARACTER*69 cline
      PARAMETER ( cpbase = '-' )
      PARAMETER ( cprpt = '* ===>>> :' )
      PARAMETER ( cpdots = '  ------  ' )
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Print text 
C        ----------
C
          cline = ' '
          ilen = len(cdtext)
          DO 110 jl = 1, ilen
            cline(jl:jl) = cpbase
 110      CONTINUE
          WRITE(UNIT = nulou,FMT='(/,A,1X,A)') cpdots, cline
          WRITE(UNIT = nulou,FMT='(/,A,1X,A,/)') cprpt, cdtext
          WRITE(UNIT = nulou,FMT='(A,1X,A,/)')cpdots, cline
C
C
C*    2. FLUSH the coupler output
C        ------------------------
C
c     CALL FLUSH (nulou)
C
C
C*    3. Abort the coupler
C        -----------------
C
      CALL abort
      RETURN
      END
