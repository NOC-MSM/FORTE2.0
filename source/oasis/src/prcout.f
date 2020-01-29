      SUBROUTINE prcout (cdtext, cdstring, kstyle)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *prcout*  - Print output
C
C     Purpose:
C     -------
C     Print out character string and one character value
C
C**   Interface:
C     ---------
C       *CALL*  *prcout (cdtext, cdstring, kstyle)*
C
C     Input:
C     -----
C                cdtext   : character string to be printed
C                cdstring : character variable to be printed
C                kstyle   : printing style
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
C     See OASIS manual (1995) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      95/10/01  created
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
      CHARACTER*(*) cdtext, cdstring
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
C*    1. Print character string + character value
C        ----------------------------------------
C
      IF ( kstyle .EQ. 1 .OR. kstyle .EQ. 2) THEN
          cline = ' '
          ilen = len(cdtext)
          DO 110 jl = 1, ilen
            cline(jl:jl) = cpbase
 110      CONTINUE
          IF ( kstyle .EQ. 2 ) THEN
              WRITE(UNIT = nulou,FMT='(/,A,1X,A)') cpdots, cline
          ENDIF
          WRITE(UNIT = nulou,FMT='(A,1X,A,1X,A)') 
     $        cprpt, cdtext, cdstring
          WRITE(UNIT = nulou,FMT='(A,1X,A,/)') cpdots, cline
        ELSE
          WRITE(UNIT = nulou,FMT='(/,A,1X,A,1X,A,/)') 
     $          cprpt, cdtext, cdstring
      ENDIF
C
C
C*    3. End of routine
C        --------------
C
c     CALL FLUSH (nulou)
      RETURN
      END
