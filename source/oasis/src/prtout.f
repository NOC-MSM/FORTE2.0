      SUBROUTINE prtout (cdtext, kvalue, kstyle)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C
C**** *prtout*  - Print output
C
C     Purpose:
C     -------
C     Print out character string and one integer value
C
C**   Interface:
C     ---------
C       *CALL*  *prtout (cdtext, kvalue, kstyle)*
C
C     Input:
C     -----
C                cdtext : character string to be printed
C                kvalue : integer variable to be printed
C                kstyle : printing style
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
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
      INCLUDE 'unit.h'
C
C* ---------------------------- Argument declarations ----------------------
C
      CHARACTER*(*) cdtext
      INTEGER kvalue, kstyle
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER cbase
      CHARACTER*10 cprpt, cdots
      CHARACTER*69 cline
      PARAMETER ( cbase = '-' )
      PARAMETER ( cprpt = '* ===>>> :' )
      PARAMETER ( cdots = '  ------  ' )
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Print character string + integer value
C        --------------------------------------
C
      IF ( kstyle .EQ. 1 .OR. kstyle .EQ. 2) THEN
          cline = ' '
          ilen = len(cdtext)
          DO 110 jl = 1, ilen
            cline(jl:jl) = cbase
 110      CONTINUE
          IF ( kstyle .EQ. 2 ) THEN
              WRITE(UNIT = nulou,FMT='(/,A,1X,A)') cdots, cline
          ENDIF
          WRITE(UNIT = nulou,FMT='(A,1X,A,1X,I8)') 
     $        cprpt, cdtext, kvalue
          WRITE(UNIT = nulou,FMT='(A,1X,A,/)') cdots, cline
        ELSE
          WRITE(UNIT = nulou,FMT='(/,A,1X,A,1X,I8,/)') 
     $          cprpt, cdtext, kvalue
      ENDIF
C
C
C*    2. End of routine
C        --------------
C
c     CALL FLUSH ( nulou )
      RETURN
      END
