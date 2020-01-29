      SUBROUTINE parse (cdone, cdtwo, knumb, klen, kleng)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL T *
C               * -------------     ------- *
C               *****************************
C
C**** *parse*  - Parsing routine
C
C     Purpose:
C     -------
C     Find the knumb'th string in cdone and put it in cdtwo.
C     A string is defined as a continuous set of non-blanks characters
C
C**   Interface:
C     ---------
C       *CALL*  *parse (cdone, cdtwo, knumb, klen, kleng)*
C
C     Input:
C     -----
C                cdone : line to be parsed (char string)
C                knumb : rank within the line of the extracted string (integer)
C                klen  : length of the input line (integer)
C
C     Output:
C     ------
C                cdtwo : extracted character string (char string)
C                kleng : length of the extracted string (integer)
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     nextbl, nextch, ilenstr
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
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
C
C* ---------------------------- Argument declarations -------------------
C
      CHARACTER*1 cdone, cdtwo
      DIMENSION cdone(klen), cdtwo(klen)
C
C* ---------------------------- Local declarations -------------------
C
      CHARACTER*80 clline
      CHARACTER*1 clblank, clcmt 
      DATA clblank/' '/, clcmt/'#'/     
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Skip line if it is a comment
C        ----------------------------
C
 100  IF (cdone(1) .NE. clcmt) GO TO 120
      READ (UNIT = nulin, FMT = 1001) clline
      DO 110 jc = 1, klen
        cdone(jc) = clline(jc:jc)
 110  CONTINUE
      GO TO 100
 120  CONTINUE 
 1001 FORMAT(A80)
C
C
C*    2. Do the extraction job
C        ---------------------
C
C* - Fill cdtwo with blanks
C
      DO 210 ji = 1, klen
        cdtwo(ji) = clblank
  210 CONTINUE 
C
C* - Find the first character
C
      ii = nextch (cdone, 1, klen)
C
C* - If there is nothing on this line make kleng=-1
C
      IF (ii .LT. 0) THEN
        kleng = -1
        RETURN
      END IF
C
C* - If this is the one we're looking for, skip
C    otherwise go knumb-1 more sets of characters
C
      IF (knumb .GE. 2) THEN
        DO 220 jc = 2, knumb
          ii = nextbl (cdone, ii, klen)
          ii = nextch (cdone, ii, klen)
C
C* - If there are no more characters, kleng=-1
C
          IF (ii .LT. 0) THEN
            kleng = -1
            RETURN
          END IF
  220   CONTINUE
      END IF
C
C* - Find the length of this set of characters
C
      kleng = ilenstr (cdone(ii), klen -ii)
C
C* - Copy to cdtwo
C
      DO 230 jj = 1, kleng
        cdtwo(jj) = cdone(ii +jj -1)
  230 CONTINUE
C
C
C*    3. End of routine
C        --------------
C
      RETURN
      END
