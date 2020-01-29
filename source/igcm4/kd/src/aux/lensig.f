C     ****************************************************************
*DECK LENSIG
      INTEGER FUNCTION LENSIG(STRING)
C
C     Determine length of significant part of character string
C     by searching for first non-blank character from end.
C
C     Mike Blackburn     UGAMP     05.04.90.
C
      CHARACTER STRING*(*),BLANK*1
      BLANK=' '
      ICLEN=LEN(STRING)
      IL=ICLEN+1
      DO 10 I=ICLEN,1,-1
      IL=IL-1
      IF (STRING(I:I).NE.BLANK) GOTO 20
   10 CONTINUE
      IL=IL-1
   20 LENSIG=IL
      RETURN
      END
