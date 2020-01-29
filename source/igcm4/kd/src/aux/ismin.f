C
      INTEGER FUNCTION ISMIN(N,SX,INCX)
      REAL SX(*)
      II=1
      AMIN=SX(1)
      DO I=INCX+1,N
         IF (SX(I).LT.AMIN) THEN
            AMIN=SX(I)
            II=I
         ENDIF
      ENDDO
      ISMIN=II
      END
