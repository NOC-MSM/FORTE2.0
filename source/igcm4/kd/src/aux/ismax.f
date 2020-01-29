C
      INTEGER FUNCTION ISMAX(N,SX,INCX)
      REAL SX(*)
      II=1
      AMAX=SX(1)
      DO I=INCX+1,N
         IF (SX(I).GT.AMAX) THEN
            AMAX=SX(I)
            II=I
         ENDIF
      ENDDO
      ISMAX=II
      END
