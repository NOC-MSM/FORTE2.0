      SUBROUTINE GWTLT(SIT,WEIGHT,J,JG)
      PARAMETER(PI=3.14159265359)
      DOUBLE PRECISION ACC,SA,SB,SC,D1,D2,D4,BN,GG,X,AMM,AMN,
     1 EM,ANN,RE,A,DD,DLT,DTH
      ACC=1.0D-16
      SA=DSQRT(.5D0)
      SB=DSQRT(1.5D0)
      SC=DSQRT(1.D0/3.D0)
      D1=1.D0
      D2=2.D0
      D4=4.D0
      NNN=JG+JG
      BN=NNN
      GG=D2*BN+D1
      HH=8.*BN*BN
      K=0
      AJ=J
      TH=PI*(2.*AJ-.5)/GG
      DTH=TH+(COS(TH)/SIN(TH))/HH
      X=DCOS(DTH)
54    CONTINUE
      AMM=SA
      AMN=X*SB
      EM=SC
      ANN=D1
      DO 51 N=2,NNN
      ANN=ANN+D1
      RE=DSQRT(D4*ANN*ANN-D1)/ANN
      A=RE*(X*AMN-EM*AMM)
      AMM=AMN
      AMN=A
      EM=D1/RE
51    CONTINUE
      DD=GG*EM*AMM-X*ANN*A
      K=K+1
      DLT=(D1-X*X)*A/DD
      IF (DABS(DLT).LT.ACC) GOTO 52
      IF (K.GT.50) GOTO 53
      X=X-DLT
      GOTO 54
53    CONTINUE
      WRITE (2,105)
105   FORMAT(15H NO CONVERGENCE)
52    CONTINUE
      WEIGHT=GG*(D1-X*X)/(DD*DD)
      SIT=X
      RETURN
      END
