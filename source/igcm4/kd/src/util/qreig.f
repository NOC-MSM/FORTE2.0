CERN      F200      VERSION    05/03/68 QREIG       217                F
C
      SUBROUTINE QREIG(A,IDIM1,M,IDIM2,ROOTR,ROOTI)
      DIMENSION A(IDIM1,IDIM2),ROOTR(M),ROOTI(M)
      CALL HESSEN(A,IDIM1,M,IDIM2,ROOTR)
      N=M
   81 ZERO=0.0
      JJ=1
  177 XNN=0.0
      XN2=0.0
      AA=0.0
      B=0.0
      C=0.0
      DD=0.0
      R=0.0
      SIG=0.0
      ITER=0
   17 IF(N-2)83,14,12
   83 ROOTR(1)=A(1,1)
      ROOTI(1)=0.0
    1 RETURN
   14 JJ=-1
   12 X=(A(N-1,N-1)-A(N,N))**2
      S=4.0*A(N,N-1)*A(N-1,N)
      ITER=ITER+1
      IF(X)124,15,124
  124 IF(ABS(S/X).GT.1.0E-8)GO TO 15
   16 IF(ABS(A(N-1,N-1))-ABS(A(N,N)))32,32,31
   31 E=A(N-1,N-1)
      G=A(N,N)
      GO TO 33
   32 G=A(N-1,N-1)
      E=A(N,N)
   33 F=0.0
      H=0.0
      GO TO 24
   15 S=X+S
      X=A(N-1,N-1)+A(N,N)
      IF(S)18,19,19
   19 SQ=SQRT(S)
      F=0.0
      H=0.0
      IF(X)21,21,22
   21 E=(X-SQ)/2.0
      G=(X+SQ)/2.0
      GO TO 24
   22 G=(X-SQ)/2.0
      E=(X+SQ)/2.0
      GO TO 24
   18 F=SQRT(-S)/2.0
      E=X/2.0
      G=E
      H=-F
   24 IF(JJ)85,70,70
   70 D=1.0E-10*(ABS(G)+F)
      IF(ABS(A(N-1,N-2)).GT.D)GO TO 26
   85 ROOTR(N)=E
      ROOTI(N)=F
      ROOTR(N-1)=G
      ROOTI(N-1)=H
      N=N-2
      IF(JJ)1,177,177
   26 IF(ABS(A(N,N-1)).GT.1.0E-10*ABS(A(N,N)))GO TO 50
   87 ROOTR(N)=A(N,N)
      ROOTI(N)=0.0
      N=N-1
      GO TO 177
   50 IF(ABS(ABS(XNN/A(N,N-1))-1.0)-1.0E-6)63,63,62
   62 IF(ABS(ABS(XN2/A(N-1,N-2))-1.0)-1.0E-6)63,63,700
   63 VQ=ABS(A(N,N-1))-ABS(A(N-1,N-2))
   64 IF(VQ)87,87,85
  700 IF(ITER.GT.50)GO TO 63
  701 Z1=(E-AA)**2+(F-B)**2-0.25*(E*E+F*F)
      Z2=(G-C)**2+(H-DD)**2-0.25*(G*G+H*H)
      IF(Z1)51,51,52
   51 IF(Z2)53,53,54
   53 R=E*G-F*H
      SIG=E+G
      GO TO 60
   54 R=E*E
      SIG=E+E
      GO TO 60
   52 IF(Z2)55,55,601
   55 R=G*G
      SIG=G+G
      GO TO 60
  601 R=0.0
      SIG=0.0
   60 XNN=A(N,N-1)
      XN2=A(N-1,N-2)
      CALL QRT(A,N,R,SIG,D,IDIM1,IDIM2)
      AA=E
      B=F
      C=G
      DD=H
      GO TO 12
      END
