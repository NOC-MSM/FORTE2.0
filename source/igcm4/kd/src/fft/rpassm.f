*DECK RPASSM
C     SUBROUTINE 'RPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART
C     OF MULTIPLE REAL FFT (FOURIER SYNTHESIS) ROUTINE
C
C     A IS FIRST REAL INPUT VECTOR
C         EQUIVALENCE B(1) WITH A (LA*INC1+1)
C     C IS FIRST REAL OUTPUT VECTOR
C         EQUIVALENCE D(1) WITH C(IFAC*LA*INC2+1)
C     TRIGS IS A PRECALCULATED LIST OF SINES & COSINES
C     INC1 IS THE ADDRESSING INCREMENT FOR A
C     INC2 IS THE ADDRESSING INCREMENT FOR C
C     INC3 IS THE INCREMENT BETWEEN INPUT VECTORS A
C     INC4 IS THE INCREMENT BETWEEN OUTPUT VECTORS C
C     LOT IS THE NUMBER OF VECTORS
C     N IS THE LENGTH OF THE VECTORS
C     IFAC IS THE CURRENT FACTOR OF N
C     LA IS THE PRODUCT OF PREVIOUS FACTORS
C     IERR IS AN ERROR INDICATOR:
C              0 - PASS COMPLETED WITHOUT ERROR
C              1 - LOT GREATER THAN 64
C              2 - IFAC NOT CATERED FOR
C              3 - IFAC ONLY CATERED FOR IF LA=N/IFAC
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,
     *    LA,IERR)
      DIMENSION A(N),B(N),C(N),D(N),TRIGS(N)
C
      DIMENSION A10(64),A11(64),A20(64),A21(64),
     *    B10(64),B11(64),B20(64),B21(64)
      DATA SIN36/0.587785252292473/,SIN72/0.951056516295154/,
     *    QRT5/0.559016994374947/,SIN60/0.866025403784437/
C
      M=N/IFAC
      IINK=LA*INC1
      JINK=LA*INC2
      JUMP=(IFAC-1)*JINK
      KSTOP=(N-IFAC)/(2*IFAC)
C
      IBAD=1
      IF (LOT.GT.64) GO TO 910
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.EQ.7) IGO=6
      IBAD=2
      IF (IGO.LT.1.OR.IGO.GT.6) GO TO 910
      GO TO (200,300,400,500,600,800),IGO
C
C     CODING FOR FACTOR 2
C     -------------------
  200 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      JA=1
      JB=JA+JINK
C
      IF (LA.EQ.M) GO TO 290
C
      DO 220 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 210 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  210 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  220 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB-IINK
      IBASE=0
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IA.EQ.IB) GO TO 260
      DO 250 K=LA,KSTOP,LA
      KB=K+K
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      IBASE=0
      DO 240 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 230 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)-B(IB+I)
      C(JB+J)=C1*(A(IA+I)-A(IB+I))-S1*(B(IA+I)+B(IB+I))
      D(JB+J)=S1*(A(IA+I)-A(IB+I))+C1*(B(IA+I)+B(IB+I))
      I=I+INC3
      J=J+INC4
  230 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  240 CONTINUE
      IA=IA+IINK
      IB=IB-IINK
      JBASE=JBASE+JUMP
  250 CONTINUE
      IF (IA.GT.IB) GO TO 900
  260 CONTINUE
      IBASE=0
      DO 280 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 270 IJK=1,LOT
      C(JA+J)=A(IA+I)
      C(JB+J)=-B(IA+I)
      I=I+INC3
      J=J+INC4
  270 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  280 CONTINUE
      GO TO 900
C
  290 CONTINUE
      DO 294 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 292 IJK=1,LOT
      C(JA+J)=2.0*(A(IA+I)+A(IB+I))
      C(JB+J)=2.0*(A(IA+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  292 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  294 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 3
C     -------------------
  300 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      IC=IB
      JA=1
      JB=JA+JINK
      JC=JB+JINK
C
      IF (LA.EQ.M) GO TO 390
C
      DO 320 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 310 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=(A(IA+I)-0.5*A(IB+I))-(SIN60*(B(IB+I)))
      C(JC+J)=(A(IA+I)-0.5*A(IB+I))+(SIN60*(B(IB+I)))
      I=I+INC3
      J=J+INC4
  310 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  320 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB+IINK
      IC=IC-IINK
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IA.EQ.IC) GO TO 360
      DO 350 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      IBASE=0
      DO 340 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 330 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)-B(IC+I))
      C(JB+J)=
     *    C1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)+B(IC+I))))
     *   -S1*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      D(JB+J)=
     *    S1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)+B(IC+I))))
     *   +C1*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      C(JC+J)=
     *    C2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)+B(IC+I))))
     *   -S2*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      D(JC+J)=
     *    S2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)+B(IC+I))))
     *   +C2*((B(IA+I)-0.5*(B(IB+I)-B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      I=I+INC3
      J=J+INC4
  330 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  340 CONTINUE
      IA=IA+IINK
      IB=IB+IINK
      IC=IC-IINK
      JBASE=JBASE+JUMP
  350 CONTINUE
      IF (IA.GT.IC) GO TO 900
  360 CONTINUE
      IBASE=0
      DO 380 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 370 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=(0.5*A(IA+I)-A(IB+I))-(SIN60*B(IA+I))
      C(JC+J)=-(0.5*A(IA+I)-A(IB+I))-(SIN60*B(IA+I))
      I=I+INC3
      J=J+INC4
  370 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  380 CONTINUE
      GO TO 900
C
  390 CONTINUE
      SSIN60=2.0*SIN60
      DO 394 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 392 IJK=1,LOT
      C(JA+J)=2.0*(A(IA+I)+A(IB+I))
      C(JB+J)=(2.0*A(IA+I)-A(IB+I))-(SSIN60*B(IB+I))
      C(JC+J)=(2.0*A(IA+I)-A(IB+I))+(SSIN60*B(IB+I))
      I=I+INC3
      J=J+INC4
  392 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  394 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 4
C     -------------------
  400 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      IC=IB+2*M*INC1
      ID=IB
      JA=1
      JB=JA+JINK
      JC=JB+JINK
      JD=JC+JINK
C
      IF (LA.EQ.M) GO TO 490
C
      DO 420 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 410 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+A(IB+I)
      C(JB+J)=(A(IA+I)-A(IC+I))-B(IB+I)
      C(JC+J)=(A(IA+I)+A(IC+I))-A(IB+I)
      C(JD+J)=(A(IA+I)-A(IC+I))+B(IB+I)
      I=I+INC3
      J=J+INC4
  410 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  420 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB+IINK
      IC=IC-IINK
      ID=ID-IINK
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IB.EQ.IC) GO TO 460
      DO 450 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      IBASE=0
      DO 440 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 430 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)-B(IC+I))+(B(IB+I)-B(ID+I))
      C(JC+J)=
     *    C2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
     *   -S2*((B(IA+I)-B(IC+I))-(B(IB+I)-B(ID+I)))
      D(JC+J)=
     *    S2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
     *   +C2*((B(IA+I)-B(IC+I))-(B(IB+I)-B(ID+I)))
      C(JB+J)=
     *    C1*((A(IA+I)-A(IC+I))-(B(IB+I)+B(ID+I)))
     *   -S1*((B(IA+I)+B(IC+I))+(A(IB+I)-A(ID+I)))
      D(JB+J)=
     *    S1*((A(IA+I)-A(IC+I))-(B(IB+I)+B(ID+I)))
     *   +C1*((B(IA+I)+B(IC+I))+(A(IB+I)-A(ID+I)))
      C(JD+J)=
     *    C3*((A(IA+I)-A(IC+I))+(B(IB+I)+B(ID+I)))
     *   -S3*((B(IA+I)+B(IC+I))-(A(IB+I)-A(ID+I)))
      D(JD+J)=
     *    S3*((A(IA+I)-A(IC+I))+(B(IB+I)+B(ID+I)))
     *   +C3*((B(IA+I)+B(IC+I))-(A(IB+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  430 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  440 CONTINUE
      IA=IA+IINK
      IB=IB+IINK
      IC=IC-IINK
      ID=ID-IINK
      JBASE=JBASE+JUMP
  450 CONTINUE
      IF (IB.GT.IC) GO TO 900
  460 CONTINUE
      IBASE=0
      SIN45=SQRT(0.5)
      DO 480 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 470 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=SIN45*((A(IA+I)-A(IB+I))-(B(IA+I)+B(IB+I)))
      C(JC+J)=B(IB+I)-B(IA+I)
      C(JD+J)=-SIN45*((A(IA+I)-A(IB+I))+(B(IA+I)+B(IB+I)))
      I=I+INC3
      J=J+INC4
  470 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  480 CONTINUE
      GO TO 900
C
  490 CONTINUE
      DO 494 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 492 IJK=1,LOT
      C(JA+J)=2.0*((A(IA+I)+A(IC+I))+A(IB+I))
      C(JB+J)=2.0*((A(IA+I)-A(IC+I))-B(IB+I))
      C(JC+J)=2.0*((A(IA+I)+A(IC+I))-A(IB+I))
      C(JD+J)=2.0*((A(IA+I)-A(IC+I))+B(IB+I))
      I=I+INC3
      J=J+INC4
  492 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  494 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 5
C     -------------------
  500 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      IC=IB+2*M*INC1
      ID=IC
      IE=IB
      JA=1
      JB=JA+JINK
      JC=JB+JINK
      JD=JC+JINK
      JE=JD+JINK
C
      IF (LA.EQ.M) GO TO 590
C
      DO 520 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 510 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      C(JB+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))+QRT5*(A(IB+I)-A(IC+I)))
     *    -(SIN72*B(IB+I)+SIN36*B(IC+I))
      C(JC+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))-QRT5*(A(IB+I)-A(IC+I)))
     *    -(SIN36*B(IB+I)-SIN72*B(IC+I))
      C(JD+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))-QRT5*(A(IB+I)-A(IC+I)))
     *    +(SIN36*B(IB+I)-SIN72*B(IC+I))
      C(JE+J)=((A(IA+I)-0.25*(A(IB+I)+A(IC+I)))+QRT5*(A(IB+I)-A(IC+I)))
     *    +(SIN72*B(IB+I)+SIN36*B(IC+I))
      I=I+INC3
      J=J+INC4
  510 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  520 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB+IINK
      IC=IC+IINK
      ID=ID-IINK
      IE=IE-IINK
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IB.EQ.ID) GO TO 560
      DO 550 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      IBASE=0
      DO 540 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 530 IJK=1,LOT
C
      A10(IJK)=(A(IA+I)-0.25*((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))))
     *    +QRT5*((A(IB+I)+A(IE+I))-(A(IC+I)+A(ID+I)))
      A20(IJK)=(A(IA+I)-0.25*((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))))
     *    -QRT5*((A(IB+I)+A(IE+I))-(A(IC+I)+A(ID+I)))
      B10(IJK)=(B(IA+I)-0.25*((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I))))
     *    +QRT5*((B(IB+I)-B(IE+I))-(B(IC+I)-B(ID+I)))
      B20(IJK)=(B(IA+I)-0.25*((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I))))
     *    -QRT5*((B(IB+I)-B(IE+I))-(B(IC+I)-B(ID+I)))
      A11(IJK)=SIN72*(B(IB+I)+B(IE+I))+SIN36*(B(IC+I)+B(ID+I))
      A21(IJK)=SIN36*(B(IB+I)+B(IE+I))-SIN72*(B(IC+I)+B(ID+I))
      B11(IJK)=SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))
      B21(IJK)=SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))
C
      C(JA+J)=A(IA+I)+((A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I)))
      D(JA+J)=B(IA+I)+((B(IB+I)-B(IE+I))+(B(IC+I)-B(ID+I)))
      C(JB+J)=C1*(A10(IJK)-A11(IJK))-S1*(B10(IJK)+B11(IJK))
      D(JB+J)=S1*(A10(IJK)-A11(IJK))+C1*(B10(IJK)+B11(IJK))
      C(JE+J)=C4*(A10(IJK)+A11(IJK))-S4*(B10(IJK)-B11(IJK))
      D(JE+J)=S4*(A10(IJK)+A11(IJK))+C4*(B10(IJK)-B11(IJK))
      C(JC+J)=C2*(A20(IJK)-A21(IJK))-S2*(B20(IJK)+B21(IJK))
      D(JC+J)=S2*(A20(IJK)-A21(IJK))+C2*(B20(IJK)+B21(IJK))
      C(JD+J)=C3*(A20(IJK)+A21(IJK))-S3*(B20(IJK)-B21(IJK))
      D(JD+J)=S3*(A20(IJK)+A21(IJK))+C3*(B20(IJK)-B21(IJK))
C
      I=I+INC3
      J=J+INC4
  530 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  540 CONTINUE
      IA=IA+IINK
      IB=IB+IINK
      IC=IC+IINK
      ID=ID-IINK
      IE=IE-IINK
      JBASE=JBASE+JUMP
  550 CONTINUE
      IF (IB.GT.ID) GO TO 900
  560 CONTINUE
      IBASE=0
      DO 580 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 570 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IB+I))+A(IC+I)
      C(JB+J)=(QRT5*(A(IA+I)-A(IB+I))+(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN36*B(IA+I)+SIN72*B(IB+I))
      C(JE+J)=-(QRT5*(A(IA+I)-A(IB+I))+(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN36*B(IA+I)+SIN72*B(IB+I))
      C(JC+J)=(QRT5*(A(IA+I)-A(IB+I))-(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN72*B(IA+I)-SIN36*B(IB+I))
      C(JD+J)=-(QRT5*(A(IA+I)-A(IB+I))-(0.25*(A(IA+I)+A(IB+I))-A(IC+I)))
     *    -(SIN72*B(IA+I)-SIN36*B(IB+I))
      I=I+INC3
      J=J+INC4
  570 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  580 CONTINUE
      GO TO 900
C
  590 CONTINUE
      QQRT5=2.0*QRT5
      SSIN36=2.0*SIN36
      SSIN72=2.0*SIN72
      DO 594 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 592 IJK=1,LOT
      C(JA+J)=2.0*(A(IA+I)+(A(IB+I)+A(IC+I)))
      C(JB+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))
     *    +QQRT5*(A(IB+I)-A(IC+I)))-(SSIN72*B(IB+I)+SSIN36*B(IC+I))
      C(JC+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))
     *    -QQRT5*(A(IB+I)-A(IC+I)))-(SSIN36*B(IB+I)-SSIN72*B(IC+I))
      C(JD+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))
     *    -QQRT5*(A(IB+I)-A(IC+I)))+(SSIN36*B(IB+I)-SSIN72*B(IC+I))
      C(JE+J)=(2.0*(A(IA+I)-0.25*(A(IB+I)+A(IC+I)))
     *    +QQRT5*(A(IB+I)-A(IC+I)))+(SSIN72*B(IB+I)+SSIN36*B(IC+I))
      I=I+INC3
      J=J+INC4
  592 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  594 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 6
C     -------------------
  600 CONTINUE
      IA=1
      IB=IA+(2*M-LA)*INC1
      IC=IB+2*M*INC1
      ID=IC+2*M*INC1
      IE=IC
      IF=IB
      JA=1
      JB=JA+JINK
      JC=JB+JINK
      JD=JC+JINK
      JE=JD+JINK
      JF=JE+JINK
C
      IF (LA.EQ.M) GO TO 690
C
      DO 620 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 610 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(ID+I))+(A(IB+I)+A(IC+I))
      C(JD+J)=(A(IA+I)-A(ID+I))-(A(IB+I)-A(IC+I))
      C(JB+J)=((A(IA+I)-A(ID+I))+0.5*(A(IB+I)-A(IC+I)))
     *    -(SIN60*(B(IB+I)+B(IC+I)))
      C(JF+J)=((A(IA+I)-A(ID+I))+0.5*(A(IB+I)-A(IC+I)))
     *    +(SIN60*(B(IB+I)+B(IC+I)))
      C(JC+J)=((A(IA+I)+A(ID+I))-0.5*(A(IB+I)+A(IC+I)))
     *    -(SIN60*(B(IB+I)-B(IC+I)))
      C(JE+J)=((A(IA+I)+A(ID+I))-0.5*(A(IB+I)+A(IC+I)))
     *    +(SIN60*(B(IB+I)-B(IC+I)))
      I=I+INC3
      J=J+INC4
  610 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  620 CONTINUE
      IA=IA+IINK
      IINK=2*IINK
      IB=IB+IINK
      IC=IC+IINK
      ID=ID-IINK
      IE=IE-IINK
      IF=IF-IINK
      JBASE=JBASE+JUMP
      JUMP=2*JUMP+JINK
      IF (IC.EQ.ID) GO TO 660
      DO 650 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      KF=KE+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      C5=TRIGS(KF+1)
      S5=TRIGS(KF+2)
      IBASE=0
      DO 640 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 630 IJK=1,LOT
C
      A11(IJK)= (A(IE+I)+A(IB+I))+(A(IC+I)+A(IF+I))
      A20(IJK)=(A(IA+I)+A(ID+I))-0.5*A11(IJK)
      A21(IJK)=SIN60*((A(IE+I)+A(IB+I))-(A(IC+I)+A(IF+I)))
      B11(IJK)= (B(IB+I)-B(IE+I))+(B(IC+I)-B(IF+I))
      B20(IJK)=(B(IA+I)-B(ID+I))-0.5*B11(IJK)
      B21(IJK)=SIN60*((B(IB+I)-B(IE+I))-(B(IC+I)-B(IF+I)))
C
      C(JA+J)=(A(IA+I)+A(ID+I))+A11(IJK)
      D(JA+J)=(B(IA+I)-B(ID+I))+B11(IJK)
      C(JC+J)=C2*(A20(IJK)-B21(IJK))-S2*(B20(IJK)+A21(IJK))
      D(JC+J)=S2*(A20(IJK)-B21(IJK))+C2*(B20(IJK)+A21(IJK))
      C(JE+J)=C4*(A20(IJK)+B21(IJK))-S4*(B20(IJK)-A21(IJK))
      D(JE+J)=S4*(A20(IJK)+B21(IJK))+C4*(B20(IJK)-A21(IJK))
C
      A11(IJK)=(A(IE+I)-A(IB+I))+(A(IC+I)-A(IF+I))
      B11(IJK)=(B(IE+I)+B(IB+I))-(B(IC+I)+B(IF+I))
      A20(IJK)=(A(IA+I)-A(ID+I))-0.5*A11(IJK)
      A21(IJK)=SIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      B20(IJK)=(B(IA+I)+B(ID+I))+0.5*B11(IJK)
      B21(IJK)=SIN60*((B(IE+I)+B(IB+I))+(B(IC+I)+B(IF+I)))
C
      C(JD+J)=
     *  C3*((A(IA+I)-A(ID+I))+A11(IJK))-S3*((B(IA+I)+B(ID+I))-B11(IJK))
      D(JD+J)=
     *  S3*((A(IA+I)-A(ID+I))+A11(IJK))+C3*((B(IA+I)+B(ID+I))-B11(IJK))
      C(JB+J)=C1*(A20(IJK)-B21(IJK))-S1*(B20(IJK)-A21(IJK))
      D(JB+J)=S1*(A20(IJK)-B21(IJK))+C1*(B20(IJK)-A21(IJK))
      C(JF+J)=C5*(A20(IJK)+B21(IJK))-S5*(B20(IJK)+A21(IJK))
      D(JF+J)=S5*(A20(IJK)+B21(IJK))+C5*(B20(IJK)+A21(IJK))
C
      I=I+INC3
      J=J+INC4
  630 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  640 CONTINUE
      IA=IA+IINK
      IB=IB+IINK
      IC=IC+IINK
      ID=ID-IINK
      IE=IE-IINK
      IF=IF-IINK
      JBASE=JBASE+JUMP
  650 CONTINUE
      IF (IC.GT.ID) GO TO 900
  660 CONTINUE
      IBASE=0
      DO 680 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 670 IJK=1,LOT
      C(JA+J)=A(IB+I)+(A(IA+I)+A(IC+I))
      C(JD+J)=B(IB+I)-(B(IA+I)+B(IC+I))
      C(JB+J)=(SIN60*(A(IA+I)-A(IC+I)))-(0.5*(B(IA+I)+B(IC+I))+B(IB+I))
      C(JF+J)=-(SIN60*(A(IA+I)-A(IC+I)))-(0.5*(B(IA+I)+B(IC+I))+B(IB+I))
      C(JC+J)=SIN60*(B(IC+I)-B(IA+I))+(0.5*(A(IA+I)+A(IC+I))-A(IB+I))
      C(JE+J)=SIN60*(B(IC+I)-B(IA+I))-(0.5*(A(IA+I)+A(IC+I))-A(IB+I))
      I=I+INC3
      J=J+INC4
  670 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  680 CONTINUE
      GO TO 900
C
  690 CONTINUE
      SSIN60=2.0*SIN60
      DO 694 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 692 IJK=1,LOT
      C(JA+J)=(2.0*(A(IA+I)+A(ID+I)))+(2.0*(A(IB+I)+A(IC+I)))
      C(JD+J)=(2.0*(A(IA+I)-A(ID+I)))-(2.0*(A(IB+I)-A(IC+I)))
      C(JB+J)=(2.0*(A(IA+I)-A(ID+I))+(A(IB+I)-A(IC+I)))
     *    -(SSIN60*(B(IB+I)+B(IC+I)))
      C(JF+J)=(2.0*(A(IA+I)-A(ID+I))+(A(IB+I)-A(IC+I)))
     *    +(SSIN60*(B(IB+I)+B(IC+I)))
      C(JC+J)=(2.0*(A(IA+I)+A(ID+I))-(A(IB+I)+A(IC+I)))
     *    -(SSIN60*(B(IB+I)-B(IC+I)))
      C(JE+J)=(2.0*(A(IA+I)+A(ID+I))-(A(IB+I)+A(IC+I)))
     *    +(SSIN60*(B(IB+I)-B(IC+I)))
      I=I+INC3
      J=J+INC4
  692 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  694 CONTINUE
      GO TO 900
C
C     CODING FOR FACTOR 8
C     -------------------
  800 CONTINUE
      IBAD=3
      IF (LA.NE.M) GO TO 910
      IA=1
      IB=IA+LA*INC1
      IC=IB+2*LA*INC1
      ID=IC+2*LA*INC1
      IE=ID+2*LA*INC1
      JA=1
      JB=JA+JINK
      JC=JB+JINK
      JD=JC+JINK
      JE=JD+JINK
      JF=JE+JINK
      JG=JF+JINK
      JH=JG+JINK
      SSIN45=SQRT(2.0)
C
      DO 820 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 810 IJK=1,LOT
      C(JA+J)=2.0*(((A(IA+I)+A(IE+I))+A(IC+I))+(A(IB+I)+A(ID+I)))
      C(JE+J)=2.0*(((A(IA+I)+A(IE+I))+A(IC+I))-(A(IB+I)+A(ID+I)))
      C(JC+J)=2.0*(((A(IA+I)+A(IE+I))-A(IC+I))-(B(IB+I)-B(ID+I)))
      C(JG+J)=2.0*(((A(IA+I)+A(IE+I))-A(IC+I))+(B(IB+I)-B(ID+I)))
      C(JB+J)=2.0*((A(IA+I)-A(IE+I))-B(IC+I))
     *    +SSIN45*((A(IB+I)-A(ID+I))-(B(IB+I)+B(ID+I)))
      C(JF+J)=2.0*((A(IA+I)-A(IE+I))-B(IC+I))
     *    -SSIN45*((A(IB+I)-A(ID+I))-(B(IB+I)+B(ID+I)))
      C(JD+J)=2.0*((A(IA+I)-A(IE+I))+B(IC+I))
     *    -SSIN45*((A(IB+I)-A(ID+I))+(B(IB+I)+B(ID+I)))
      C(JH+J)=2.0*((A(IA+I)-A(IE+I))+B(IC+I))
     *    +SSIN45*((A(IB+I)-A(ID+I))+(B(IB+I)+B(ID+I)))
      I=I+INC3
      J=J+INC4
  810 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  820 CONTINUE
C
C     RETURN
C     ------
  900 CONTINUE
      IBAD=0
  910 CONTINUE
      IERR=IBAD
      RETURN
      END
