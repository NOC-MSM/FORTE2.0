C* ----------------------------------------------------------------------
C 
C                            FSCINT
C                            ------
C               *******************************
C               * OASIS INTERPOLATION PACKAGE *
C               * ----- ------------- ------- *
C               *******************************
C 
C* This is the Fast Scalar INTerpolator package initially written by Yves
C* Chartier and colleagues at RPN Canada. This software has been adapted
C* to the OASIS structure and provides several interpolation techniques
C* (see oasis documentation for details). This package has been split in
C* 2 files: discendo.f and semper.f (for people not fluent in latin,
C* "semper discendo" means "always learning" ... ;-)...). discendo.f
C* contains all the interpolation related routines while semper.f has
C* the memory allocation using fortran 90 features.
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------
C       1.1       L. Terray      94/06/01  Introduced
C       1.1       P. Braconnot   94/06/15  Extend longitude range for Z grids
C       2.0       L. Terray      96/09/25 
C       2.2       G. Risari      97/12/01  new routine names 
C       2.2       A.P, S.V, L.T  97/12/14  Change memory allocation
C       2.3       L. Terray      99/09/15  Introduction of Y grid and cleaning
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      INTEGER FUNCTION FINDLON(VAL, TABLEAU, NBELEM, TMP)
C* ----------------------------------------------------------------------
C
C* New function to account for strange AGCM Z-grids (longitude<0 or >360)
C
C* ----------------------------------------------------------------------
      INTEGER NBELEM
      REAL VAL
      REAL TABLEAU(NBELEM)
      REAL TMP
      INTEGER DEBUT, MILIEU, FIN
C
C* Get longitudes in the right interval
C
      IF( VAL .GT. TABLEAU(NBELEM)) THEN
          TMP = VAL - 360.
      ELSEIF( VAL .LT. TABLEAU(1)) THEN
          TMP = VAL + 360.
      ELSE
          TMP = VAL
      ENDIF
C
C* Find grid point coordinates
C
      DEBUT = 1
      FIN = NBELEM
      MILIEU = (DEBUT+FIN)*0.5
23000 IF( (MILIEU.NE. DEBUT))THEN
         IF( TMP .LE. TABLEAU(MILIEU) ) THEN
            FIN   = MILIEU
         ELSE 
            DEBUT = MILIEU
         ENDIF 
         MILIEU = (DEBUT+FIN)*0.5
         GOTO 23000
      ENDIF 
      FINDLON = MILIEU
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      INTEGER FUNCTION CHERCHE(VAL, TABLEAU, NBELEM)
      INTEGER NBELEM
      REAL VAL
      REAL TABLEAU(NBELEM)
      INTEGER DEBUT, MILIEU, FIN
      DEBUT = 1
      FIN = NBELEM
      MILIEU = (DEBUT+FIN)*0.5
23000 IF( (MILIEU.NE. DEBUT))THEN
         IF( (VAL.LE. TABLEAU(MILIEU)))THEN
            FIN   = MILIEU
         ELSE 
            DEBUT = MILIEU
         ENDIF 
         MILIEU = (DEBUT+FIN)*0.5
         GOTO 23000
      ENDIF 
      CHERCHE = MILIEU
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE CHKXTRAP(PX, PY, NPTS, NI, NJ)
      IMPLICIT NONE
      INTEGER NPTS, NI, NJ, OFFL, OFFR
      REAL PX(NPTS), PY(NPTS)
      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      INTEGER N, I, J
      FLGXTRAP = .FALSE.
      IF( (ORDINT.EQ. 3))THEN
         OFFR = 3
         OFFL = 2
      ELSE 
         OFFR = 1
         OFFL = 0
      ENDIF 
      DO 23002 N=1, NPTS
         I = IFIX(PX(N))
         J = IFIX(PY(N))
         IF( (I.LT. OFFL.OR. J.LT. OFFL.OR. I.GT. (NI-OFFR).OR. J
     $   .GT. (NJ-OFFR)))THEN
            FLGXTRAP = .TRUE.
         ENDIF 
23002 CONTINUE 
      IF( (FLGXTRAP.AND. CODXTRAP.EQ. ABORT))THEN
         WRITE(6, *)
     $'*****************************************************************
     $****'
         WRITE(6, *)
     $       'target grid contains points outside of source grid'
         WRITE(6, *)  'We stop the program'
         WRITE(6, *)
     $'*****************************************************************
     $****'
         STOP
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE DGAUSS (N,ROOTS,KASE)
      REAL ROOTS(*)
C
C AUTEUR- D. ROBERTSON
C
C OBJET(DGAUSS)
C     - CALCULATES THE ZEROES OF THE ORDINARY LEGENDRE
C     POLYNOMIAL OF ORDER N,  I.E. DEFINE GAUSSIAN GRID
C
C ALGORITHME
C     - THE POSITIVE ROOTS ARE APPROXIMATED BY THE BEST
C     ASYMPTOTIC FORMULA AVAILABLE TO THE AUTHOR, FOUND IN
C     ABRAMOWITZ AND STEGUN "HANDBOOK OF MATHEMATICAL FUNCTIONS".
C     CHAPTER 22 FORMULA 22.16.6.
C     NEWTON'S METHOD IS USED TO REFINE THE GUESS TO PRECISION
C     DEFINED BY THE CONSTANT TOL.  SINCE THE ROOTS ARE OF ORDER
C     OF MAGNITUDE UNITY, ABSOLUTE PRECISION IS ADEQUATE, RATHER
C     THAN A RELATIVE TEST.
C     A STANDARD IDENTITY IS USED TO DETERMINE THE DERIVATIVE OF
C     THE POLYNOMIAL IN TERMS OF THE VALUES OF P(N;X),P(N-1;X).
C     (X**2-1.0)*(DP/DX)=N*(X*P(N;,X)-P(N-1;X)).
C     SEE ABRAMOWITZ AND STEGUN FORMULA 22.8.5
C     NOTE THAT IN CONTRAST TO OTHER FORMULAS THIS REQUIRES ONLY
C     2 EVALUATIONS OF A LEGENDRE POLYNOMIAL PER ITERATION.
C     NOTE THAT THE COORDINATE USED IS CONVENTIONALLY REFERRED TO
C     AS MU=COS(THETA), RUNNING FROM +1 TO -1, FOR THETA FROM 0 TO
C     PI. THE NEGATIVE ROOTS ARE  FILLED BY SYMMETRY.
C     FOR KASE=GLOBAL, ALL N ROOTS ARE FOUND, WHILE FOR
C     DASE=NORTH/SOUTH ONLY THE +VE/-VE ROOTS ARE FOUND,
C     (INCLUDING 0 IF N IS ODD)  I.E. N/2+MOD(N,2) ROOTS.
C
C
C APPEL - CALL DGAUSS(N,ROOTS,KASE)
C
C ARGUMENTS
C     IN    - N     - ORDER OF THE POLYNOMIALS
C     OUT   - ROOTS - ARRAY CONTAINING THE ZEROES OF THE
C     ORDINARY LEGENDRE POLYNOMIALS
C     IN    - KASE  - =0, GLOBAL
C     =1, NORTH
C     =2, SOUTH
C 
C MODULES APPELES
C     - ORDLEG
C
C ----------------------------------------------------------------------
C
C     THE ANSWERS ARE RETURNED IN ROOTS.

      REAL NORMN,NORMNM
      INTEGER GLOBAL,NORTH,SOUTH,NORD,SUD
      PARAMETER(GLOBAL=0,NORTH=1,NORD=1,SOUTH=2,SUD=2)
      DATA TOL /1.0E-06/
C
C     RDTODG = 180/PI, DGTORD = PI/180
C
      DATA PI   /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C
C     ORDLEG RETURNS POLYNOMIALS NORMALIZED TO UNIT INTEGRAL.
C     NORMN,NORMNMN RESTORE THE CONVENTION NORMALIZATION, P(N;1.0)=1.0.
C* ----------------------------------------------------------------------

      NORMN =SQRT(2.0/(2.0*N+1.0))
      NORMNM=SQRT(2.0/(2.0*N-1.0))
      L = N/2
C
C     CALCULATE ASYMPTOTIC APPROXIMATION
C
      DO 23000 I=1,L
         IF((KASE.NE.SOUTH))THEN
            J = I
         ENDIF 
         IF((KASE.EQ.SOUTH))THEN
            J=I+L+MOD(N,2)
         ENDIF 
         T = (4*J-1)*PI/FLOAT(4*N+2)
         IF((KASE.NE.SOUTH))THEN
            IRT = I
         ENDIF 
         IF((KASE.EQ.SOUTH))THEN
            IRT = I + MOD(N,2)
         ENDIF 
         ROOTS(IRT)=COS(T+1.0/(8.0*FLOAT(N**2)*TAN(T)))

C
23000 CONTINUE 
      DO 23010 I=1,L

C
C     REPEAT 1 NEWTON ITERATION
C     **  BEGIN
C
6        CALL ORDLEG(G,ROOTS(I),N)
         CALL ORDLEG(GM,ROOTS(I),N-1)
         PN = NORMN*G
         PNM= NORMNM*GM
C
C     **         GUESS(K+1)=GUESS(K)-P/(DP/DX)
C

         RDPDX = (ROOTS(I)**2-1.0)/(N*(ROOTS(I)*PN-PNM))
         DELTA = -PN*RDPDX
         ROOTS(I) = ROOTS(I)+DELTA
         IF((ABS(DELTA).GT.TOL))THEN
            GO TO 6
C
C     **  END UNTIL ABS(DELTA).LE.TOL
C

         ENDIF 
         ROOTS(N+1-I) = -ROOTS(I)

C
23010 CONTINUE 
      IF((MOD(N,2).EQ.0))THEN
         RETURN
      ENDIF 
      IF((KASE.NE.SOUTH))THEN
         IRT = L+1
      ENDIF 
      IF((KASE.EQ.SOUTH))THEN
         IRT = 1
      ENDIF 
      ROOTS(IRT) = 0.0
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE GAUSS(NRACP,RACP,PG,SIA,RAD,PGSSIN2,SINM1,SINM2,
     $SIN2)
C* -----------------------------------------------------------------------
C     CALCULE LES NRACP RACINES POSITIVES DU POLYNOME DE LEGENDRE DE
C     DEGRE 2*NRACP (ICI-APRES NOTE PN) DEFINI SUR L INTERVALLE DES
C     COLATITUDES ALLANT DE 0 (POLE NORD) A PI (POLE SUD). ON SAIT QUE
C     LES 2*NRACP RACINES SONT ANTI-SYMETRIQUES P/R A L EQUATEUR PI/2,
C     ETANT POSITIVES ENTRE COLAT=0 ET COLAT =PI/2.
C     ON CALCULE ENSUITE LES POIDS DE GAUSS ASSOCIES AUX COLATITUDES
C     GAUSSIENNES (ICI APRES NOTEES CG), AINSI QU UN CERTAIN NOMBRE DE
C     FONCTIONS DE CG DEFINIES PLUS LOIN. ON RAPPELLE ENFIN QUE LA LATI-
C     TUDE LAT=COLAT-PI/2, ET DONC QUE SIN(LAT)=COS(COLAT).
C     NRACP        : NOMBRE DE RACINES POSITIVES DU POLYNOME DE LEGENDRE
C                  : DE DEGRE 2*NRACP.
C     RACP(I)      : RACINES DE PN, =SIN(LG)=COS(CG).
C     PG(I)        : POIDS DE GAUSS CORRESPONDANTS.
C     SIA(I)       : SIN(CG)=COS(LG).
C     RAD(I)       : COLATITUDE CG EN RADIANS.
C     PGSSIN2(I)   : POIDS DE GAUSS / (SIN(CG))**2.
C     SINM1(I)     : (SIN(CG))**-1.
C     SINM2(I)     : (SIN(CG))**-2.
C     VOIR NST 8, CHAP. A, PP.1-7, ET APPENDICE D12, PP. 26-27.
C     VERSION REVISEE PAR MICHEL BELAND, 9 DECEMBRE 1980.
C     *****************************************************************
C
C
C     -----------------------------------------------------------------

      DIMENSION RACP(1),PG(1),SIA(1),RAD(1),PGSSIN2(1),SINM1(1),
     $SINM2(1),SIN2(1)
C     --------------------------------------------------------------
C
C     ON DEMANDE UNE PRECISION DE 1.E-13 POUR LES RACINES DE PN.
C

      XLIM=1.E-6
      PI = 3.1415926535898
      IR = 2*NRACP
      FI=FLOAT(IR)
      FI1=FI+1.
      FN=FLOAT(NRACP)
C
C     ON UTILISE UNE FORMULE ASYMPTOTIQUE POUR OBTENIR LES VALEURS
C     APPROXIMATIVES DES COLATITUDES GAUSSIENNES
C     CG(I) = (PI/2) * (2*I-1)/(2*NRACP).
C     VOIR ABRAMOWITZ AND STEGUN, P. 787, EQU. 22.16.6 .
C

      DO 23000 I=1,NRACP
         DOT=FLOAT(I-1)
         RACP(I)=-PI*.5*(DOT+.5)/FN + PI*.5
         RACP(I) =  SIN(RACP(I))

C
C     ON CALCULE ENSUITE LES CONSTANTES FACTEURS DE P(N+1) ET P(N-1)
C     DANS L EXPRESSION DE LA PSEUDO-DERIVEE DE PN.
C
23000 CONTINUE 
      DN = FI/SQRT(4.*FI*FI-1.)
      DN1=FI1/SQRT(4.*FI1*FI1-1.)
      A = DN1*FI
      B = DN*FI1
      IRP = IR + 1
      IRM = IR -1
C
C     ON EMPLOIE ENSUITE UNE METHODE DE NEWTON POUR AUGMENTER LA PREC.
C     SI RACTEMP EST UNE SOL. APPROXIMATIVE  DE PN(RACP)=0., ALORS LA
C     SEQUENCE RACTEMP(I+1)=RACTEMP(I)-PN(RACTEMP(I))/DER.PN(RACTEMP(I))
C     CONVERGE VERS RACP DE FACON QUADRATIQUE.
C     VOIR ABRAMOWITZ AND STEGUN, P.18, EQU. 3.9.5.
C     ORDLEG CALCULE LA VALEUR DE PN (RACP) , NORMALISE.
C

      DO 23002 I=1,NRACP
42       CALL ORDLEG(G,RACP(I),IR)
         CALL ORDLEG(GM,RACP(I),IRM)
         CALL ORDLEG(GP,RACP(I),IRP)
         GT = (A*GP-B*GM)/(RACP(I)*RACP(I)-1.)
         RACTEMP = RACP(I) - G/GT
         GTEMP = RACP(I) - RACTEMP
         RACP(I) = RACTEMP
         IF(( ABS(GTEMP).GT.XLIM))THEN
            GO TO 42
         ENDIF 

C
C     ON CALCULE ENSUITE LES POIDS DE GAUSS SELON L ALGORITHME
C     PG(I) = 2./[(1.-RACP(I)**2)*(DER.PN(RACP(I)))**2].
C     VOIR ABRAMOWITZ AND STEGUN, P.887, EQU. 25.4.29.
C     NOTE: ON DOIT MULTIPLIER LA PRECEDENTE FORMULE PAR UN FACTEUR
C     DE DENORMALISATION, LES PN DONNES PAR ORDLEG ETANT NORMALISES.
C     ON SE SERT D UNE FORMULE DE RECURRENCE POUR LA DERIVEE DE PN.
C
23002 CONTINUE 
      DO 23006 I=1,NRACP
         A=2.*(1.-RACP(I)**2)
         CALL ORDLEG(B,RACP(I),IRM)
         B = B*B*FI*FI
         PG(I)=A*(FI-.5)/B
         RAD(I) =   ACOS(RACP(I))
         SIA(I) =  SIN(RAD(I))
         C=(SIA(I))**2
         SINM1(I) = 1./SIA(I)
         SINM2(I) = 1./C
         PGSSIN2(I) =PG(I)/C
         SIN2(I)=C
23006 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE GDW2LLW(SPDO,PSIO,XLON,LI,LJ,GRTYP,IG1,IG2,IG3,IG4)
      IMPLICIT NONE
      INTEGER LI,LJ
      REAL SPDO(LI,LJ), PSIO(LI,LJ), XLON(LI,LJ)
      CHARACTER*1 GRTYP
      INTEGER IG1,IG2,IG3,IG4
      EXTERNAL CIGAXG
C
C AUTEUR   - Y. CHARTIER - AVRIL 91
C
C OBJET(GDW2LLW)
C         - PASSE DE VENT DE GRILLE (COMPOSANTES U ET V)
C         - A VITESSE ET DIRECTION (SPEED, PSI)
C APPEL    - CALL GDW2LLW(SPD,PSI,LI,LJ,IYP,XG1,XG2,XG3,XG4)
C
C MODULES  - XGAIG
C
C ARGUMENTS
C  IN/OUT - SPD   - A L'ENTREE CONTIENT LA PSIOTESSE DU VENT ET
C                   A LA SORTIE LA COMPOSANTE U.
C  IN/OUT - PSI   - A L'ENTREE CONTIENT LA DIRECTION DU VENT ET
C                   A LA SORTIE LA COMPOSANTE V.
C   IN    - LI    - PREMIERE DIMENSION DES CHAMPS SPD ET PSI
C   IN    - LJ    - DEUXIEME DIMENSION DES CHAMPS SPD ET PSI
C   IN    - IGTYP  - TYPE DE GRILLE (VOIR OUVRIR)
C   IN    - XG1   - ** DESCRIPTEUR DE GRILLE (REEL),
C   IN    - XG2   -    IGTYP = 'N', PI, PJ, D60, DGRW
C   IN    - XG3   -    IGTYP = 'L', LAT0, LON0, DLAT, DLON,
C   IN    - XG4   -    IGTYP = 'A', 'B', 'G', XG1 = 0. GLOBAL,
C                                                 = 1. NORD
C                                                 = 2. SUD **
C
C MESSAGES - "ERREUR MAUVAISE GRILLE (GDW2LLW)"
C
C-------------------------------------------------------------
C
C
C
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
C
C RDTODG = 180/PIE, DGTORD = PIE/180

      REAL PIE,RDTODG,DGTORD
      DATA PIE   /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C

      INTEGER I,J
      REAL SPD, PSI
      REAL XG1,XG2,XG3,XG4
      IF( (GRTYP.EQ. 'N'))THEN
         CALL CIGAXG(GRTYP,XG1,XG2,XG3,XG4,IG1,IG2,IG3,IG4)
         DO 23002 I=1,LI
            DO 23004 J=1,LJ
               SPD=SQRT(SPDO(I,J)*SPDO(I,J)+PSIO(I,J)*PSIO(I,J))
               IF( (SPD.EQ. 0.0))THEN
                  PSI= 0.0
               ELSE 
                  IF( (SPDO(I,J).EQ. 0.0))THEN
                     IF( (PSIO(I,J).GE. 0.0))THEN
                        PSI= XLON(I,J)+XG4-90.0
                     ELSE 
                        PSI= XLON(I,J)+XG4+90.0
                     ENDIF 
                  ELSE 
                     PSI=XLON(I,J)+XG4-RDTODG*ATAN2(PSIO(I,J),SPDO(I
     $               ,J))
                  ENDIF 
               ENDIF 
               PSI=AMOD(AMOD(PSI,360.0)+360.0,360.0)
               SPDO(I,J)=SPD
               PSIO(I,J)=PSI
23004       CONTINUE 
23002    CONTINUE 
         RETURN
      ENDIF 
      IF( (GRTYP.EQ. 'S'))THEN
         CALL CIGAXG(GRTYP,XG1,XG2,XG3,XG4,IG1,IG2,IG3,IG4)
         DO 23014 I=1,LI
            DO 23016 J=1,LJ
               SPD=SQRT(SPDO(I,J)*SPDO(I,J)+PSIO(I,J)*PSIO(I,J))
               IF( (SPD.EQ. 0.0))THEN
                  PSI= 0.0
               ELSE 
                  IF( (SPDO(I,J).EQ. 0.0))THEN
                     IF( (PSIO(I,J).GE. 0.0))THEN
                        PSI= 90.0 - XLON(I,J)+XG4
                     ELSE 
                        PSI= 270.0 - XLON(I,J)+XG4
                     ENDIF 
                  ELSE 
                     PSI= 180.0 - XLON(I,J)+XG4-RDTODG*ATAN2(PSIO(I,
     $               J),SPDO(I,J))
                  ENDIF 
               ENDIF 
               PSI=AMOD(AMOD(PSI,360.0)+360.0,360.0)
               SPDO(I,J)=SPD
               PSIO(I,J)=PSI
23016       CONTINUE 
23014    CONTINUE 
         RETURN
      ENDIF 
      IF( (GRTYP.EQ.'A'.OR.GRTYP.EQ.'B'.OR.GRTYP.EQ.'G'.OR.GRTYP.EQ.
     $'L'))THEN
         DO 23026 I=1,LI
            DO 23028 J=1,LJ
               SPD=SQRT(SPDO(I,J)*SPDO(I,J)+PSIO(I,J)*PSIO(I,J))
               IF( (SPD.EQ. 0.0))THEN
                  PSI= 0.0
               ELSE 
                  IF( (SPDO(I,J).EQ. 0.0))THEN
                     IF( (PSIO(I,J).GE. 0.0))THEN
                        PSI= 180.0
                     ELSE 
                        PSI= 0.0
                     ENDIF 
                  ELSE 
                     PSI=270.0 - RDTODG*ATAN2(PSIO(I,J),SPDO(I,J))
                  ENDIF 
               ENDIF 
               PSI=AMOD(AMOD(PSI,360.0)+360.0,360.0)
               SPDO(I,J)=SPD
               PSIO(I,J)=PSI
23028       CONTINUE 
23026    CONTINUE 
         RETURN
      ENDIF 
      WRITE(6, 600) GRTYP
600   FORMAT('0',' Error bad grid (GDW2LLW) - GRTYP = ', A1)
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE GGGDINT(ZO,PX,PY,NPTS,AY,CY,Z,I1,I2,J1,J2)
      IMPLICIT NONE
C*******
C AUTEUR: Y.CHARTIER, DRPN
C        FEVRIER 1991
C
C OBJET:  INTERPOLATION BI-CUBIQUE DE POINTS A PARTIR
C        D'UNE GRILLE GAUSSIENNE.
C*******
      INTEGER NPTS,I1,I2,J1,J2
      REAL ZO(NPTS),PX(NPTS),PY(NPTS)
      REAL AY(J1:J2),CY(J1:J2,6)
      REAL Z(I1:I2,J1:J2)
C
C  NPTS   : NOMBRE DE POINTS A INTERPOLER
C  I1:I2  : DIMENSION DE LA GRILLE SOURCE SELON X
C  J1:J2  : DIMENSION DE LA GRILLE SOURCE SELON Y
C  ZO     : VECTEUR DE SORTIE CONTENANT LES VALEURS INTERPOLEES
C  PX     : VECTEUR CONTENANT LA POSITION X DES POINTS QUE L'ON
C           VEUT INTERPOLER
C  PY     : VECTEUR CONTENANT LA POSITION Y DES POINTS QUE L'ON
C           VEUT INTERPOLER
C  AY     : VECTEUR CONTENANT LA POS. DES POINTS SUR L'AXE DES Y.
C  CY     : VECTEUR CONTENANT UNE TABLE DE DIFFERENCES SELON Y.
C  Z      : VALEURS DE LA GRILLE SOURCE.
C
C***************************************************************************
C
C  *   *   *   *
C
C  *   *   *   *
C        #        ==>   PT (X,Y)
C  *  (=)  *   *  ==> = PT (I, J)
C
C  *   *   *   *
C
C
C
C  CY(I,1) = 1.0 / (X2-X1)
C  CY(I,2) = 1.0 / (X3-X1)
C  CY(I,3) = 1.0 / (X3-X2)
C  CY(I,4) = 1.0 / (X4-X1)
C  CY(I,5) = 1.0 / (X4-X2)
C  CY(I,6) = 1.0 / (X4-X3)
C
C  STRUCTURE IDENTIQUE POUR CY(J,1..6)

      INTEGER I, J, M, N,STRIDE
      REAL X, X1, X2, X3, X4
      REAL B1, B2,  B3,  B4
      REAL B11, B12, B13, B14
      REAL Y,Y1,Y2,Y3,Y4
      REAL Y11, Y12, Y13, Y14
      REAL AY1, AY2, AY3, AY4
      REAL FA, FA2, FA3, FA4
      REAL A1,A2,A3,A4,C1,C2,C3,C4,C5,C6
C  DEFINITION DES FONCTIONS IN-LINE

      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      REAL CUBIC, DX,DY,Z1,Z2,Z3,Z4
      REAL ZLIN, ZZ1, ZZ2, ZDX
      CUBIC(Z1,Z2,Z3,Z4,DX)=((((Z4-Z1)*0.1666666666666 + 0.5*(Z2-Z3)
     $)*DX + 0.5*(Z1+Z3)-Z2)*DX + Z3-0.1666666666666*Z4-0.5*Z2-0.
     $3333333333333*Z1)*DX+Z2
      ZLIN(ZZ1,ZZ2,ZDX) = ZZ1 + (ZZ2 - ZZ1) * ZDX
      FA(A1,A2,A3,A4,X,X1,X2,X3)=A1+(X-X1)*(A2+(X-X2)*(A3+A4*(X-X3))
     $)
      FA2(C1,A1,A2)=C1*(A2-A1)
      FA3(C1,C2,C3,A1,A2,A3)=C2*(C3*(A3-A2)-C1*(A2-A1))
      FA4(C1,C2,C3,C4,C5,C6,A1,A2,A3,A4)=C4*(C5*(C6*(A4-A3)-C3*(A3-
     $A2))   - C2*(C3*(A3-A2)-C1*(A2-A1)))
      STRIDE = 1
      IF( (ORDINT.EQ. CUBIQUE))THEN
         DO 23002 N=1,NPTS
            I = MIN(I2-2,MAX(I1+1,IFIX(PX(N))))
            J = MIN(J2-2,MAX(J1+1,IFIX(PY(N))))
            DX = PX(N) - I
            Y1=CUBIC(Z(I-1,J-1),Z(I  ,J-1),Z(I+1,J-1),Z(I+2,J-1),DX)
            Y2=CUBIC(Z(I-1,J  ),Z(I  ,J  ),Z(I+1,J  ),Z(I+2,J  ),DX)
            Y3=CUBIC(Z(I-1,J+1),Z(I  ,J+1),Z(I+1,J+1),Z(I+2,J+1),DX)
            Y4=CUBIC(Z(I-1,J+2),Z(I  ,J+2),Z(I+1,J+2),Z(I+2,J+2),DX)
            Y = AY(J) + (AY(J+1)-AY(J))*(PY(N)-J)
C           INTERPOLATION FINALE SELON Y

            AY1=AY(J-1)
            AY2=AY(J)
            AY3=AY(J+1)
            AY4=AY(J+2)
            Y11 = Y1
            Y12 = FA2(CY(J,1),Y1,Y2)
            Y13 = FA3(CY(J,1),CY(J,2),CY(J,3),Y1,Y2,Y3)
            Y14 = FA4(CY(J,1),CY(J,2),CY(J,3),CY(J,4),CY(J,5),CY(J,6
     $      ),Y1,Y2,Y3,Y4)
            ZO(N) = FA(Y11,Y12,Y13,Y14,Y,AY1,AY2,AY3)
23002    CONTINUE 
      ENDIF 
      IF( (ORDINT.EQ. LINEAIR))THEN
         DO 23006 N=1,NPTS
            I = MIN(I2-2,MAX(I1+1,IFIX(PX(N))))
            J = MIN(J2-2,MAX(J1+1,IFIX(PY(N))))
            DX = PX(N) - I
            Y = AY(J) + (AY(J+1)-AY(J))*(PY(N)-J)
            DY = (Y -AY(J))/(AY(J+1)-AY(J))
            Y2=ZLIN(Z(I,J  ),Z(I+1,J  ),DX)
            Y3=ZLIN(Z(I,J+1),Z(I+1,J+1),DX)
            ZO(N)=ZLIN(Y2,Y3,DY)
23006    CONTINUE 
      ENDIF 
      IF( (ORDINT.EQ. VOISIN))THEN
         DO 23010 N=1,NPTS
            I = MIN(I2,MAX(I1,NINT(PX(N))))
            J = MIN(J2,MAX(J1,NINT(PY(N))))
            ZO(N) = Z(I,J)
23010    CONTINUE 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE GGLL2GD(X,Y,XLAT,XLON,NPTS,NI,NJ,HEM,LROOTS)
C* -------------------------------------------------------------------
C* S/R GGLL2GD - COMPUTES THE GRID CO-ORDINATES OF A POINT ON
C                A GAUSSIAN GRID
C* -------------------------------------------------------------------
      IMPLICIT NONE
C* --------------------------------------------------------------------
      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
      INTEGER NPTS, NI, NJ
      REAL X(NPTS), Y(NPTS), XLAT(NPTS), XLON(NPTS)
      REAL LROOTS(NJ)
      REAL ABS,DEL,EPSPHI
      INTEGER HEM
      INTEGER I,J
      INTEGER GUESS
      INTEGER NJHEM
      REAL TMPLAT, DELLAT, DELLON, XLAT0, XLON0
      IF( (HEM.EQ. GLOBAL))THEN
         NJHEM = NJ / 2
      ELSE 
         NJHEM = NJ
      ENDIF 
      DELLON = 360.0 / REAL(NI)
      XLON0 = 0.0
      DELLAT = 90.0 / REAL(NJHEM)
      XLAT0 = DELLAT * 0.5
CVDIR NOVECTOR

      DO 23002 I = 1, NPTS
         X(I) = (XLON(I) - XLON0)/DELLON + 1.0
         TMPLAT = XLAT(I)
         IF( (TMPLAT.LT. 0))THEN
            TMPLAT = -TMPLAT
         ENDIF 
         IF( (TMPLAT.GT. LROOTS(NJHEM)))THEN
            Y(I)=NJHEM+0.5*(TMPLAT-LROOTS(NJHEM))/(90.0-LROOTS(NJHEM
     $      ))
         ELSE 
            IF( (TMPLAT.LT. LROOTS(1)))THEN
               Y(I)=0.5+(0.5*TMPLAT/LROOTS(1))
            ELSE 
               GUESS=INT((TMPLAT-XLAT0)/DELLAT + 1.0)
23010          IF((LROOTS(GUESS+1).LT.TMPLAT))THEN
                  GUESS = GUESS+1
                  GOTO 23010
               ENDIF 
23012          IF((LROOTS(GUESS).GE.TMPLAT))THEN
                  GUESS = GUESS-1
                  GOTO 23012
               ENDIF 
               Y(I)=GUESS+(TMPLAT-LROOTS(GUESS))/(LROOTS(GUESS+1)-
     $            LROOTS(GUESS))
            ENDIF 
         ENDIF 
         IF( (HEM.EQ. GLOBAL))THEN
            IF( (0.EQ. MOD(NJ, 2)))THEN
               IF( (XLAT(I).GE. 0.0))THEN
                  Y(I) = Y(I) + NJHEM
               ELSE 
                  Y(I) = NJHEM - Y(I) + 1
               ENDIF 
            ELSE 
               IF( (XLAT(I).GE. 0.0))THEN
                  Y(I) = Y(I) + NJHEM + 1
               ELSE 
                  Y(I) = NJHEM - Y(I) + 1
               ENDIF 
            ENDIF 
         ENDIF 
         IF( (HEM.EQ. NORD))THEN
            IF( (XLAT(I).LT. 0.0))THEN
               Y(I) = -Y(I) + 1
            ENDIF 
         ENDIF 
         IF( (HEM.EQ. SUD))THEN
            IF( (XLAT(I).GE. 0.0))THEN
               Y(I) = Y(I) + NJ
            ELSE 
               Y(I) = NJ - Y(I) + 1.0
            ENDIF 
         ENDIF 
23002 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      BLOCK DATA QQQCOMBLK
      IMPLICIT NONE
C* ----------------------------------------------------------------------
C* COMMON QQQCOM1
C
      REAL, DIMENSION(:), POINTER :: XGD, YGD
      REAL, DIMENSION(:), POINTER :: ZTMP,AXTMP,AYTMP,CXTMP,CYTMP
      INTEGER, DIMENSION(:), POINTER :: NPOLPTS, SPOLPTS
      INTEGER XGDPTR,YGDPTR
      INTEGER ZPTR, AXPTR, AYPTR, CXPTR, CYPTR
      INTEGER NPOLPTR, SPOLPTR, NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX
      LOGICAL PTPOLN, PTPOLS
      COMMON /QQQCOM1/ XGD,YGD,ZTMP,AXTMP,AYTMP,CXTMP,CYTMP,XGDPTR,
     $YGDPTR,ZPTR,AXPTR,AYPTR,CXPTR,CYPTR,NPOLPTS,SPOLPTS,NPOLPTR,
     $ SPOLPTR,NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX,PTPOLN, PTPOLS
      DATA XGDPTR, YGDPTR, ZPTR   /   0,   0,   0  /
      DATA AXPTR, AYPTR, CXPTR, CYPTR   /   0,   0,   0,   0 /
      DATA NPOLPTR,SPOLPTR,NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX   /   0,
     $   0,   0,   0,   0,   0  /
      DATA PTPOLN, PTPOLS / .FALSE., .FALSE. /
C
C-- COMMON QQQCOM2, QQQCOM3
C-

      INTEGER I1,I2,J1,J2
      INTEGER LSTLILJ, LSTNI, LSTNJ, LSTXOR
      INTEGER LSTIG1, LSTIG2, LSTIG3, LSTIG4
      COMMON /QQQCOM2/ I1,I2,J1,J2,LSTLILJ,LSTNI,LSTNJ,LSTIG1,LSTIG2
     $,LSTIG3,LSTIG4,LSTXOR
      CHARACTER*1 LSTGTYP
      COMMON /QQQCOM3/ LSTGTYP
      DATA LSTLILJ, LSTNI, LSTNJ, LSTIG1, LSTIG2, LSTIG3, LSTIG4   /
     $   0,   0,   0,   0,   0,   0,   0 /
      DATA LSTXOR, LSTGTYP   /   0,   ' ' /
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE IGSCINT(ZO,LI,LJ,XLAT,XLON,ZI,NI,NJ,GRTYP,GRREF,IG1
     $,IG2,IG3,IG4,SYM,AX,AY,VECFLG)
      IMPLICIT NONE
      INTEGER LI,LJ,NI,NJ,IG1,IG2,IG3,IG4,BIDON
      REAL ZO(LI,LJ),XLAT(LI,LJ),XLON(LI,LJ)
      REAL ZI(NI,NJ),AX(NI),AY(NJ)
      LOGICAL SYM
      CHARACTER*1 GRTYP, GRREF
      LOGICAL QQQCMP
      EXTERNAL QQQCMP, RGOPTI
      EXTERNAL RGFREE, QQQCHK, QQQALOC, QQQKEEP, LL2RGD, LL2IGD
      EXTERNAL XPNCOF, XPNGD, NWTNCOF, IRGDINT, RGDINT
      EXTERNAL XPNAXEZ,XPNAXEG,GGGDINT,QQQGLAT,QQQXTRAP,POLRINT
      EXTERNAL XPNAXEY
C
CDEFINITION DES VARIABLES LOCALES
C
C-- COMMON QQQCOM1
C--
C
      REAL, DIMENSION(:), POINTER :: XGD, YGD
      REAL, DIMENSION(:), POINTER :: ZTMP,AXTMP,AYTMP,CXTMP,CYTMP
      INTEGER, DIMENSION(:), POINTER :: NPOLPTS, SPOLPTS
      INTEGER XGDPTR,YGDPTR
      INTEGER ZPTR, AXPTR, AYPTR, CXPTR, CYPTR
      INTEGER NPOLPTR, SPOLPTR, NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX
      LOGICAL PTPOLN, PTPOLS
      COMMON /QQQCOM1/ XGD,YGD,ZTMP,AXTMP,AYTMP,CXTMP,CYTMP,XGDPTR,
     $YGDPTR,ZPTR,AXPTR,AYPTR,CXPTR,CYPTR,NPOLPTS,SPOLPTS,NPOLPTR,
     $ SPOLPTR,NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX,PTPOLN, PTPOLS
C----------------------
C-- COMMON QQQCOM2
C-

      INTEGER I1,I2,J1,J2
      INTEGER LSTLILJ, LSTNI, LSTNJ, LSTXOR
      INTEGER LSTIG1, LSTIG2, LSTIG3, LSTIG4
      COMMON /QQQCOM2/ I1,I2,J1,J2,LSTLILJ,LSTNI,LSTNJ,LSTIG1,LSTIG2
     $,LSTIG3,LSTIG4,LSTXOR
C-
C------------
C* COMMON QQQCOM3
C

      CHARACTER*1 LSTGTYP
      COMMON /QQQCOM3/ LSTGTYP
C------------

      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      EXTERNAL QQQCOMBLK
      LOGICAL OK
      INTEGER XORSUM
      LOGICAL VECTEUR
C
C* Test if field is scalar or vector
C* ---------------------------------
C
      CHARACTER*6 VECFLG
      IF (VECFLG .EQ. 'SCALAR') THEN
          VECTEUR = .FALSE.
        ELSEIF (VECFLG .EQ. 'VECTOR') THEN
          VECTEUR = .TRUE.
        ELSE
          WRITE(6,*) ' WARNING !!! Wrong value for VECFLG '
          WRITE(6,*) ' WE STOP IN igscint                 '
          WRITE(6,*) ' '
          STOP
      ENDIF
C*----------------------
C
CVERIFICATION DU TYPE DE GRILLE
C

      IF( (GRTYP.EQ. 'Z' .OR. GRTYP .EQ. 'Y')
     $    .AND. (GRREF.EQ. 'N'.OR. GRREF.EQ. 'S'.OR.
     $    GRREF.EQ. 'L')) THEN
         GOTO 10
      ELSE 
         GOTO 101
      ENDIF 
      ENTRY IGVINT(ZO,LI,LJ,XLAT,XLON,ZI,NI,NJ,GRTYP,GRREF,IG1,IG2,
     $IG3,IG4,SYM,AX,AY)
      VECTEUR = .TRUE.
      IF( (GRTYP.EQ.'Z' .OR. GRTYP .EQ. 'Y')
     $    .AND. (GRREF.EQ. 'N'.OR. GRREF.EQ. 'S'.OR.
     $    GRREF.EQ. 'L')) THEN
         GOTO 10
      ELSE 
         GOTO 101
      ENDIF 
101   CONTINUE
      IF( GRTYP .NE. 'Z' .OR. GRTYP .NE. 'Y')THEN
         WRITE(6,*) 'Error, bad grid type (GRTYP)(IGSCINT)'
         WRITE(6,*) '''GRTYP'' should be equal to ''Z'' or ''Y'''
      ELSE 
         WRITE(6,*) 
     $    'Error, bad grid type (GRREF)(IGSCINT)'
         WRITE(6,*)
     $    '''GRREF'' should be equal to ''N'', ''S'' or ''L'''
      ENDIF 
      RETURN
      ENTRY RGVINT(ZO,LI,LJ,XLAT,XLON,ZI,NI,NJ,GRTYP,IG1,IG2,IG3,IG4
     $,SYM)
      VECTEUR = .TRUE.
      GOTO 3
      ENTRY RGSCINT(ZO,LI,LJ,XLAT,XLON,ZI,NI,NJ,GRTYP,IG1,IG2,IG3,
     $IG4,SYM,VECFLG)
C
C* Test if field is scalar or vector
C* ---------------------------------
C
      IF (VECFLG .EQ. 'SCALAR') THEN
          VECTEUR = .FALSE.
        ELSEIF (VECFLG .EQ. 'VECTOR') THEN
          VECTEUR = .TRUE.
        ELSE
          WRITE(6,*) ' WARNING !!! Wrong value for VECFLG '
          WRITE(6,*) ' WE STOP IN igscint                 '
          WRITE(6,*) ' '
          STOP
      ENDIF
C*
3     CONTINUE
      IF( (GRTYP.EQ.'A'.OR.GRTYP.EQ.'B'.OR.GRTYP.EQ.'L'.OR.GRTYP.EQ.
     $'N'.OR.GRTYP.EQ.'S'   .OR.GRTYP.EQ.'G'))THEN
         GOTO 10
      ENDIF 
      WRITE (6,200)
200   FORMAT('0','Error bad grid type (GRTYP)(RGSCINT)')
      RETURN
10    CALL RGOPTI('INTERP', BIDON, .FALSE.)
      OK = QQQCMP(XLAT,XLON,LI*LJ,NI,NJ,GRTYP,IG1,IG2,IG3,IG4,XORSUM
     $)
      IF( (.NOT.OK))THEN
         CALL XPNCOF(I1,I2,J1,J2,NI,NJ,GRTYP,GRREF,IG1,IG1,IG3,IG4,
     $   SYM,AX,AY)
         CALL RGFREE
         CALL QQQALOC(XLAT, XLON, LI, LJ, I1, I2, J1, J2)
         CALL QQQKEEP(LI*LJ,NI,NJ,GRTYP,IG1,IG2,IG3,IG4,XORSUM)
         CALL QQQCHK(XLAT, XLON, LI*LJ)
         IF( GRTYP.EQ. 'Z' .OR. GRTYP.EQ. 'Y')THEN
         IF( GRTYP.EQ. 'Z') CALL XPNAXEZ(AXTMP(AXPTR),AYTMP(AYPTR),
     $           I1,I2,J1,J2,AX,AY,NI,NJ)
         IF( GRTYP.EQ. 'Y') CALL XPNAXEY(AXTMP(AXPTR),AYTMP(AYPTR),
     $           I1,I2,J1,J2,AX,AY,NI,NJ)
            CALL NWTNCOF(CXTMP(CXPTR),CYTMP(CYPTR),AXTMP(AXPTR),
     $      AYTMP(AYPTR),I1,I2,J1,J2)
            CALL LL2IGD(XGD(XGDPTR),YGD(YGDPTR),XLAT,XLON,LI*LJ,NI,
     $      NJ,GRTYP,GRREF,IG1,IG2,IG3,IG4,SYM,AX,AY)
         ELSE 
            IF( (GRTYP.EQ. 'G'))THEN
               CALL QQQGLAT(NJ,IG1)
               CALL XPNAXEG(AXTMP(AXPTR),AYTMP(AYPTR),I1,I2,J1,J2,NI
     $         ,NJ,IG1)
               CALL NWTNCOF(CXTMP(CXPTR),CYTMP(CYPTR),AXTMP(AXPTR),
     $         AYTMP(AYPTR),I1,I2,J1,J2)
            ENDIF 
            CALL LL2RGD(XGD(XGDPTR),YGD(YGDPTR),XLAT,XLON,LI*LJ,NI,
     $      NJ,GRTYP,IG1,IG2,IG3,IG4,SYM)
         ENDIF 
      ENDIF 
      CALL XPNGD(ZTMP(ZPTR),I1,I2,J1,J2,ZI,NI,NJ,GRTYP,IG1,IG2,IG3,
     $IG4,SYM,VECTEUR)
      IF( (GRTYP.EQ. 'Z' .OR. GRTYP .EQ. 'Y'))THEN
         CALL IRGDINT(ZO,XGD(XGDPTR),YGD(YGDPTR),LI*LJ,AXTMP(AXPTR),
     $   AYTMP(AYPTR),CXTMP(CXPTR),CYTMP(CYPTR),ZTMP(ZPTR),I1,I2,J1,
     $   J2)
      ENDIF 
      IF( (GRTYP.EQ. 'G'))THEN
         CALL GGGDINT(ZO,XGD(XGDPTR),YGD(YGDPTR),LI*LJ,AYTMP(AYPTR),
     $   CYTMP(CYPTR),ZTMP(ZPTR),I1,I2,J1,J2)
      ENDIF 
      IF( (GRTYP.NE. 'G'.AND. GRTYP.NE. 'Z' 
     $    .AND. GRTYP .NE. 'Y'))THEN
         CALL RGDINT(ZO,XGD(XGDPTR),YGD(YGDPTR),LI*LJ,ZTMP(ZPTR),I1,
     $   I2,J1,J2)
      ENDIF 
      IF( (GRTYP.EQ. 'L'.OR. GRTYP.EQ. 'N'.OR. GRTYP.EQ. 'S'.OR.
     $ GRTYP.EQ. 'Z' .OR. GRTYP .EQ. 'Y'))THEN
         CALL QQQXTRAP(ZO,XGD(XGDPTR),YGD(YGDPTR),LI*LJ,ZTMP(ZPTR),
     $   I1,I2,J1,J2)
      ENDIF 
      IF( (PTPOLN.OR. PTPOLS))THEN
         CALL POLRINT(ZO,XLAT,XLON,LI*LJ,ZTMP(ZPTR),NI,NJ,I1,I2,J1,
     $   J2,GRTYP,IG1,IG2,IG3,IG4,VECTEUR)
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE IGUVINT(SPDO,PSIO,LI,LJ,XLAT,XLON,UI,VI,NI,NJ,GRTYP
     $,GRREF,IG1,IG2,IG3,IG4,SWS,AX,AY)
      IMPLICIT NONE
      INTEGER LI,LJ,NI,NJ,IG1,IG2,IG3,IG4,BIDON
      REAL SPDO(LI,LJ),PSIO(LI,LJ),XLAT(LI,LJ),XLON(LI,LJ)
      REAL UI(NI,NJ),VI(NI,NJ),AX(NI),AY(NJ)
      LOGICAL SWS
      CHARACTER*1 GRTYP, GRREF
      EXTERNAL GDW2LLW,IGSCINT,RGVINT,MODULE,RGOPTI,IGVINT
      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
C* -------------------------------------------------------------------
C
CVERIFICATION DU TYPE DE GRILLE
C

      CALL RGOPTI('INTERP', BIDON, .FALSE.)
      IF( (GRTYP.EQ.'Z' .OR. GRTYP .EQ. 'Y')
     $    .AND. (GRREF.EQ. 'N'.OR. GRREF.EQ. 'S'.OR.
     $ GRREF.EQ. 'L')) THEN
         CALL IGVINT(SPDO,LI,LJ,XLAT,XLON,UI,NI,NJ,GRTYP,GRREF,IG1,
     $   IG2,IG3,IG4,.TRUE.,AX,AY)
         CALL IGVINT(PSIO,LI,LJ,XLAT,XLON,VI,NI,NJ,GRTYP,GRREF,IG1,
     $   IG2,IG3,IG4,.FALSE.,AX,AY)
         IF( (SWS))THEN
            CALL GDW2LLW(SPDO,PSIO,XLON,LI,LJ,GRREF,IG1,IG2,IG3,IG4)
         ELSE 
            CALL MODULE(SPDO,PSIO,LI,LJ)
         ENDIF 
      ELSE 
         IF( (GRTYP.NE. 'Z' .AND. GRTYP .NE. 'Y'))THEN
            WRITE(6,*)
     $       'Error, Bad grid type (GRTYP)(IGSCINT)'
            WRITE(6,*) 
     $          '''GRTYP'' should be equal to ''Z'' or ''Y'''
         ELSE 
            WRITE(6,*)
     $ 'Error, Bad reference grid type(GRREF)(IGSCINT)'
            WRITE(6,*)
     $       '''GRREF'' should be equal to ''N'', ''S'' or ''L'''
         ENDIF 
      ENDIF 
      RETURN
      ENTRY RGUVINT(SPDO,PSIO,LI,LJ,XLAT,XLON,UI,VI,NI,NJ,GRTYP,IG1,
     $IG2,IG3,IG4,SWS)
      CALL RGOPTI('INTERP', BIDON, .FALSE.)
      IF( (GRTYP.EQ.'A'.OR.GRTYP.EQ.'B'.OR.GRTYP.EQ.'L'.OR.GRTYP.EQ.
     $'N'.OR.GRTYP.EQ.'S'   .OR.GRTYP.EQ.'G'))THEN
         CALL RGVINT(SPDO,LI,LJ,XLAT,XLON,UI,NI,NJ,GRTYP,IG1,IG2,IG3
     $   ,IG4,.TRUE.,.TRUE.)
         CALL RGVINT(PSIO,LI,LJ,XLAT,XLON,VI,NI,NJ,GRTYP,IG1,IG2,IG3
     $   ,IG4,.FALSE.,.TRUE.)
         IF( (SWS))THEN
            CALL GDW2LLW(SPDO,PSIO,XLON,LI,LJ,GRTYP,IG1,IG2,IG3,IG4)
         ELSE 
            CALL MODULE(SPDO,PSIO,LI,LJ)
         ENDIF 
      ELSE 
         WRITE (6,200)
200      FORMAT('0','Error bad grid type (GRTYP)(RGSCINT)')
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE IRGDINT(ZO,PX,PY,NPTS,AX,AY,CX,CY,Z,I1,I2,J1,J2)
      IMPLICIT NONE
C*******
C AUTEUR: Y.CHARTIER, DRPN
C        FEVRIER 1991
C
C OBJET:  INTERPOLATION BI-CUBIQUE DE POINTS A PARTIR
C        D'UNE GRILLE SOURCE IRREGULIERE.
C*******
      INTEGER NPTS,I1,I2,J1,J2
      REAL ZO(NPTS),PX(NPTS),PY(NPTS)
      REAL FA, FA2, FA3, FA4
      REAL AX(I1:I2),AY(J1:J2),CX(I1:I2,6)
      REAL CY(J1:J2,6)
      REAL Z(I1:I2,J1:J2)
C
C  NPTS   : NOMBRE DE POINTS A INTERPOLER
C  I1:I2  : DIMENSION DE LA GRILLE SOURCE SELON X
C  J1:J2  : DIMENSION DE LA GRILLE SOURCE SELON Y
C  ZO     : VECTEUR DE SORTIE CONTENANT LES VALEURS INTERPOLEES
C  PX     : VECTEUR CONTENANT LA POSITION X DES POINTS QUE L'ON
C           VEUT INTERPOLER
C  PY     : VECTEUR CONTENANT LA POSITION Y DES POINTS QUE L'ON
C           VEUT INTERPOLER
C  AX     : VECTEUR CONTENANT LA POS. DES POINTS SUR L'AXE DES X.
C  AY     : VECTEUR CONTENANT LA POS. DES POINTS SUR L'AXE DES Y.
C  CX     : VECTEUR CONTENANT UNE TABLE DE DIFFERENCES SELON X.
C  CY     : VECTEUR CONTENANT UNE TABLE DE DIFFERENCES SELON Y.
C  Z      : VALEURS DE LA GRILLE SOURCE.
C
C***************************************************************************
C
C  *   *   *   *
C
C  *   *   *   *
C        #        ==>   PT (X,Y)
C  *  (=)  *   *  ==> = PT (I, J)
C
C  *   *   *   *
C
C
C
C  CX(I,1) = 1.0 / (X2-X1)
C  CX(I,2) = 1.0 / (X3-X1)
C  CX(I,3) = 1.0 / (X3-X2)
C  CX(I,4) = 1.0 / (X4-X1)
C  CX(I,5) = 1.0 / (X4-X2)
C  CX(I,6) = 1.0 / (X4-X3)
C
C  STRUCTURE IDENTIQUE POUR CY(J,1..6)

      REAL A11,A12,A13,A14,A21,A22,A23,A24
      REAL A31,A32,A33,A34,A41,A42,A43,A44
      REAL B1,B2,B3,B4,B11,B12,B13,B14
      REAL X1,X2,X3,X4,Y1,Y2,Y3,Y4
      INTEGER I, J, N
      REAL A1,A2,A3,A4,X,Y,C1,C2,C3,C4,C5,C6
C  DEFINITION DES FONCTIONS IN-LINE

      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      REAL CUBIC, DX,DY,Z1,Z2,Z3,Z4
      REAL ZLIN, ZZ1, ZZ2, ZDX
      CUBIC(Z1,Z2,Z3,Z4,DX)=((((Z4-Z1)*0.1666666666666 + 0.5*(Z2-Z3)
     $)*DX + 0.5*(Z1+Z3)-Z2)*DX + Z3-0.1666666666666*Z4-0.5*Z2-0.
     $3333333333333*Z1)*DX+Z2
      ZLIN(ZZ1,ZZ2,ZDX) = ZZ1 + (ZZ2 - ZZ1) * ZDX
      FA(A1,A2,A3,A4,X,X1,X2,X3)=A1+(X-X1)*(A2+(X-X2)*(A3+A4*(X-X3))
     $)
      FA2(C1,A1,A2)=C1*(A2-A1)
      FA3(C1,C2,C3,A1,A2,A3)=C2*(C3*(A3-A2)-C1*(A2-A1))
      FA4(C1,C2,C3,C4,C5,C6,A1,A2,A3,A4)=C4*(C5*(C6*(A4-A3)-C3*(A3-
     $A2))   - C2*(C3*(A3-A2)-C1*(A2-A1)))
      IF( (ORDINT.EQ. CUBIQUE))THEN
         DO 23002 N=1,NPTS
            I = MIN(I2-2,MAX(I1+1,IFIX(PX(N))))
            J = MIN(J2-2,MAX(J1+1,IFIX(PY(N))))
            X = AX(I) + (AX(I+1)-AX(I))*(PX(N)-I)
            Y = AY(J) + (AY(J+1)-AY(J))*(PY(N)-J)
            X1=AX(I-1)
            X2=AX(I)
            X3=AX(I+1)
            X4=AX(I+2)
            Y1=AY(J-1)
            Y2=AY(J)
            Y3=AY(J+1)
            Y4=AY(J+2)
C        INTERPOLATION 1ERE RANGEE SELON X

            Z1=Z(I-1,J-1)
            Z2=Z(I  ,J-1)
            Z3=Z(I+1,J-1)
            Z4=Z(I+2,J-1)
            A11 = Z1
            A12 = FA2(CX(I,1),Z1,Z2)
            A13 = FA3(CX(I,1),CX(I,2),CX(I,3),Z1,Z2,Z3)
            A14 = FA4(CX(I,1),CX(I,2),CX(I,3),CX(I,4),CX(I,5),CX(I,6
     $      ),Z1,Z2,Z3,Z4)
            B1  = FA(A11,A12,A13,A14,X,X1,X2,X3)
C        INTERPOLATION 2EME RANGEE SELON X

            Z1=Z(I-1,J)
            Z2=Z(I  ,J)
            Z3=Z(I+1,J)
            Z4=Z(I+2,J)
            A21 = Z1
            A22 = FA2(CX(I,1),Z1,Z2)
            A23 = FA3(CX(I,1),CX(I,2),CX(I,3),Z1,Z2,Z3)
            A24 = FA4(CX(I,1),CX(I,2),CX(I,3),CX(I,4),CX(I,5),CX(I,6
     $      ),Z1,Z2,Z3,Z4)
            B2  = FA(A21,A22,A23,A24,X,X1,X2,X3)
C     INTERPOLATION 3EME RANGEE SELON X

            Z1=Z(I-1,J+1)
            Z2=Z(I  ,J+1)
            Z3=Z(I+1,J+1)
            Z4=Z(I+2,J+1)
            A31 = Z1
            A32 = FA2(CX(I,1),Z1,Z2)
            A33 = FA3(CX(I,1),CX(I,2),CX(I,3),Z1,Z2,Z3)
            A34 = FA4(CX(I,1),CX(I,2),CX(I,3),CX(I,4),CX(I,5),CX(I,6
     $      ),Z1,Z2,Z3,Z4)
            B3  = FA(A31,A32,A33,A34,X,X1,X2,X3)
C        INTERPOLATION 4EME RANGEE SELON X

            Z1=Z(I-1,J+2)
            Z2=Z(I  ,J+2)
            Z3=Z(I+1,J+2)
            Z4=Z(I+2,J+2)
            A41 = Z1
            A42 = FA2(CX(I,1),Z1,Z2)
            A43 = FA3(CX(I,1),CX(I,2),CX(I,3),Z1,Z2,Z3)
            A44 = FA4(CX(I,1),CX(I,2),CX(I,3),CX(I,4),CX(I,5),CX(I,6
     $      ),Z1,Z2,Z3,Z4)
            B4  = FA(A41,A42,A43,A44,X,X1,X2,X3)
C        INTERPOLATION FINALE SELON Y

            B11 = B1
            B12 = FA2(CY(J,1),B1,B2)
            B13 = FA3(CY(J,1),CY(J,2),CY(J,3),B1,B2,B3)
            B14 = FA4(CY(J,1),CY(J,2),CY(J,3),CY(J,4),CY(J,5),CY(J,6
     $      ),B1,B2,B3,B4)
            ZO(N) = FA(B11,B12,B13,B14,Y,Y1,Y2,Y3)
23002    CONTINUE 
      ENDIF 
      IF( (ORDINT.EQ. LINEAIR))THEN
         DO 23006 N=1,NPTS
            I = MIN(I2-2,MAX(I1+1,IFIX(PX(N))))
            J = MIN(J2-2,MAX(J1+1,IFIX(PY(N))))
            X = AX(I) + (AX(I+1)-AX(I))*(PX(N)-I)
            Y = AY(J) + (AY(J+1)-AY(J))*(PY(N)-J)
            DX = (X - AX(I))/(AX(I+1)-AX(I))
            DY = (Y - AY(J))/(AY(J+1)-AY(J))
            Y1 = ZLIN(Z(I,J),Z(I+1,J),DX)
            Y2 = ZLIN(Z(I,J+1),Z(I+1,J+1),DX)
            ZO(N) = ZLIN(Y1,Y2,DY)
23006    CONTINUE 
      ENDIF 
      IF( (ORDINT.EQ. VOISIN))THEN
         DO 23010 N=1,NPTS
            I = MIN(I2,MAX(I1,NINT(PX(N))))
            J = MIN(J2,MAX(J1,NINT(PY(N))))
            ZO(N) = Z(I,J)
23010    CONTINUE 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE LL2IGD(PX,PY,XLAT,XLON,NPTS,NI,NJ,GRTYP,GRREF,IG1,
     $IG2,IG3,IG4,SYM,AX,AY)
C* ----------------------------------------------------------------------
C**S/R LL2IGD - CONVERSION DE COORDONNEES LAT-LON A PTS DE GRILLE
C* ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
      EXTERNAL CIGAXG,VXYFLL,LLLL2GD,PERMUT,CHKXTRAP
      INTEGER I, NPTS, NI, NJ
      REAL PX(NPTS),PY(NPTS),XLAT(NPTS),XLON(NPTS)
      CHARACTER*1 GRTYP, GRREF
      INTEGER IG1,IG2,IG3,IG4
      LOGICAL SYM
      REAL AX(NI), AY(NJ)
      REAL PI,PJ,DGRW,D60,TMP
      REAL DLAT, DLON, XLAT0, XLON0
      INTEGER INDX, INDY
      INTEGER CHERCHE, FINDLON
      EXTERNAL CHERCHE, FINDLON 
      IF( (GRREF.EQ. 'N'.OR. GRREF.EQ. 'S'.OR. GRREF.EQ. 'L')
     $)THEN
         IF( (GRREF.EQ. 'N'))THEN
            CALL CIGAXG(GRREF,  PI, PJ, D60, DGRW, IG1, IG2, IG3,
     $       IG4)
            CALL VXYFLL(PX, PY, XLAT, XLON, NPTS, D60,DGRW,PI,PJ,1)
         ELSE 
            IF( (GRREF.EQ. 'S'))THEN
               CALL CIGAXG(GRREF,  PI, PJ, D60, DGRW, IG1, IG2, IG3,
     $          IG4)
               CALL VXYFLL(PX, PY, XLAT, XLON, NPTS, D60,DGRW,PI,PJ,
     $         2)
            ELSE 
               CALL CIGAXG(GRREF, XLAT0, XLON0, DLAT, DLON, IG1, IG2
     $         , IG3, IG4)
               CALL LLLL2GD(PX, PY, XLAT, XLON, NPTS, XLAT0, XLON0,
     $          DLAT, DLON)
            ENDIF 
         ENDIF 
         DO 23006 I=1,NPTS
            INDX = FINDLON(PX(I), AX, NI, TMP)
            INDY = CHERCHE(PY(I), AY, NJ)
            IF( (INDX.GE. NI))THEN
               INDX = NI - 1
            ENDIF 
            IF( (INDY.GE. NJ))THEN
               INDY = NJ - 1
            ENDIF 
            PX(I) = REAL(INDX) + (TMP - AX(INDX))/(AX(INDX+1)-AX(
     $      INDX))
            PY(I) = REAL(INDY) + (PY(I) - AY(INDY))/(AY(INDY+1)-AY(
     $      INDY))
23006    CONTINUE 
         CALL CHKXTRAP(PX, PY, NPTS, NI, NJ)
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE LL2RGD(PX,PY,XLAT,XLON,NPTS,NI,NJ,GRTYP,IG1,IG2,IG3
     $,IG4,SYM)
C* -----------------------------------------------------------------
C* S/R LL2RGD - CONVERSION DE COORDONNEES LAT-LON A PTS DE GRILLE
C* ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
C
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
C
C RDTODG = 180/PIE, DGTORD = PIE/180

      REAL PIE,RDTODG,DGTORD
      DATA PIE   /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C
C-- COMMON GAUSSGD
C      REAL ROOTS(1),LROOTS(1)
      REAL, DIMENSION(:), POINTER :: ROOTS,LROOTS
      INTEGER IROOTS, ILROOTS
      COMMON /GAUSSGD/ ROOTS,LROOTS,IROOTS,ILROOTS
C----------------------

      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      EXTERNAL CIGAXG,VXYFLL,LLLL2GD,PERMUT
      EXTERNAL QQQGLAT,GGLL2GD,CHKXTRAP
      INTEGER NPTS, NI, NJ
      REAL PX(NPTS),PY(NPTS),XLAT(NPTS),XLON(NPTS)
      CHARACTER*1 GRTYP
      INTEGER IG1,IG2,IG3,IG4
      LOGICAL SYM
      REAL PI,PJ,DGRW,D60
      REAL DELLAT, DELLON, XLAT0, XLON0
      INTEGER I,J
      IF( (GRTYP.EQ.'N'))THEN
         CALL CIGAXG(GRTYP,PI,PJ,D60,DGRW,IG1,IG2,IG3,IG4)
         CALL VXYFLL(PX,PY,XLAT,XLON,NPTS,D60,DGRW,PI,PJ,NORD)
         CALL CHKXTRAP(PX,PY,NPTS,NI,NJ)
      ENDIF 
      IF( (GRTYP.EQ.'S'))THEN
         CALL CIGAXG(GRTYP,PI,PJ,D60,DGRW,IG1,IG2,IG3,IG4)
         CALL VXYFLL(PX,PY,XLAT,XLON,NPTS,D60,DGRW,PI,PJ,SUD)
         CALL CHKXTRAP(PX,PY,NPTS,NI,NJ)
      ENDIF 
      IF( (GRTYP.EQ.'A'))THEN
         DELLON = 360.0 / REAL(NI)
         XLON0 = 0.0
         IF( (IG1.EQ. GLOBAL))THEN
            DELLAT = 180.0 / REAL(NJ)
            XLAT0 = -90.0 + DELLAT * 0.5
         ENDIF 
         IF( (IG1.EQ. NORD))THEN
            DELLAT = 90.0 / REAL(NJ)
            XLAT0 =  DELLAT * 0.5
         ENDIF 
         IF( (IG1.EQ. SUD))THEN
            DELLAT = 90.0 / REAL(NJ)
            XLAT0 = -90.0 + DELLAT * 0.5
         ENDIF 
         FLGXTRAP = .FALSE.
         CALL LLLL2GD(PX,PY,XLAT,XLON,NPTS,XLAT0, XLON0, DELLAT,
     $    DELLON)
      ENDIF 
      IF( (GRTYP.EQ.'B'))THEN
         DELLON = 360.0 / REAL(NI-1)
         XLON0 = 0.0
         IF( (IG1.EQ. GLOBAL))THEN
            DELLAT = 180.0 / REAL(NJ-1)
            XLAT0 = -90.0
         ENDIF 
         IF( (IG1.EQ. NORD))THEN
            DELLAT = 90.0 / REAL(NJ-1)
            XLAT0 =  0.0
         ENDIF 
         IF( (IG1.EQ. SUD))THEN
            DELLAT = 90.0 / REAL(NJ-1)
            XLAT0 = -90.0
         ENDIF 
         FLGXTRAP = .FALSE.
         CALL LLLL2GD(PX,PY,XLAT,XLON,NPTS,XLAT0, XLON0, DELLAT,
     $    DELLON)
      ENDIF 
      IF( (GRTYP.EQ. 'G'))THEN
         FLGXTRAP = .FALSE.
         CALL GGLL2GD(PX,PY,XLAT,XLON,NPTS,NI,NJ,IG1,LROOTS(ILROOTS)
     $   )
      ENDIF 
      IF( (GRTYP.EQ. 'L'))THEN
         CALL CIGAXG(GRTYP,XLAT0,XLON0,DELLAT,DELLON,IG1,IG2,IG3,IG4
     $   )
         DO 23024 I=1,NPTS
            IF( (XLON(I).LT. XLON0))THEN
               XLON(I) = XLON(I) + 360.0
            ENDIF 
            IF( (XLON(I).GT. (XLON0 + NI*DELLON)))THEN
               XLON(I) = XLON(I) - 360.0
            ENDIF 
23024    CONTINUE 
         CALL LLLL2GD(PX,PY,XLAT,XLON,NPTS,XLAT0, XLON0, DELLAT,
     $    DELLON)
         CALL CHKXTRAP(PX,PY,NPTS,NI,NJ)
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE LLLL2GD(X,Y,DLAT,DLON,NPTS,XLAT0,XLON0, DELLAT,
     $ DELLON)
C* ----------------------------------------------------------------------
C* S/R LLLL2GD - COMPUTES THE GRID CO-ORDINATES OF A POINT
C* ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPTS
      REAL X(NPTS), Y(NPTS), DLAT(NPTS), DLON(NPTS)
      REAL XLAT0, XLON0, DELLAT, DELLON
      INTEGER I
      DO 23000 I=1,NPTS
         X(I) = (DLON(I) - XLON0)/DELLON + 1.0
         Y(I) = (DLAT(I) - XLAT0)/DELLAT + 1.0
23000 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE MODULE(U,V,LI,LJ)
      INTEGER LI,LJ
      REAL U(LI,LJ), V(LI,LJ)
      INTEGER I,J
      DO 23000 I=1,LI
         DO 23002 J=1,LJ
            U(I,J)=SQRT(U(I,J)*U(I,J)+V(I,J)*V(I,J))
23002    CONTINUE 
23000 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE NWTNCOF(CX,CY,AX,AY,I1,I2,J1,J2)
C*******
C AUTEUR: Y. CHARTIER, DRPN
C        FEVRIER 1991
C
C OBJET:  CALCUL DE COEFFICIENTS UTI1ISES DANS LA FORME NEWTONIENNE
C        DE L'INTERPOLATION DE LAGRANGE.
C
C===========================================
C
C   -----*-------------*------#------*----------*------->
C        X1            X2     X      X3         X4
C
C===========================================
C     CX(I,1) = 1.0 / (X2-X1)
C     CX(I,2) = 1.0 / (X3-X1)
C     CX(I,3) = 1.0 / (X3-X2)
C     CX(I,4) = 1.0 / (X4-X1)
C     CX(I,5) = 1.0 / (X4-X2)
C     CX(I,6) = 1.0 / (X4-X3)
C
C     STRUCTURE IDENTIQUE POUR CY(J,1..6)
C*******

      IMPLICIT NONE
      INTEGER I1,I2,J1,J2
      REAL CX(I1:I2,6),CY(J1:J2,6),AX(I1:I2),AY(J1:J2)
      INTEGER I,J
      DO 23000 I=I1+1,I2-2
         CX(I,1) = 1. / (AX(I  ) - AX(I-1))
         CX(I,2) = 1. / (AX(I+1) - AX(I-1))
         CX(I,3) = 1. / (AX(I+1) - AX(I  ))
         CX(I,4) = 1. / (AX(I+2) - AX(I-1))
         CX(I,5) = 1. / (AX(I+2) - AX(I  ))
         CX(I,6) = 1. / (AX(I+2) - AX(I+1))
23000 CONTINUE 
      DO 23002 J=J1+1,J2-2
         CY(J,1) = 1. / (AY(J  ) - AY(J-1))
         CY(J,2) = 1. / (AY(J+1) - AY(J-1))
         CY(J,3) = 1. / (AY(J+1) - AY(J  ))
         CY(J,4) = 1. / (AY(J+2) - AY(J-1))
         CY(J,5) = 1. / (AY(J+2) - AY(J  ))
         CY(J,6) = 1. / (AY(J+2) - AY(J+1))
23002 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE POLRINT(ZO,XLAT,XLON,LILJ,ZI,NI,NJ,I1,I2,J1,J2,
     $GRTYP,IG1,IG2,IG3,IG4,VECTEUR)
      IMPLICIT NONE
      INTEGER LILJ,NI,NJ,IG1,IG2,IG3,IG4
      INTEGER I1,I2,J1,J2
      REAL ZO(LILJ),XLAT(LILJ),XLON(LILJ)
      REAL ZI(I1:I2,J1:J2)
      CHARACTER*1 GRTYP
      LOGICAL VECTEUR
      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
C-- COMMON QQQCOM1
C--
      REAL, DIMENSION(:), POINTER :: XGD, YGD
      REAL, DIMENSION(:), POINTER :: ZTMP,AXTMP,AYTMP,CXTMP,CYTMP
      INTEGER, DIMENSION(:), POINTER :: NPOLPTS, SPOLPTS
      INTEGER XGDPTR,YGDPTR
      INTEGER ZPTR, AXPTR, AYPTR, CXPTR, CYPTR
      INTEGER NPOLPTR, SPOLPTR, NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX
      LOGICAL PTPOLN, PTPOLS
      COMMON /QQQCOM1/ XGD,YGD,ZTMP,AXTMP,AYTMP,CXTMP,CYTMP,XGDPTR,
     $YGDPTR,ZPTR,AXPTR,AYPTR,CXPTR,CYPTR,NPOLPTS,SPOLPTS,NPOLPTR,
     $ SPOLPTR,NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX,PTPOLN, PTPOLS
C----------------------
C
CDEFINITION DES VARIABLES LOCALES
C

      INTEGER I,J
      INTEGER N1, N2, S1, S2
      REAL VPOLNOR, VPOLSUD
      IF( (VECTEUR))THEN
         RETURN
      ENDIF 
      IF( (GRTYP.EQ. 'L'.OR. GRTYP.EQ. 'N'.OR. GRTYP.EQ. 'S'.OR.
     $ GRTYP.EQ. 'Z' .OR. GRTYP .EQ. 'Y'))THEN
         RETURN
      ENDIF 
      IF( (GRTYP.EQ. 'B'))THEN
         IF( (IG1.EQ. GLOBAL))THEN
            VPOLNOR = ZI(0, NJ)
            VPOLSUD = ZI(0, 1)
         ENDIF 
         IF( (IG1.EQ. NORD))THEN
            VPOLNOR = ZI(0, NJ)
            VPOLSUD = ZI(0, -NJ+1)
         ENDIF 
         IF( (IG1.EQ. SUD))THEN
            VPOLNOR = ZI(0, 2*NJ-1)
            VPOLSUD = ZI(0, 1)
         ENDIF 
      ENDIF 
      IF( (GRTYP.EQ. 'A'.OR. GRTYP.EQ. 'G'))THEN
         VPOLNOR = 0.0
         VPOLSUD = 0.0
         IF( (IG1.EQ. GLOBAL))THEN
            N1 = NJ
            N2 = NJ - 1
            S1 = 1
            S2 = 2
         ENDIF 
         IF( (IG1.EQ. NORD))THEN
            N1 = NJ
            N2 = NJ - 1
            S1 = -NJ+1
            S2 = -NJ+2
         ENDIF 
         IF( (IG1.EQ. SUD))THEN
            N1 = 2*NJ
            N2 = 2*NJ - 1
            S1 = 1
            S2 = 2
         ENDIF 
         IF( (PTPOLN))THEN
            DO 23022 I=1,NI
               VPOLNOR = VPOLNOR + 9.0 * ZI(I, N1) + ZI(I,N2)
23022       CONTINUE 
            VPOLNOR = 0.1 * VPOLNOR / REAL(NI)
         ENDIF 
         IF( (PTPOLS))THEN
            DO 23026 I=1,NI
               VPOLSUD = VPOLSUD + 9.0 * ZI(I, S1) + ZI(I,S2)
23026       CONTINUE 
            VPOLSUD = 0.1 * VPOLSUD / REAL(NI)
         ENDIF 
      ENDIF 
      DO 23028 I=1,NPOLNUM
         ZO(NPOLPTS(NPOLPTR+I-1)) = VPOLNOR
23028 CONTINUE 
      DO 23030 I=1,SPOLNUM
         ZO(SPOLPTS(SPOLPTR+I-1)) = VPOLSUD
23030 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE QQQALOC(XLAT, XLON, LI, LJ, I1, I2, J1, J2)
      USE memoir
      IMPLICIT NONE
C      EXTERNAL MEMOIRH
      INTEGER LI,LJ,I1,I2,J1,J2
      REAL XLAT(LI,LJ), XLON(LI,LJ)
C-- COMMON QQQCOM1
C--
      REAL, DIMENSION(:), POINTER :: XGD, YGD
      REAL, DIMENSION(:), POINTER :: ZTMP,AXTMP,AYTMP,CXTMP,CYTMP
      INTEGER, DIMENSION(:), POINTER :: NPOLPTS, SPOLPTS
      INTEGER XGDPTR,YGDPTR
      INTEGER ZPTR, AXPTR, AYPTR, CXPTR, CYPTR
      INTEGER NPOLPTR, SPOLPTR, NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX
      LOGICAL PTPOLN, PTPOLS
      COMMON /QQQCOM1/ XGD,YGD,ZTMP,AXTMP,AYTMP,CXTMP,CYTMP,XGDPTR,
     $YGDPTR,ZPTR,AXPTR,AYPTR,CXPTR,CYPTR,NPOLPTS,SPOLPTS,NPOLPTR,
     $ SPOLPTR,NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX,PTPOLN, PTPOLS
C----------------------
      CALL MEMOIRH(XGD, XGDPTR, LI*LJ, 0)
      CALL MEMOIRH(YGD, YGDPTR, LI*LJ, 0)
      CALL MEMOIRH(ZTMP,ZPTR,(I2-I1+1)*(J2-J1+1), 0)
      CALL MEMOIRH(AXTMP,AXPTR,(I2-I1+1), 0)
      CALL MEMOIRH(AYTMP,AYPTR,(J2-J1+1), 0)
      CALL MEMOIRH(CXTMP,CXPTR, (I2-I1+1) * 6, 0)
      CALL MEMOIRH(CYTMP,CYPTR, (J2-J1+1) * 6, 0)
      CALL MEMOIRH(NPOLPTS, NPOLPTR, 128, 0)
      CALL MEMOIRH(SPOLPTS, SPOLPTR, 128, 0)
      NPOLMAX = 128
      SPOLMAX = 128
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE QQQCHK(XLAT, XLON, LILJ)
      USE memoir
      IMPLICIT NONE

C-- COMMON QQQCOM1
C--
      REAL, DIMENSION(:), POINTER :: XGD, YGD
      REAL, DIMENSION(:), POINTER :: ZTMP,AXTMP,AYTMP,CXTMP,CYTMP
      INTEGER, DIMENSION(:), POINTER :: NPOLPTS, SPOLPTS
      INTEGER XGDPTR,YGDPTR
      INTEGER ZPTR, AXPTR, AYPTR, CXPTR, CYPTR
      INTEGER NPOLPTR, SPOLPTR, NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX
      LOGICAL PTPOLN, PTPOLS
      COMMON /QQQCOM1/ XGD,YGD,ZTMP,AXTMP,AYTMP,CXTMP,CYTMP,XGDPTR,
     $YGDPTR,ZPTR,AXPTR,AYPTR,CXPTR,CYPTR,NPOLPTS,SPOLPTS,NPOLPTR,
     $ SPOLPTR,NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX,PTPOLN, PTPOLS
C----------------------

      INTEGER LILJ
      REAL XLAT(LILJ), XLON(LILJ)
      REAL TMP
      LOGICAL BADLAT
      INTEGER NPOLPT2,SPOLPT2
      INTEGER I,J
C  VERIF POUR POINT AU POLE

      REAL EPSILON
      DATA EPSILON /1.0E-6/
      BADLAT = .FALSE.
      DO 23000 I=1,LILJ
         TMP = XLON(I)
         XLON(I)=AMOD(AMOD(XLON(I),360.0)+360.0,360.0)
         TMP = XLAT(I)
         XLAT(I)=AMAX1(-90.0,AMIN1(90.0,XLAT(I)))
         IF( (TMP.NE. XLAT(I)))THEN
            BADLAT = .TRUE.
         ENDIF 
23000 CONTINUE 
      IF( (BADLAT))THEN
         WRITE(6,*) '<QQQGLAT> Latitudes gotten > 90.0 or < -90.0'
         WRITE(6,*)
     $    'Put latitudes back into -90.0 and +90.0'
         WRITE(6,*) 'LAT. > 90.0 = 90.0, LAT < -90.0 = -90.0'
      ENDIF 
      PTPOLN = .FALSE.
      NPOLNUM = 0
      DO 23006 I=1,LILJ
         IF( (EPSILON.GT. ABS(90.0 - XLAT(I))))THEN
            IF( (NPOLNUM.EQ. NPOLMAX))THEN
               CALL MEMOIRH(NPOLPTS, NPOLPT2, NPOLMAX+128, NPOLMAX)
               NPOLPTR = NPOLPT2
               NPOLMAX = NPOLMAX + 128
            ENDIF 
            PTPOLN = .TRUE.
            NPOLPTS(NPOLPTR+NPOLNUM) = I
            NPOLNUM = NPOLNUM + 1
         ENDIF 
23006 CONTINUE 
      PTPOLS = .FALSE.
      SPOLNUM = 0
      DO 23016 I=1,LILJ
         IF( (EPSILON.GT. ABS(-90.0 - XLAT(I))))THEN
            IF( (SPOLNUM.EQ. SPOLMAX))THEN
               CALL MEMOIRH(SPOLPTS, SPOLPT2, SPOLMAX+128, SPOLMAX)
               SPOLPTR = SPOLPT2
               SPOLMAX = SPOLMAX + 128
            ENDIF 
            PTPOLS = .TRUE.
            SPOLPTS(SPOLPTR+SPOLNUM) = I
            SPOLNUM = SPOLNUM + 1
         ENDIF 
23016 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      LOGICAL FUNCTION QQQCMP(XLAT, XLON, LILJ, NI, NJ,GRTYP, IG1,
     $ IG2, IG3, IG4, XORSUM)
      REAL XLAT(LILJ), XLON(LILJ)
      INTEGER LILJ, NI, NJ, IG1, IG2, IG3, IG4, XORSUM
      CHARACTER*1 GRTYP
      INTEGER XORCALC
      CHARACTER*4 GRTMP
C-- COMMON QQQCOM1
C--
      REAL, DIMENSION(:), POINTER :: XGD, YGD
      REAL, DIMENSION(:), POINTER :: ZTMP,AXTMP,AYTMP,CXTMP,CYTMP
      INTEGER, DIMENSION(:), POINTER :: NPOLPTS, SPOLPTS
      INTEGER XGDPTR,YGDPTR
      INTEGER ZPTR, AXPTR, AYPTR, CXPTR, CYPTR
      INTEGER NPOLPTR, SPOLPTR, NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX
      LOGICAL PTPOLN, PTPOLS
      COMMON /QQQCOM1/ XGD,YGD,ZTMP,AXTMP,AYTMP,CXTMP,CYTMP,XGDPTR,
     $YGDPTR,ZPTR,AXPTR,AYPTR,CXPTR,CYPTR,NPOLPTS,SPOLPTS,NPOLPTR,
     $ SPOLPTR,NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX,PTPOLN, PTPOLS
C----------------------
C-- COMMON QQQCOM2
C-
      INTEGER I1,I2,J1,J2
      INTEGER LSTLILJ, LSTNI, LSTNJ, LSTXOR
      INTEGER LSTIG1, LSTIG2, LSTIG3, LSTIG4
      COMMON /QQQCOM2/ I1,I2,J1,J2,LSTLILJ,LSTNI,LSTNJ,LSTIG1,LSTIG2
     $,LSTIG3,LSTIG4,LSTXOR
C-
C------------
C* COMMON QQQCOM3
C

      CHARACTER*1 LSTGTYP
      COMMON /QQQCOM3/ LSTGTYP
C------------

      QQQCMP = .TRUE.
      IF( (LILJ.NE.LSTLILJ.OR.NI.NE.LSTNI.OR.NJ.NE.LSTNJ))THEN
         QQQCMP = .FALSE.
      ENDIF 
      GRTMP = GRTYP
      IF( (IG1.NE.LSTIG1.OR.IG2.NE.LSTIG2.OR.IG3.NE.LSTIG3.OR.IG4
     $.NE.LSTIG4.OR.GRTMP.NE.LSTGTYP))THEN
         QQQCMP = .FALSE.
      ENDIF 
      XORSUM = XORCALC(XLAT, XLON, LILJ)
      IF( (XORSUM.NE.LSTXOR))THEN
         QQQCMP = .FALSE.
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      BLOCK DATA GAUSSGDBLK
      IMPLICIT NONE
C
C-- COMMON GAUSSGD
      REAL, DIMENSION(:), POINTER :: ROOTS,LROOTS
      INTEGER IROOTS, ILROOTS
      COMMON /GAUSSGD/ ROOTS,LROOTS,IROOTS,ILROOTS
C----------------------
      DATA IROOTS, ILROOTS /0, 0/
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE QQQGLAT(NJ,HEM)
C* ----------------------------------------------------------------------
C* S/R QQQGLAT - CALCUL DES LATITUDES D'UNE GRILLE GAUSSIENNE
C*
C  AUTEUR: YVES CHARTIER. MARS 1991.
C* ----------------------------------------------------------------------
      USE memoir
      IMPLICIT NONE
      INTEGER NJ, HEM
      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
C
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
C
C RDTODG = 180/PIE, DGTORD = PIE/180

      REAL PIE,RDTODG,DGTORD
      DATA PIE   /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C
C-- COMMON GAUSSGD
      REAL, DIMENSION(:), POINTER :: ROOTS,LROOTS
      INTEGER IROOTS, ILROOTS
      COMMON /GAUSSGD/ ROOTS,LROOTS,IROOTS,ILROOTS
C----------------------
      EXTERNAL DGAUSS
      EXTERNAL GAUSSGDBLK
      INTEGER J,NPOLY
      INTEGER NJ2
      IF( (IROOTS.NE. 0))THEN
         CALL MEMOIRH(ROOTS,  IROOTS, 0, 0)
      ENDIF 
      IF( (ILROOTS.NE. 0))THEN
         CALL MEMOIRH(LROOTS,ILROOTS, 0, 0)
      ENDIF 
      IF( (HEM.NE. GLOBAL))THEN
         NPOLY = NJ * 2
      ELSE 
         NPOLY = NJ
      ENDIF 
      CALL MEMOIRH(ROOTS, IROOTS, NPOLY, 0)
      CALL MEMOIRH(LROOTS,ILROOTS,NPOLY, 0)
      CALL DGAUSS(NPOLY, ROOTS(IROOTS), HEM)
      IF( (HEM.EQ. GLOBAL))THEN
          NJ2 = NJ / 2
          DO 23008 J=1,NJ2
            LROOTS(J-1+ILROOTS)=90.0-RDTODG*ACOS(ROOTS(IROOTS+NJ2-J)
     $          )
23008     CONTINUE 
      ENDIF 
      IF( (HEM.EQ. NORD))THEN
         DO 23012 J=1,NJ
            LROOTS(J-1+ILROOTS)=90.0-RDTODG*ACOS(ROOTS(IROOTS+NJ-J))
23012    CONTINUE 
      ENDIF 
      IF( (HEM.EQ. SUD))THEN
         DO 23016 J=1,NJ
            LROOTS(ILROOTS-1+J)=-(90.0-RDTODG*ACOS(ROOTS(IROOTS-1+J)
     $      ))
23016    CONTINUE 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE QQQKEEP(LILJ,NI,NJ,GRTYP,IG1,IG2,IG3,IG4,XORSUM)
      IMPLICIT NONE
      INTEGER LILJ, NI, NJ, IG1, IG2, IG3, IG4, XORSUM
      CHARACTER*1 GRTYP
C-- COMMON QQQCOM2
C-

      INTEGER I1,I2,J1,J2
      INTEGER LSTLILJ, LSTNI, LSTNJ, LSTXOR
      INTEGER LSTIG1, LSTIG2, LSTIG3, LSTIG4
      COMMON /QQQCOM2/ I1,I2,J1,J2,LSTLILJ,LSTNI,LSTNJ,LSTIG1,LSTIG2
     $,LSTIG3,LSTIG4,LSTXOR
C-
C------------
C* COMMON QQQCOM3
C

      CHARACTER*1 LSTGTYP
      COMMON /QQQCOM3/ LSTGTYP
C------------

      LSTLILJ = LILJ
      LSTNI   = NI
      LSTNJ   = NJ
      LSTGTYP = GRTYP
      LSTIG1  = IG1
      LSTIG2  = IG2
      LSTIG3  = IG3
      LSTIG4  = IG4
      LSTXOR  = XORSUM
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE QQQXTRAP(ZO, PX, PY, NPTS, Z, I1, I2, J1, J2)
      IMPLICIT NONE
      INTEGER NPTS,I1,I2,J1,J2
      REAL ZO(NPTS),PX(NPTS),PY(NPTS)
      REAL Z(I1:I2,J1:J2)
      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      INTEGER N, I, J, OFFL, OFFR
      REAL RMIN, RMAX, TEMPMIN, TEMPMAX
      IF( (.NOT.FLGXTRAP))THEN
         RETURN
      ENDIF 
      IF( (CODXTRAP.EQ. OUI))THEN
         RETURN
      ENDIF 
      IF( (ORDINT.EQ. 3))THEN
         OFFR = 2
         OFFL = 1
      ELSE 
         OFFR = 0
         OFFL = 0
      ENDIF 
      RMIN = Z(I1, J1)
      RMAX = Z(I1, J1)
      DO 23006 J=J1, J2
         DO 23008 I=I1, I2
            IF( (Z(I,J).LT. RMIN))THEN
               RMIN = Z(I,J)
            ENDIF 
            IF( (Z(I,J).GT. RMAX))THEN
               RMAX = Z(I,J)
            ENDIF 
23008    CONTINUE 
23006 CONTINUE 
      TEMPMIN = RMIN - 0.25*(RMAX - RMIN)
      TEMPMAX = RMAX + 0.25*(RMAX - RMIN)
      RMIN = TEMPMIN
      RMAX = TEMPMAX
      DO 23014 N=1, NPTS
         I = IFIX(PX(N))
         J = IFIX(PY(N))
         IF( (I.LT.(I1+OFFL).OR. J.LT.(J1+OFFL).OR. I.GT. (I2-OFFR)
     $   .OR. J.GT. (J2-OFFR)))THEN
            IF( (CODXTRAP.EQ. VOISIN))THEN
               I = MIN(I2, MAX(I1, NINT(PX(N))))
               J = MIN(J2, MAX(J1, NINT(PY(N))))
               ZO(N) = Z(I,J)
            ENDIF 
            IF( (CODXTRAP.EQ. MINIMUM))THEN
               ZO(N) = RMIN
            ENDIF 
            IF( (CODXTRAP.EQ. MAXIMUM))THEN
               ZO(N) = RMAX
            ENDIF 
            IF( (CODXTRAP.EQ. VALEUR))THEN
               ZO(N) = VALXTRAP
            ENDIF 
         ENDIF 
23014 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE RGDINT(ZO,PX,PY,NPTS,Z,I1,I2,J1,J2)
C*******
C AUTEUR: Y.CHARTIER, DRPN
C        FEVRIER 1991
C
C OBJET:  INTERPOLATION BI-CUBIQUE DE POINTS A PARTIR D'UNE GRILLE
C        SOURCE REGULIERE.
C
C*******

      IMPLICIT NONE
      INTEGER NPTS,I1,I2,J1,J2
      REAL ZO(NPTS),PX(NPTS),PY(NPTS)
      REAL Z(I1:I2,J1:J2)
C
C  NPTS   : NOMBRE DE POINTS A INTERPOLER
C  I1:I2  : DIMENSION DE LA GRILLE SOURCE SELON X
C  J1:J2  : DIMENSION DE LA GRILLE SOURCE SELON Y
C  ZO     : VECTEUR DE SORTIE CONTENANT LES VALEURS INTERPOLEES
C  PX     : VECTEUR CONTENANT LA POSITION X DES POINTS QUE L'ON
C         : VEUT INTERPOLER
C  PY     : VECTEUR CONTENANT LA POSITION Y DES POINTS QUE L'ON
C         : VEUT INTERPOLER
C  Z      : VALEURS DE LA GRILLE SOURCE.
C
C===========================================
C
C     *   *   *   *
C
C     *   *   *   *
C           #        ==>   PT (X,Y)
C     *  (=)  *   *  ==> = PT (IIND, JIND)
C
C     *   *   *   *
C
C===========================================

      REAL Y1,Y2,Y3,Y4
      INTEGER M,N,I,J,STRIDE
      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      REAL CUBIC, DX,DY,Z1,Z2,Z3,Z4
      REAL ZLIN, ZZ1, ZZ2, ZDX
      CUBIC(Z1,Z2,Z3,Z4,DX)=((((Z4-Z1)*0.1666666666666 + 0.5*(Z2-Z3)
     $)*DX + 0.5*(Z1+Z3)-Z2)*DX + Z3-0.1666666666666*Z4-0.5*Z2-0.
     $3333333333333*Z1)*DX+Z2
      ZLIN(ZZ1,ZZ2,ZDX) = ZZ1 + (ZZ2 - ZZ1) * ZDX
      STRIDE = 1
      IF( (ORDINT.EQ. CUBIQUE))THEN
         DO 23002 N=1,NPTS
            I = MIN(I2-2,MAX(I1+1,IFIX(PX(N))))
            J = MIN(J2-2,MAX(J1+1,IFIX(PY(N))))
            DX = PX(N) - I
            DY = PY(N) - J
            Y1=CUBIC(Z(I-1,J-1),Z(I  ,J-1),Z(I+1,J-1),Z(I+2,J-1),DX)
            Y2=CUBIC(Z(I-1,J  ),Z(I  ,J  ),Z(I+1,J  ),Z(I+2,J  ),DX)
            Y3=CUBIC(Z(I-1,J+1),Z(I  ,J+1),Z(I+1,J+1),Z(I+2,J+1),DX)
            Y4=CUBIC(Z(I-1,J+2),Z(I  ,J+2),Z(I+1,J+2),Z(I+2,J+2),DX)
            ZO(N)=CUBIC(Y1,Y2,Y3,Y4,DY)
23002    CONTINUE 
      ENDIF 
      IF( (ORDINT.EQ. LINEAIR))THEN
         DO 23006 N=1,NPTS
            I = MIN(I2-2,MAX(I1+1,IFIX(PX(N))))
            J = MIN(J2-2,MAX(J1+1,IFIX(PY(N))))
            DX = PX(N) - I
            DY = PY(N) - J
            Y2=ZLIN(Z(I,J  ),Z(I+1,J  ),DX)
            Y3=ZLIN(Z(I,J+1),Z(I+1,J+1),DX)
            ZO(N)=ZLIN(Y2,Y3,DY)
23006    CONTINUE 
      ENDIF 
      IF( (ORDINT.EQ. VOISIN))THEN
         DO 23010 N=1,NPTS
            I = MIN(I2,MAX(I1,NINT(PX(N))))
            J = MIN(J2,MAX(J1,NINT(PY(N))))
            ZO(N)=Z(I,J)
23010    CONTINUE 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE RGFREE()
      USE memoir
      IMPLICIT NONE
C
C* - COMMON QQQCOM1
C
      REAL, DIMENSION(:), POINTER :: XGD, YGD
      REAL, DIMENSION(:), POINTER :: ZTMP,AXTMP,AYTMP,CXTMP,CYTMP
      INTEGER, DIMENSION(:), POINTER :: NPOLPTS, SPOLPTS
      INTEGER XGDPTR,YGDPTR
      INTEGER ZPTR, AXPTR, AYPTR, CXPTR, CYPTR
      INTEGER NPOLPTR, SPOLPTR, NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX
      LOGICAL PTPOLN, PTPOLS
      COMMON /QQQCOM1/ XGD,YGD,ZTMP,AXTMP,AYTMP,CXTMP,CYTMP,XGDPTR,
     $YGDPTR,ZPTR,AXPTR,AYPTR,CXPTR,CYPTR,NPOLPTS,SPOLPTS,NPOLPTR,
     $ SPOLPTR,NPOLNUM,SPOLNUM,NPOLMAX,SPOLMAX,PTPOLN, PTPOLS
C
C* - COMMON QQQCOM2
C

      INTEGER I1,I2,J1,J2
      INTEGER LSTLILJ, LSTNI, LSTNJ, LSTXOR
      INTEGER LSTIG1, LSTIG2, LSTIG3, LSTIG4
      COMMON /QQQCOM2/ I1,I2,J1,J2,LSTLILJ,LSTNI,LSTNJ,LSTIG1,LSTIG2
     $,LSTIG3,LSTIG4,LSTXOR
C
C* - COMMON QQQCOM3
C

      CHARACTER*1 LSTGTYP
      COMMON /QQQCOM3/ LSTGTYP
C* -----------------------------------------------------------------------
      IF( (XGDPTR.NE.0))THEN
         CALL MEMOIRH(XGD, XGDPTR, 0, 0)
         XGDPTR = 0
      ENDIF 
      IF( (YGDPTR.NE.0))THEN
         CALL MEMOIRH(YGD, YGDPTR, 0, 0)
         YGDPTR = 0
      ENDIF 
      IF( (ZPTR.NE.0))THEN
         CALL MEMOIRH(ZTMP,ZPTR, 0, 0)
         ZPTR = 0
      ENDIF 
      IF( (AXPTR.NE.0))THEN
         CALL MEMOIRH(AXTMP,AXPTR, 0, 0)
         AXPTR = 0
      ENDIF 
      IF( (AYPTR.NE.0))THEN
         CALL MEMOIRH(AYTMP,AYPTR, 0, 0)
         AYPTR = 0
      ENDIF 
      IF( (CXPTR.NE.0))THEN
         CALL MEMOIRH(CXTMP, CXPTR, 0, 0)
         CXPTR = 0
      ENDIF 
      IF( (CYPTR.NE.0))THEN
         CALL MEMOIRH(CYTMP, CYPTR, 0, 0)
         CYPTR = 0
      ENDIF 
      IF( (NPOLPTR.NE.0))THEN
         CALL MEMOIRH(NPOLPTS, NPOLPTR, 0, 0)
         NPOLPTR = 0
         NPOLMAX = 0
      ENDIF 
      IF( (SPOLPTR.NE.0))THEN
         CALL MEMOIRH(SPOLPTS, SPOLPTR, 0, 0)
         SPOLPTR = 0
         SPOLMAX = 0
      ENDIF 
      LSTLILJ = 0
      LSTNI   = 0
      LSTNJ   = 0
      LSTGTYP = ' '
      LSTIG1  = 0
      LSTIG2  = 0
      LSTIG3  = 0
      LSTIG4  = 0
      LSTXOR  = 0
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE RGLL2GD(SPDO,PSIO,XLON,LI,LJ,GRTYP,IG1,IG2,IG3,IG4)
      IMPLICIT NONE
      INTEGER LI,LJ
      REAL SPDO(LI,LJ), PSIO(LI,LJ), XLON(LI,LJ)
      CHARACTER*1 GRTYP
      INTEGER IG1,IG2,IG3,IG4
      EXTERNAL CIGAXG
C
C AUTEUR   - Y. CHARTIER - AVRIL 91
C
C OBJET(RGLL2GD)
C         - PASSE DE PSIOTESSE ET DIRECTION
C         - A VENT DE GRILLE (COMPOSANTES U ET V)
C
C APPEL    - CALL RGLL2GD(SPD,PSI,LI,LJ,IYP,XG1,XG2,XG3,XG4)
C
C MODULES  - XGAIG
C
C ARGUMENTS
C  IN/OUT - SPD   - A L'ENTREE CONTIENT LA PSIOTESSE DU VENT ET
C                   A LA SORTIE LA COMPOSANTE U.
C  IN/OUT - PSI   - A L'ENTREE CONTIENT LA DIRECTION DU VENT ET
C                   A LA SORTIE LA COMPOSANTE V.
C   IN    - LI    - PREMIERE DIMENSION DES CHAMPS SPD ET PSI
C   IN    - LJ    - DEUXIEME DIMENSION DES CHAMPS SPD ET PSI
C   IN    - IGTYP  - TYPE DE GRILLE (VOIR OUVRIR)
C   IN    - XG1   - ** DESCRIPTEUR DE GRILLE (REEL),
C   IN    - XG2   -    IGTYP = 'N', PI, PJ, D60, DGRW
C   IN    - XG3   -    IGTYP = 'L', LAT0, LON0, DLAT, DLON,
C   IN    - XG4   -    IGTYP = 'A', 'B', 'G', XG1 = 0. GLOBAL,
C                                                 = 1. NORD
C                                                 = 2. SUD **
C
CMESSAGES - "ERREUR MAUVAISE GRILLE (RGLL2GD)"
C
C-------------------------------------------------------------
C
C
C
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
C
C RDTODG = 180/PIE, DGTORD = PIE/180

      REAL PIE,RDTODG,DGTORD
      DATA PIE   /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C

      INTEGER I,J
      REAL PSI,U,V
      REAL XG1, XG2, XG3, XG4
      IF( (GRTYP.EQ. 'N'))THEN
         CALL CIGAXG(GRTYP,XG1,XG2,XG3,XG4,IG1,IG2,IG3,IG4)
         DO 23002 I=1,LI
            DO 23004 J=1,LJ
               PSI =XLON(I,J)+XG4-PSIO(I,J)
               U = COS(PSI*DGTORD)*SPDO(I,J)
               V = SIN(PSI*DGTORD)*SPDO(I,J)
               SPDO(I,J) = U
               PSIO(I,J) = V
23004       CONTINUE 
23002    CONTINUE 
         RETURN
      ENDIF 
      IF( (GRTYP.EQ. 'S'))THEN
         CALL CIGAXG(GRTYP,XG1,XG2,XG3,XG4,IG1,IG2,IG3,IG4)
         DO 23008 I=1,LI
            DO 23010 J=1,LJ
               PSI =180.0 - XLON(I,J)+XG4-PSIO(I,J)
               U = COS(PSI*DGTORD)*SPDO(I,J)
               V = SIN(PSI*DGTORD)*SPDO(I,J)
               SPDO(I,J) = U
               PSIO(I,J) = V
23010       CONTINUE 
23008    CONTINUE 
         RETURN
      ENDIF 
      IF( (GRTYP.EQ.'A'.OR.GRTYP.EQ.'B'.OR.GRTYP.EQ.'G'.OR.GRTYP.EQ.
     $'L'))THEN
         DO 23014 I=1,LI
            DO 23016 J=1,LJ
               PSI = 270.0 - PSIO(I,J)
               U = COS(PSI*DGTORD)*SPDO(I,J)
               V = SIN(PSI*DGTORD)*SPDO(I,J)
               SPDO(I,J) = U
               PSIO(I,J) = V
23016       CONTINUE 
23014    CONTINUE 
         RETURN
      ENDIF 
      WRITE(6, 600) GRTYP
600   FORMAT('0',' Error, bad grid (RGLL2GD) - GRTYP = ', A1)
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE RGOPTC(OP, VAL, FLAG)
      IMPLICIT NONE
      CHARACTER *(*) OP, VAL
      LOGICAL FLAG
CFLAG = .TRUE.  MODE SET
CFLAG = .FALSE. MODE GET

      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      IF( (FLAG))THEN
         IF( (OP.EQ.  'EXTRAP'))THEN
            IF( (VAL.EQ.  'OUI'))THEN
               CODXTRAP = OUI
            ELSE 
               IF( (VAL.EQ.  'ABORT'))THEN
                  CODXTRAP = ABORT
               ELSE 
                  IF( (VAL.EQ.  'MAXIMUM'))THEN
                     CODXTRAP = MAXIMUM
                  ELSE 
                     IF( (VAL.EQ.  'MINIMUM'))THEN
                        CODXTRAP = MINIMUM
                     ELSE 
                        IF( (VAL.EQ.  'VOISIN'))THEN
                           CODXTRAP = VOISIN
                        ELSE 
                           IF( (VAL.EQ.  'VALEUR'))THEN
                              CODXTRAP = VALEUR
                           ELSE 
                              WRITE(6,*)
     $                         ' <RGOPTC>: bad value for VAL'
                              WRITE(6,*) '           VAL = ', VAL
                              WRITE(6,*)
     $                         '           VAL initialized to ''ABORT'''
                              CODXTRAP = ABORT
                           ENDIF 
                        ENDIF 
                     ENDIF 
                  ENDIF 
               ENDIF 
            ENDIF 
         ELSE 
            IF( (OP.EQ.  'INTERP'.OR.  OP.EQ.  'INTERP'))THEN
               IF( (VAL.EQ.  'VOISIN'))THEN
                  ORDINT = VOISIN
               ELSE 
                  IF( (VAL.EQ.  'LINEAIR'))THEN
                     ORDINT = LINEAIR
                  ELSE 
                     IF( (VAL.EQ.  'CUBIQUE'))THEN
                        ORDINT = CUBIQUE
                     ELSE 
                        WRITE(6,*)
     $                   ' <RGOPTC>: bad value for VAL'
                        WRITE(6,*) '           VAL = ', VAL
                        WRITE(6,*)
     $                   '           VAL initialized to ''CUBIQUE'''
                        ORDINT = CUBIQUE
                     ENDIF 
                  ENDIF 
               ENDIF 
            ELSE 
               WRITE(6,*) ' <RGOPTC>: bad value for OP'
               WRITE(6,*)
     $ '           OP should be equal to ''EXTRAP'' or ''INTERP'''
            ENDIF 
         ENDIF 
      ELSE 
         IF( (OP.EQ.  'EXTRAP'))THEN
            IF( (CODXTRAP.EQ.  OUI))THEN
               VAL = 'OUI'
            ELSE 
               IF( (CODXTRAP.EQ.  ABORT))THEN
                  VAL = 'ABORT'
               ELSE 
                  IF( (CODXTRAP.EQ.  MAXIMUM))THEN
                     VAL = 'MAXIMUM'
                  ELSE 
                     IF( (CODXTRAP.EQ.  MINIMUM))THEN
                        VAL = 'MINIMUM'
                     ELSE 
                        IF( (CODXTRAP.EQ.  VOISIN))THEN
                           VAL = 'VOISIN'
                        ELSE 
                           IF( (CODXTRAP.EQ.  VALEUR))THEN
                              VAL = 'VALEUR'
                           ELSE 
                              WRITE(6,*)
     $ ' <RGOPTC>: bad value for CODXTRAP'
                              WRITE(6,*) '           CODXTRAP = ',
     $                         CODXTRAP
                              VAL = 'SCRAP'
                           ENDIF 
                        ENDIF 
                     ENDIF 
                  ENDIF 
               ENDIF 
            ENDIF 
         ELSE 
            IF( (OP.EQ.  'INTERP'))THEN
               IF( (ORDINT.EQ.  VOISIN))THEN
                  VAL = 'VOISIN'
               ELSE 
                  IF( (ORDINT.EQ.  LINEAIR))THEN
                     VAL = 'LINEAIR'
                  ELSE 
                     IF( (ORDINT.EQ.  CUBIQUE))THEN
                        VAL = 'CUBIQUE'
                     ELSE 
                        WRITE(6,*)
     $                   ' <RGOPTC>: bad value for ORDINT'
                        WRITE(6,*) '           ORDINT = ', ORDINT
                        VAL = 'SCRAP'
                     ENDIF 
                  ENDIF 
               ENDIF 
            ELSE 
               WRITE(6,*) ' <RGOPTC>: bad value for OP'
               WRITE(6,*)
     $ '           OP should be equal to ''EXTRAP'' OU ''INTERP'''
            ENDIF 
         ENDIF 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      BLOCK DATA QQQXTRPBLK
      IMPLICIT NONE
      INTEGER OUI
      PARAMETER (OUI   =   1)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      DATA CODXTRAP / OUI /
      DATA FLGXTRAP / .FALSE. /
      DATA ORDINT   / 3 /
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE RGOPTI(OP, VAL, FLAG)
      IMPLICIT NONE
      CHARACTER *(*) OP
      INTEGER VAL
      LOGICAL FLAG
CFLAG = .TRUE.  MODE SET
CFLAG = .FALSE. MODE GET

      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      EXTERNAL QQQXTRPBLK
      IF( (FLAG))THEN
         IF( (OP.EQ.  'EXTRAP'))THEN
            VALXTRAP = REAL(VAL)
         ELSE 
            IF( (OP.EQ.  'INTERP'))THEN
               IF( (VAL.EQ.  VOISIN.OR.  VAL.EQ.  LINEAIR.OR.  VAL
     $         .EQ. CUBIQUE))THEN
                  ORDINT = VAL
               ELSE 
                  WRITE(6,*) ' <RGOPTI>: bad value for VAL'
                  WRITE(6,*) '           VAL = ', VAL
                  WRITE(6,*)
     $             '           VAL initialized to ''CUBIQUE'''
                  ORDINT = CUBIQUE
               ENDIF 
            ELSE 
               WRITE(6,*) ' <RGOPTI>: bad value for OP'
               WRITE(6,*)
     $ '           OP should be equal to ''EXTRAP'' or ''INTERP'''
            ENDIF 
         ENDIF 
      ELSE 
         IF( (OP.EQ.  'EXTRAP'))THEN
            VAL = NINT(VALXTRAP)
         ELSE 
            IF( (OP.EQ.  'INTERP'))THEN
               IF( (ORDINT.EQ.  VOISIN.OR.  ORDINT.EQ.  LINEAIR.OR.
     $          ORDINT.EQ.  CUBIQUE))THEN
                  VAL = ORDINT
               ELSE 
                  WRITE(6,*) ' <RGOPTI>: bad value for ORDINT'
                  WRITE(6,*) '           ORDINT = ', ORDINT
                  VAL = -1
               ENDIF 
            ELSE 
               WRITE(6,*) ' <RGOPTI>: bad value for OP'
               WRITE(6,*)
     $ '           OP should be equal to ''EXTRAP'' or ''INTERP'''
            ENDIF 
         ENDIF 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE RGOPTR(OP, VAL, FLAG)
      IMPLICIT NONE
      CHARACTER *(*) OP
      REAL VAL
      LOGICAL FLAG
C  FLAG = .TRUE.  MODE SET
C  FLAG = .FALSE. MODE GET

      INTEGER VOISIN, LINEAIR, CUBIQUE
      INTEGER OUI, ABORT, VALEUR, MAXIMUM, MINIMUM
      PARAMETER (VOISIN  =   0)
      PARAMETER (LINEAIR =   1)
      PARAMETER (CUBIQUE =   3)
      PARAMETER (OUI   =   1)
      PARAMETER (MINIMUM =   2)
      PARAMETER (MAXIMUM =   3)
      PARAMETER (VALEUR  =   4)
      PARAMETER (ABORT   =  13)
      LOGICAL FLGXTRAP
      INTEGER CODXTRAP, ORDINT
      REAL VALXTRAP
      COMMON /QQQXTRP/ ORDINT, FLGXTRAP, CODXTRAP, VALXTRAP
      IF( (FLAG))THEN
         IF( (OP.EQ. 'EXTRAP'))THEN
            VALXTRAP = VAL
         ELSE 
            IF( (OP.EQ. 'INTERP'))THEN
               IF( (VAL.EQ. VOISIN.OR. VAL.EQ. LINEAIR.OR. VAL.EQ.
     $          CUBIQUE))THEN
                  ORDINT = NINT(VAL)
               ELSE 
                  WRITE(6,*)' <RGOPTR>: bad value for VAL'
                  WRITE(6,*) '           VAL = ', VAL
                  WRITE(6,*)
     $             '           VAL initialized to ''CUBIQUE'''
                  ORDINT = CUBIQUE
               ENDIF 
            ELSE 
               WRITE(6,*) ' <RGOPTR>: bad value for OP'
               WRITE(6,*)
     $ '           OP should be equal to ''EXTRAP'' or ''INTERP'''
            ENDIF 
         ENDIF 
      ELSE 
         IF( (OP.EQ. 'EXTRAP'))THEN
            VAL = VALXTRAP
         ELSE 
            IF( (OP.EQ. 'INTERP'))THEN
               IF( (ORDINT.EQ. VOISIN.OR. ORDINT.EQ. LINEAIR.OR.
     $          ORDINT.EQ. CUBIQUE))THEN
                  VAL = REAL(ORDINT)
               ELSE 
                  WRITE(6,*) ' <RGOPTR>: bad value for ORDINT'
                  WRITE(6,*) '           ORDINT = ', ORDINT
                  VAL = REAL(ORDINT)
               ENDIF 
            ELSE 
               WRITE(6,*) ' <RGOPTR>: bad value for OP'
               WRITE(6,*)
     $ '           OP should be equal to ''EXTRAP'' OU ''INTERP'''
            ENDIF 
         ENDIF 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE VLLFXY(DLAT,DLON,X,Y,NI,NJ,D60,DGRW,PI,PJ,NHEM)
C* ----------------------------------------------------------------------
C* S/R VLLFXY(I) - COMPUTES THE GRID CO-ORDINATES OF A POINT
C* ----------------------------------------------------------------------
      IMPLICIT NONE
C* ----------------------------------------------------------------------
C* ARGUMENTS
C   OUT   - DLAT - LATITUDE IN DEGREES (-90 TO +90, POSITIVE N).
C         - DLON - LONGITUDE IN DEGREES (-180 TO +180, POSITIVE E).
C   IN    - X    - X-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
C                  AS ORIGIN
C         - Y    - Y-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
C                  AS ORIGIN
C         - D60  - GRID LENGTH (IN METRES) OF THE POLAR STEREOGRAPHIC
C                  GRID AT 60 DEGREES
C         - DGRW - ORIENTATION OF GREENWICH MERIDIAN WITH RESPECT TO
C                  THE GRID (IN DEGREES)
C         - NHEM - 1 FOR NORTHERN HEMISPHERE
C                  2 FOR SOUTHERN HEMISPHERE
C
CNOTES    - THE COMPANION ROUTINE XYFLL, WHICH COMPUTES THE GRID
C           CO-ORDINATES GIVEN THE LATITUDE AND LONGITUDE, IS ALSO
C           AVAILABLE.
C
C-----------------------------------------------------------------------
C
C
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
C
C RDTODG = 180/PIE, DGTORD = PIE/180
      REAL PIE,RDTODG,DGTORD
      DATA PIE   /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C

      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
      INTEGER NI,NJ, NHEM
      REAL X(NI,NJ), Y(NI,NJ), DLAT(NI,NJ), DLON(NI,NJ)
      REAL X1, Y1
      REAL D60, DGRW, PI, PJ
      REAL RE,RE2,R2,RLON,RLAT,SINLAT,R
      INTEGER I,J
      RE=1.866025*6.371E+6/D60
      RE2=RE**2
      DO 23000 I=1,NI
         DO 23002 J=1,NJ
            X1 = X(I,J) - PI
            Y1 = Y(I,J) - PJ
            IF( (X1.EQ. 0..AND. Y1.EQ. 0.))THEN
               DLAT(I,J)=90.
               DLON(I,J)=0.

C
C  CALCULATE LONGITUDE IN MAP COORDINATES.
C
            ENDIF 
            IF((X1.EQ. 0.))THEN
               DLON(I,J)=SIGN(90.,Y1)
            ENDIF 
            IF((X1.NE. 0.))THEN
               DLON(I,J)=ATAN(Y1/X1)*RDTODG
            ENDIF 
            IF((X1.LT.  0.))THEN
               DLON(I,J)=DLON(I,J)+SIGN(180.,Y1)
C
C  * ADJUST LONGITUDE FOR GRID ORIENTATION.
C

            ENDIF 
            DLON(I,J)=DLON(I,J)-DGRW
            IF((DLON(I,J).GT.  180.))THEN
               DLON(I,J)=DLON(I,J)-360.
            ENDIF 
            IF((DLON(I,J).LT. -180.))THEN
               DLON(I,J)=DLON(I,J)+360.
C
C     * CALCULATE LATITUDE.
C

            ENDIF 
            R2=X1**2+Y1**2
            DLAT(I,J)=(RE2-R2)/(RE2+R2)
            DLAT(I,J)= ASIN(DLAT(I,J))*RDTODG
C
C     CHANGE SIGNS IF IN SOUTHERN HEMISPHERE.
C

            IF((NHEM.EQ. 2))THEN
               DLAT(I,J)=-DLAT(I,J)
            ENDIF 
            IF((NHEM.EQ. 2))THEN
               DLON(I,J)=-DLON(I,J)
            ENDIF 
23002    CONTINUE 
23000 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE VXYFLL(X,Y,DLAT,DLON,NPTS,D60,DGRW,PI,PJ,NHEM)
      IMPLICIT NONE

C AUTHOR   - J.D. HENDERSON  -  FEB 75
C
C OBJECT(VXYFLL)
C     - COMPUTES THE GRID CO-ORDINATES MEASURED FROM THE POLE OF A
C       POINT, GIVEN THE LATITUDE AND LONGITUDE IN DEGREES.
C
C USAGE    - CALL VXYFLL(X,Y,DLAT,DLON,NPTS,D60,DGRW,PI,PJ,NHEM)
C
C ARGUMENTS
C   OUT   - X    - X-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
C                  AS ORIGIN
C         - Y    - Y-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
C                  AS ORIGIN
C   IN    - DLAT - LATITUDE IN DEGREES (-90 TO +90, POSITIVE N)
C         - DLON - LONGITUDE IN DEGREES (-180 TO +180, POSITIVE E)
C         - D60  - GRID LENGTH (IN METRES) OF THE POLAR STEREOGRAPHIC
C                  GRID AT 60 DEGREES
C         - DGRW - ORIENTATION OF GREENWICH MERIDIAN WITH RESPECT TO
C                  THE GRID (IN DEGREES)
C         - NHEM - 1 FOR NORTHERN HEMISPHERE
C                  2 FOR SOUTHERN HEMISPHERE
C
C NOTES    - THE COMPANION ROUTINE LLFXY, WHICH COMPUTES THE LATITUDE
C         - AND LONGITUDE GIVEN THE GRID-COORDINATES,
C         - IS ALSO AVAILABLE.
C*--------------------------------------------------------------------
C
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
C
C RDTODG = 180/PIE, DGTORD = PIE/180
      REAL PIE,RDTODG,DGTORD
      DATA PIE   /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C

      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
      INTEGER NPTS, NHEM
      REAL X(NPTS), Y(NPTS), DLAT(NPTS), DLON(NPTS)
      REAL D60, DGRW, PI, PJ
      REAL RE,RLON,RLAT,SINLAT,R
      INTEGER I
      RE=1.866025*6.371E+6/D60
C

      IF( (NHEM.EQ.NORD))THEN
         DO 23002 I=1,NPTS
            RLON=DGTORD*(DLON(I)+DGRW)
            RLAT=DGTORD*DLAT(I)
            SINLAT=SIN(RLAT)
            R=RE*SQRT((1.-SINLAT)/(1.+SINLAT))
            X(I)=R*COS(RLON) + PI
            Y(I)=R*SIN(RLON) + PJ
23002    CONTINUE 
         RETURN
      ENDIF 
      IF( (NHEM.EQ.SUD))THEN
         DO 23006 I=1,NPTS
            RLON = DLON(I)
            IF( (RLON.GT. 180.0))THEN
               RLON = RLON - 360.0
            ENDIF 
            RLON=DGTORD*(-RLON+DGRW)
            RLAT=DGTORD*(-DLAT(I))
            SINLAT=SIN(RLAT)
            R=RE*SQRT((1.-SINLAT)/(1.+SINLAT))
            X(I)=R*COS(RLON)+PI
            Y(I)=R*SIN(RLON)+PJ
23006    CONTINUE 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      INTEGER FUNCTION XORCALC(ILAT, ILON, LILJ)
      INTEGER ILAT(LILJ), ILON(LILJ)
      INTEGER LILJ
      XORCALC = 0
      DO 23000 I=1,LILJ,17
         XORCALC = IEOR(IEOR(XORCALC,ILAT(I)),ILON(I))
23000 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE XPNAXEG(NEWAXEX,NEWAXEY,I1,I2,J1,J2,NI,NJ,HEM)
      IMPLICIT NONE
      INTEGER I1,I2,J1,J2,NI,NJ,HEM
      REAL NEWAXEX(I1:I2)
      REAL NEWAXEY(J1:J2)
C
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
C
C RDTODG = 180/PIE, DGTORD = PIE/180

      REAL PIE,RDTODG,DGTORD
      DATA PIE   /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C
C-- COMMON GAUSSGD
      REAL, DIMENSION(:), POINTER :: ROOTS,LROOTS
      INTEGER IROOTS, ILROOTS
      COMMON /GAUSSGD/ ROOTS,LROOTS,IROOTS,ILROOTS
C----------------------

      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
      INTEGER I,J
      DO 23000 I=1,NI
         NEWAXEX(I) = REAL(I)
23000 CONTINUE 
      NEWAXEX(0) = NEWAXEX(NI) - REAL(NI)
      NEWAXEX(NI+1) = NEWAXEX(1) + REAL(NI)
      NEWAXEX(NI+2) = NEWAXEX(2) + REAL(NI)
      IF( (HEM.EQ. NORD))THEN
         DO 23004 J=1,NJ
            NEWAXEY(J) = LROOTS(ILROOTS-1+J)
23004    CONTINUE 
         NEWAXEY(NJ+1) = 180.0 - NEWAXEY(NJ)
         NEWAXEY(NJ+2) = 180.0 - NEWAXEY(NJ-1)
         DO 23006 J=-NJ+1,0
            NEWAXEY(J) = -(NEWAXEY(-J+1))
23006    CONTINUE 
         NEWAXEY(-NJ  ) = -180.0 - NEWAXEY(-NJ+1)
         NEWAXEY(-NJ-1) = -180.0 - NEWAXEY(-NJ+2)
      ENDIF 
      IF( (HEM.EQ. SUD))THEN
         DO 23010 J=1,NJ
            NEWAXEY(J) = -(LROOTS(ILROOTS-1+NJ-J+1))
            NEWAXEY(NJ+J) = LROOTS(ILROOTS-1+NJ-J+1)
23010    CONTINUE 
         NEWAXEY(0 ) = -180.0 - NEWAXEY(1)
         NEWAXEY(-1) = -180.0 - NEWAXEY(2)
         NEWAXEY(NJ+1) = 180.0 - NEWAXEY(2*NJ)
         NEWAXEY(NJ+2) = 180.0 - NEWAXEY(2*NJ-1)
      ENDIF 
      IF( (HEM.EQ. GLOBAL))THEN
         IF( (0.EQ. MOD(NJ,2)))THEN
            DO 23016 J=1,NJ/2
               NEWAXEY(J)   =  -(LROOTS(ILROOTS-1+NJ/2-J+1))
               NEWAXEY(NJ/2+J) = LROOTS(ILROOTS-1+J)
23016       CONTINUE 
         ELSE 
            DO 23018 J=1,NJ/2
               NEWAXEY(J)   =  -(LROOTS(ILROOTS-1+NJ/2-J+1))
               NEWAXEY(NJ/2+J+1) = LROOTS(ILROOTS-1+J)
23018       CONTINUE 
            NEWAXEY(NJ/2+1) = 0.0
         ENDIF 
         NEWAXEY(0 ) = -180.0 - NEWAXEY(1)
         NEWAXEY(-1) = -180.0 - NEWAXEY(2)
         NEWAXEY(NJ+1) = 180.0 - NEWAXEY(NJ)
         NEWAXEY(NJ+2) = 180.0 - NEWAXEY(NJ-1)
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE XPNAXEZ(NEWAXEX,NEWAXEY,I1,I2,J1,J2,AX,AY,NI,NJ)
      IMPLICIT NONE
      INTEGER I1,I2,J1,J2,NI,NJ
      REAL NEWAXEX(I1:I2)
      REAL NEWAXEY(I1:I2)
      REAL AX(NI)
      REAL AY(NJ)
      INTEGER I,J
      DO 23000 I=1,NI
         NEWAXEX(I) = AX(I)
23000 CONTINUE 
      NEWAXEX(0) = NEWAXEX(NI) - AX(NI)
      NEWAXEX(NI+1) = NEWAXEX(1) + AX(NI)
      NEWAXEX(NI+2) = NEWAXEX(2) + AX(NI)
      DO 23002 J=1,NJ
         NEWAXEY(J) = AY(J)
23002 CONTINUE 
      NEWAXEY(0 ) = -180.0 - NEWAXEY(1)
      NEWAXEY(-1) = -180.0 - NEWAXEY(2)
      NEWAXEY(NJ+1) = 180.0 - NEWAXEY(NJ)
      NEWAXEY(NJ+2) = 180.0 - NEWAXEY(NJ-1)
cLT
         WRITE(*,*) NEWAXEY(0 ), NEWAXEY(-1), NEWAXEY(NJ+1), 
     $       NEWAXEY(NJ+2)
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE XPNAXEY(NEWAXEX,NEWAXEY,I1,I2,J1,J2,AX,AY,NI,NJ)
      IMPLICIT NONE
      INTEGER I1,I2,J1,J2,NI,NJ
      REAL NEWAXEX(NI)
      REAL NEWAXEY(NJ)
      REAL AX(NI)
      REAL AY(NJ)
      INTEGER I,J
      DO 23000 I=1,NI
         NEWAXEX(I) = AX(I)
23000 CONTINUE 
      DO 23002 J=1,NJ
         NEWAXEY(J) = AY(J)
23002 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE XPNCOF(I1,I2,J1,J2,NI,NJ,GRTYP,GRREF,IG1,IG2,IG3,
     $IG4,SYM,AX,AY)
      IMPLICIT NONE
      INTEGER I1,J1,I2,J2,NI,NJ,IG1,IG2,IG3,IG4
      LOGICAL SYM
      CHARACTER*1 GRTYP, GRREF
      REAL AX(NI),AY(NJ)
      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
      IF( (GRTYP.EQ.'L'.OR.GRTYP.EQ.'N'.OR.GRTYP.EQ.'S'.OR.GRTYP.EQ.
     $'Y'))THEN
         I1 = 1
         I2 = NI
         J1 = 1
         J2 = NJ
         RETURN
      ENDIF 
      IF ( (GRTYP .EQ. 'Z')) THEN
         I1 = 0
         I2 = NI + 2
         J1 = -1
         J2 = NJ + 2
      ENDIF         
      IF( (GRTYP.EQ.'A'.OR. GRTYP.EQ.'G'))THEN
         I1 = 0
         I2 = NI + 2
         IF( (IG1.EQ.GLOBAL))THEN
            J1 = -1
            J2 = NJ + 2
         ELSE 
            IF( (IG1.EQ.NORD))THEN
               J1 = -NJ - 1
               J2 =  NJ + 2
            ELSE 
               J1 = -1
               J2 =  2 * NJ + 2
            ENDIF 
         ENDIF 
         RETURN
      ENDIF 
      IF( (GRTYP.EQ.'B'))THEN
         I1 = 0
         I2 = NI + 1
         IF( (IG1.EQ.GLOBAL))THEN
            J1 = 0
            J2 = NJ + 1
         ELSE 
            IF( (IG1.EQ.NORD))THEN
               J1 = -NJ + 1
               J2 =  NJ + 1
            ELSE 
               J1 =  0
               J2 =  2 * NJ
            ENDIF 
         ENDIF 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE XPNGD(ZOUT,I1,I2,J1,J2,ZI,NI,NJ,GRTYP,IG1,IG2,IG3,
     $IG4,SYM,VECT)
      IMPLICIT NONE
      INTEGER GLOBAL, NORD, SUD, SUDNORD, NORDSUD
      PARAMETER (GLOBAL = 0)
      PARAMETER (NORD   = 1)
      PARAMETER (SUD   = 2)
      PARAMETER (SUDNORD= 0)
      PARAMETER (NORDSUD= 1)
      EXTERNAL PERMUT
      INTEGER I1,I2,J1,J2,NI,NJ
      INTEGER IG1, IG2, IG3, IG4
      CHARACTER*1 GRTYP
      INTEGER I,J
      REAL ZOUT(I1:I2,J1:J2)
      REAL ZI(NI,NJ)
      LOGICAL SYM,VECT
      REAL SIGN
      IF( (VECT))THEN
         SIGN = -1.0
      ELSE 
         SIGN = 1.0
      ENDIF 
      IF( (GRTYP.EQ. 'L'.OR. GRTYP.EQ. 'N'.OR. GRTYP.EQ. 'S'.OR.
     $ GRTYP.EQ. 'Y'))THEN
         DO 23004 J=1,NJ
            DO 23006 I=1,NI
               ZOUT(I,J) = ZI(I,J)
23006       CONTINUE 
23004    CONTINUE 
         RETURN
      ENDIF
      IF( (GRTYP.EQ. 'Z') ) THEN
          DO J=1,NJ
            DO I=1,NI
              ZOUT(I,J) = ZI(I,J)
            END DO 
          END DO
          DO J=1,NJ
            ZOUT(0,J) = ZI(NI,J)
            ZOUT(NI+1,J) = ZOUT(1,J)
            ZOUT(NI+2,J) = ZOUT(2,J)
          END DO 
          DO I=0,NI/2
            ZOUT(I,0) = SIGN * ZOUT(I+NI/2, 1)
            ZOUT(I,-1)= SIGN * ZOUT(I+NI/2, 2)
            ZOUT(I,NJ+1)=SIGN * ZOUT(I+NI/2, NJ)
            ZOUT(I,NJ+2)=SIGN * ZOUT(I+NI/2, NJ-1)
            ZOUT(I+NI/2,0) = SIGN * ZOUT(I, 1)
            ZOUT(I+NI/2,-1)= SIGN * ZOUT(I, 2)
            ZOUT(I+NI/2,NJ+1)=SIGN * ZOUT(I, NJ)
            ZOUT(I+NI/2,NJ+2)=SIGN * ZOUT(I, NJ-1)
          END DO 
          DO I=1,2
            ZOUT(NI+I,0) = SIGN * ZOUT(NI/2+I,1)
            ZOUT(NI+I,-1)= SIGN * ZOUT(I+NI/2, 2)
            ZOUT(NI+I,NJ+1)=SIGN * ZOUT(I+NI/2, NJ)
            ZOUT(NI+I,NJ+2)=SIGN * ZOUT(I+NI/2, NJ-1)
          END DO 
      ENDIF
      IF( (GRTYP.EQ.'A'.OR. GRTYP.EQ.'G'))THEN
         IF( (IG2.EQ. NORDSUD))THEN
            CALL PERMUT(ZI, NI, NJ)
         ENDIF 
         DO 23012 J=1,NJ
            DO 23014 I=1,NI
               ZOUT(I,J) = ZI(I,J)
23014       CONTINUE 
23012    CONTINUE 
         DO 23016 J=1,NJ
            ZOUT(0,J) = ZI(NI,J)
            ZOUT(NI+1,J) = ZOUT(1,J)
            ZOUT(NI+2,J) = ZOUT(2,J)
23016    CONTINUE 
         IF( (IG1.EQ. GLOBAL))THEN
            DO 23020 I=0,NI/2
               ZOUT(I,0) = SIGN * ZOUT(I+NI/2, 1)
               ZOUT(I,-1)= SIGN * ZOUT(I+NI/2, 2)
               ZOUT(I,NJ+1)=SIGN * ZOUT(I+NI/2, NJ)
               ZOUT(I,NJ+2)=SIGN * ZOUT(I+NI/2, NJ-1)
               ZOUT(I+NI/2,0) = SIGN * ZOUT(I, 1)
               ZOUT(I+NI/2,-1)= SIGN * ZOUT(I, 2)
               ZOUT(I+NI/2,NJ+1)=SIGN * ZOUT(I, NJ)
               ZOUT(I+NI/2,NJ+2)=SIGN * ZOUT(I, NJ-1)
23020       CONTINUE 
            DO 23022 I=1,2
               ZOUT(NI+I,0) = SIGN * ZOUT(NI/2+I,1)
               ZOUT(NI+I,-1)= SIGN * ZOUT(I+NI/2, 2)
               ZOUT(NI+I,NJ+1)=SIGN * ZOUT(I+NI/2, NJ)
               ZOUT(NI+I,NJ+2)=SIGN * ZOUT(I+NI/2, NJ-1)
23022       CONTINUE 
         ENDIF 
         IF( (IG1.EQ. NORD))THEN
            IF( (SYM))THEN
               DO 23028 J=-NJ+1, 0
                  DO 23030 I=0,NI+2
                     ZOUT(I,J) = ZOUT(I, -J+1)
23030             CONTINUE 
23028          CONTINUE 
            ELSE 
               DO 23032 J=-NJ+1, 0
                  DO 23034 I=0,NI+2
                     ZOUT(I,J) = -ZOUT(I, -J+1)
23034             CONTINUE 
23032          CONTINUE 
            ENDIF 
            DO 23036 I=0,NI/2
               ZOUT(I,-NJ) = SIGN * ZOUT(I+NI/2, -NJ+1)
               ZOUT(I,-NJ-1)= SIGN * ZOUT(I+NI/2,-NJ+2)
               ZOUT(I,NJ+1)=SIGN * ZOUT(I+NI/2, NJ)
               ZOUT(I,NJ+2)=SIGN * ZOUT(I+NI/2, NJ-1)
               ZOUT(I+NI/2,-NJ) = SIGN * ZOUT(I, -NJ+1)
               ZOUT(I+NI/2,-NJ-1)= SIGN * ZOUT(I, -NJ+2)
               ZOUT(I+NI/2,NJ+1)=SIGN * ZOUT(I, NJ)
               ZOUT(I+NI/2,NJ+2)=SIGN * ZOUT(I, NJ-1)
23036       CONTINUE 
            DO 23038 I=1,2
               ZOUT(NI+I,-NJ) = SIGN * ZOUT(NI/2+I,-NJ+1)
               ZOUT(NI+I,-NJ-1)= SIGN * ZOUT(I+NI/2, -NJ+2)
               ZOUT(NI+I,NJ+1)=SIGN * ZOUT(I+NI/2, NJ)
               ZOUT(NI+I,NJ+2)=SIGN * ZOUT(I+NI/2, NJ-1)

23038       CONTINUE 
         ENDIF 
         IF( (IG1.EQ. SUD))THEN
            IF( (SYM))THEN
               DO 23044 J=1,NJ
                  DO 23046 I=0,NI+2
                     ZOUT(I,NJ+J) = ZOUT(I, NJ-J+1)
23046             CONTINUE 
23044          CONTINUE 
            ELSE 
               DO 23048 J=1,NJ
                  DO 23050 I=0,NI+2
                     ZOUT(I,NJ+J) = -ZOUT(I,NJ-J+1)
23050             CONTINUE 
23048          CONTINUE 
            ENDIF 
            DO 23052 I=0,NI/2
               ZOUT(I,0) = SIGN * ZOUT(I+NI/2, 1)
               ZOUT(I,-1)= SIGN * ZOUT(I+NI/2, 2)
               ZOUT(I,2*NJ+1)=SIGN * ZOUT(I+NI/2, 2*NJ)
               ZOUT(I,2*NJ+2)=SIGN * ZOUT(I+NI/2, 2*NJ-1)
               ZOUT(I+NI/2,0) = SIGN * ZOUT(I, 1)
               ZOUT(I+NI/2,-1)= SIGN * ZOUT(I,2)
               ZOUT(I+NI/2,2*NJ+1)=SIGN * ZOUT(I, 2*NJ)
               ZOUT(I+NI/2,2*NJ+2)=SIGN * ZOUT(I, 2*NJ-1)
23052       CONTINUE 
            DO 23054 I=1,2
               ZOUT(NI+I,0) = SIGN * ZOUT(NI/2+I,1)
               ZOUT(NI+I,1)= SIGN * ZOUT(I+NI/2, 2)
               ZOUT(NI+I,2*NJ+1)=SIGN * ZOUT(I+NI/2, 2*NJ)
               ZOUT(NI+I,2*NJ+2)=SIGN * ZOUT(I+NI/2, 2*NJ-1)
23054       CONTINUE 
         ENDIF 
      ENDIF 
      IF( (GRTYP.EQ.'B'))THEN
         IF( (IG2.EQ. NORDSUD))THEN
            CALL PERMUT(ZI, NI, NJ)
         ENDIF 
         DO 23060 J=1,NJ
            DO 23062 I=1,NI
               ZOUT(I,J) = ZI(I,J)
23062       CONTINUE 
23060    CONTINUE 
         DO 23064 J=1,NJ
            ZOUT(0,J) = ZI(NI-1,J)
            ZOUT(NI+1,J) = ZOUT(2,J)
23064    CONTINUE 
         IF( (IG1.EQ. GLOBAL))THEN
            DO 23068 I=0,NI/2+1
               ZOUT(I,0) = SIGN * ZOUT(I+NI/2, 2)
               ZOUT(I,NJ+1)=SIGN * ZOUT(I+NI/2, NJ-1)
               ZOUT(I+NI/2,0) = SIGN * ZOUT(I, 2)
               ZOUT(I+NI/2,NJ+1)=SIGN * ZOUT(I, NJ-1)
23068       CONTINUE 
            ZOUT(NI+1,0) = SIGN * ZOUT(NI/2+1,2)
            ZOUT(NI+1,NJ+1)=SIGN * ZOUT(NI/2+1, NJ-1)
         ENDIF 
         IF( (IG1.EQ. NORD))THEN
            IF( (SYM))THEN
               DO 23074 J=-NJ+2, 0
                  DO 23076 I=0,NI+1
                     ZOUT(I,J) = ZOUT(I, -J+2)
23076             CONTINUE 
23074          CONTINUE 
            ELSE 
               DO 23078 J=-NJ+2, 0
                  DO 23080 I=0,NI+1
                     ZOUT(I,J) = -ZOUT(I, -J+2)
23080             CONTINUE 
23078          CONTINUE 
            ENDIF 
            DO 23082 I=0,NI/2+1
               ZOUT(I,-NJ+1) = SIGN * ZOUT(I+NI/2, -NJ+3)
               ZOUT(I,NJ+1)=SIGN * ZOUT(I+NI/2, NJ-1)
               ZOUT(I+NI/2,-NJ+1) = SIGN * ZOUT(I, -NJ+3)
               ZOUT(I+NI/2,NJ+1)=SIGN * ZOUT(I, NJ-1)
23082       CONTINUE 
            ZOUT(NI+1,-NJ+1) = SIGN * ZOUT(NI/2+1,-NJ+3)
            ZOUT(NI+1,NJ+1)=SIGN * ZOUT(1+NI/2, NJ-1)
         ENDIF 
         IF( (IG1.EQ. SUD))THEN
            IF( (SYM))THEN
               DO 23088 J=1,NJ-1
                  DO 23090 I=0,NI+1
                     ZOUT(I,NJ+J) = ZOUT(I, NJ-J)
23090             CONTINUE 
23088          CONTINUE 
            ELSE 
               DO 23092 J=1,NJ-1
                  DO 23094 I=0,NI+1
                     ZOUT(I,NJ+J) = -ZOUT(I,NJ-J)
23094             CONTINUE 
23092          CONTINUE 
            ENDIF 
            DO 23096 I=0,NI/2+1
               ZOUT(I,0) = SIGN * ZOUT(I+NI/2, 2)
               ZOUT(I,2*NJ)=SIGN * ZOUT(I+NI/2, 2*NJ-2)
               ZOUT(I+NI/2,0) = SIGN * ZOUT(I, 2)
               ZOUT(I+NI/2,2*NJ)=SIGN * ZOUT(I, 2*NJ-2)
23096       CONTINUE 
            ZOUT(NI+1,0) = SIGN * ZOUT(NI/2+1,2)
            ZOUT(NI+1,2*NJ)=SIGN * ZOUT(1+NI/2, 2*NJ-2)
         ENDIF 
      ENDIF 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE LLFXY(DLAT,DLON,X,Y,D60,DGRW,NHEM) 
C
C AUTHOR   - J.D. HENDERSON  -  FEB 75
C 
C OBJECT(LLFXY)
C         - COMPUTES LATITUDE AND LONGITUDE OF A POINT IN A POLAR
C           STEREOGRAPHIC GRID FROM CO-ORDINATES IN THE GRID MEASURED
C           FROM THE POLE.
C
C USAGE    - CALL LLFXY(DLAT,DLON,X,Y,D60,DGRW,NHEM)
C
C ARGUMENTS
C   OUT   - DLAT - LATITUDE IN DEGREES (-90 TO +90, POSITIVE N).
C         - DLON - LONGITUDE IN DEGREES (-180 TO +180, POSITIVE E).
C   IN    - X    - X-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
C                  AS ORIGIN
C         - Y    - Y-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
C                  AS ORIGIN
C         - D60  - GRID LENGTH (IN METRES) OF THE POLAR STEREOGRAPHIC
C                  GRID AT 60 DEGREES
C         - DGRW - ORIENTATION OF GREENWICH MERIDIAN WITH RESPECT TO
C                  THE GRID (IN DEGREES)
C         - NHEM - 1 FOR NORTHERN HEMISPHERE
C                  2 FOR SOUTHERN HEMISPHERE 
C
C NOTES    - THE COMPANION ROUTINE XYFLL, WHICH COMPUTES THE GRID
C           CO-ORDINATES GIVEN THE LATITUDE AND LONGITUDE, IS ALSO
C           AVAILABLE.
C
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.
C     * RDTODG = 180/PIE, DGTORD = PIE/180                                     
C
      DATA PIE    /3.1415926535898/
      DATA RDTODG /57.295779513082/
      DATA DGTORD /1.7453292519943E-2/
C* ------------------------------------------------------------------------
      RE=1.866025*6.371E+6/D60 
      RE2=RE**2
C     * IF POINT IS AT POLE SET COORD TO (0.,90.). 
      DLAT=90.                 
      DLON=0.
      IF(X.EQ.0. .AND. Y.EQ.0.) GO TO 39
C                                                 
C     * CALCULATE LONGITUDE IN MAP COORDINATES.
C
      IF(X.EQ.0.) DLON=SIGN(90.,Y)
      IF(X.NE.0.) DLON=ATAN(Y/X)*RDTODG
      IF(X.LT.0.) DLON=DLON+SIGN(180.,Y)
C                  
C     * ADJUST LONGITUDE FOR GRID ORIENTATION. 
C   
      DLON=DLON-DGRW 
      IF(DLON.GT.+180.) DLON=DLON-360.
      IF(DLON.LT.-180.) DLON=DLON+360. 
C              
C     * CALCULATE LATITUDE. 
C                 
      R2=X**2+Y**2
      DLAT=(RE2-R2)/(RE2+R2) 
      DLAT= ASIN(DLAT)*RDTODG 
C   
C     * CHANGE SIGNS IF IN SOUTHERN HEMISPHERE.
C
   39 IF(NHEM.EQ.2) DLAT=-DLAT
      IF(NHEM.EQ.2) DLON=-DLON 
C 
      RETURN
      END 
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
C 
      SUBROUTINE XYFLL(X,Y,DLAT,DLON,D60,DGRW,NHEM) 
C  
C AUTHOR   - J.D. HENDERSON  -  FEB 75
C    
C OBJECT(XYFLL) 
C         - COMPUTES THE GRID CO-ORDINATES MEASURED FROM THE POLE OF A 
C           POINT, GIVEN THE LATITUDE AND LONGITUDE IN DEGREES.
C                                                                              
C USAGE    - CALL XYFLL(X,Y,DLAT,DLON,D60,DGRW,NHEM) 
C             
C ARGUMENTS   
C   OUT   - X    - X-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE 
C                  AS ORIGIN    
C                  AS ORIGIN
C         - Y    - Y-CO-ORDINATE OF THE POINT AS MEASURED WITH POLE
C                  AS ORIGIN      
C   IN    - DLAT - LATITUDE IN DEGREES (-90 TO +90, POSITIVE N) 
C         - DLON - LONGITUDE IN DEGREES (-180 TO +180, POSITIVE E) 
C         - D60  - GRID LENGTH (IN METRES) OF THE POLAR STEREOGRAPHIC  
C                  GRID AT 60 DEGREES
C         - DGRW - ORIENTATION OF GREENWICH MERIDIAN WITH RESPECT TO 
C                  THE GRID (IN DEGREES)      
C         - NHEM - 1 FOR NORTHERN HEMISPHERE    
C                  2 FOR SOUTHERN HEMISPHERE  
C 
C NOTES    - THE COMPANION ROUTINE LLFXY, WHICH COMPUTES THE LATITUDE   
C           AND LONGITUDE GIVEN THE GRID-COORDINATES, IS ALSO AVAILABLE.
C                                                                
C---------------------------------------------------------------------------
C                                                     
C     * 1.866025=(1+SIN60),   6.371E+6=EARTH RADIUS IN METERS.    
C      
C RDTODG = 180/PIE, DGTORD = PIE/180    
      DATA PIE    /3.1415926535898/       
      DATA RDTODG /57.295779513082/ 
      DATA DGTORD /1.7453292519943E-2/  
C 
      RE=1.866025*6.371E+6/D60    
C   
      GLON=DLON  
      IF(NHEM.EQ.2) GLON=-DLON  
      GLAT=DLAT     
      IF(NHEM.EQ.2) GLAT=-DLAT   
C    
      RLON=DGTORD*(GLON+DGRW)      
      RLAT=DGTORD*GLAT   
      SINLAT=SIN(RLAT)    
      R=RE*SQRT((1.-SINLAT)/(1.+SINLAT)) 
      X=R*COS(RLON)        
      Y=R*SIN(RLON)                                                            
C                                                                              
      RETURN                                              
      END                                  
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE ORDLEG(SX,COA,IR)   
C     
C AUTEUR   - D. ROBERTSON 
C  
C OBJET(ORDLEG)   
C         - THIS ROUTINE IS A SUBSET OF BELOUSOVS ALGORITHM  
C           USED TO CALCULATE ORDINARY LEGENDRE POLYNOMIALS.  
C                                        
C USAGE    - CALL ORDLEG(SX,COA,IR)
C   
C ARGUMENTS        
C   OUT   - SX  - LEGENDRE POLYNOMIAL EVALUATED AT COA        
C   IN    - COA - COSINE OF COLATITUDE   
C   IN    - IR  - WAVE NUMBER  
C                                         
C* -------------------------------------------------------------------------
C               
C              
C RDTODG = 180/PIE, DGTORD = PIE/180     
      DATA PIE    /3.1415926535898/ 
      DATA RDTODG /57.295779513082/   
      DATA DGTORD /1.7453292519943E-2/    
C      
      PI = PIE                        
      SQR2=SQRT(2.)                  
      IRPP = IR + 1      
      IRPPM = IRPP - 1  
      DELTA =   ACOS(COA)  
      SIA =  SIN(DELTA)   
C                              
      THETA=DELTA           
      C1=SQR2    
C                                     
      DO 20 N=1,IRPPM                              
      FN=FLOAT(N)                           
      FN2=2.*FN                             
      FN2SQ=FN2*FN2                  
      C1=C1* SQRT(1.0-1.0/FN2SQ)     
   20 CONTINUE                        
C             
      N=IRPPM              
      ANG=FN*THETA                            
      S1=0.0                    
      C4=1.0                                          
      A=-1.0                               
      B=0.0                                       
      N1=N+1          
C                        
      DO 27 KK=1,N1,2                 
      K=KK-1                   
      IF (K.EQ.N) C4=0.5*C4           
      S1=S1+C4* COS(ANG)         
      A=A+2.0        
      B=B+1.0         
      FK=FLOAT(K)      
      ANG=THETA*(FN-FK-2.0)      
      C4=(A*(FN-B+1.0)/(B*(FN2-A)))*C4  
   27 CONTINUE   
C                               
      SX=S1*C1                                  
C       
      RETURN     
      END 
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      LOGICAL FUNCTION VALIDE(NOM, ICHK, MIN, MAX)
C
C AUTEUR   - P. SARRAZIN  - AVRIL 82
C
C OBJET(VALIDE)
C         - VERIFIER SI ICHK => MIN OU =< MAX
C         - MESSAGE SI ICHK N'EST PAS DANS LES LIMITES
C
C ARGUMENTS
C   IN    - NOM   - NOM DE LA VARIABLE EMPLOYE PAR LA ROUTINE
C   IN    - ICHK  - VALEUR DE LA VARIABLE POUR VERIFICATION
C   IN    - MIN   - VALEUR MINIMUM DE ICHK
C   IN    - MAX   - VALEUR MAXIMUM DE ICHK
C
C MODULES - SCINT - UVINT
C
C* ----------------------------------------------------------------------
      IF( (ICHK.LT.MIN .OR. ICHK.GT.MAX))THEN
         WRITE(6,600) NOM,ICHK,MIN,MAX
      ENDIF 
600   FORMAT("Bad value for",I10,"VALEUR=",I10,"MINIMUM=",
     $   I10,"MAXIMUM=",I10)
      VALIDE = (ICHK.GE.MIN) .AND. (ICHK.LE.MAX)
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE PERMUT(Z,NI,NJ)
      REAL Z(NI,NJ)
C
C AUTEUR   - M. VALIN  -  FEV 82
C
C OBJET(PERMUT)
C         - ROTATION D'UNE MATRICE AUTOUR DE LA LIGNE DU MILIEU.
C           CETTE ROUTINE EST UTILISEE POUR RE=ARRANGER LES
C           DONNEES DANS UN CHAMP. EX: POUR CONTOURER, ON A
C           DES DONNEES DANS L'ORDRE SUIVANT, DU SUD AU NORD
C           ALORS QUE POUR L'INTERPOLATION ELLES DOIVENT ETRE
C           ETALEES DU NORD AU SUD.
C
C APPEL    - CALL PERMUT(Z,NI,NJ)
C
C ARGUMENTS
C IN/OUT  - Z  - CHAMP QUI SUBIT LA ROTATION
C   IN    - NI - PREMIERE DIMENSION DE Z
C   IN    - NJ - DEUXIEME DIMENSION DE Z
C* -----------------------------------------------------------------------
C
      NCC = NJ/2
      DO 23000 J=1,NCC
         DO 23002 I=1,NI
            T = Z(I,NJ+1-J)
            Z(I,NJ+1-J) = Z(I,J)
            Z(I,J) = T
23002    CONTINUE 
23000 CONTINUE 
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
C    
      SUBROUTINE CIGAXG(CGTYP,XG1,XG2,XG3,XG4,IG1,IG2,IG3,IG4)
      CHARACTER*1 CGTYP
C
C AUTEUR   - M. VALIN  -  FEV 82
C
C
C OBJET(CIGAXG)
C         - PASSE DES PARAMETRES (ENTIERS) DESCRIPTEURS DE GRILLE
C           AUX PARAMETRES REELS.
C
C APPEL    - CALL IGAXG(CGTYP,XG1,XG2,XG3,XG4,IG1,IG2,IG3,IG4)
C
C ARGUMENTS
C   IN    - CGTYP - TYPE DE GRILLE (VOIR OUVRIR)
C   OUT   - XG1   - ** DESCRIPTEUR DE GRILLE (REEL),
C   OUT   - XG2   -    IGTYP = 'N', PI, PJ, D60, DGRW
C   OUT   - XG3   -    IGTYP = 'L', LAT0, LON0, DLAT, DLON,
C   OUT   - XG4   -    IGTYP = 'A', 'B', 'G', XG1 = 0. GLOBAL,
C                                                 = 1. NORD
C                                                 = 2. SUD **
C   IN    - IG1   - DESCRIPTEUR DE GRILLE (ENTIER) VOIR OUVRIR
C   IN    - IG2   - DESCRIPTEUR DE GRILLE (ENTIER) VOIR OUVRIR
C   IN    - IG3   - DESCRIPTEUR DE GRILLE (ENTIER) VOIR OUVRIR
C   IN    - IG4   - DESCRIPTEUR DE GRILLE (ENTIER) VOIR OUVRIR
C
CMESSAGES - "ERREUR, MAUVAISE SPECIFICATION DE GRILLE, (TYPE) (IGAXG)"
C
C-------------------------------------------------------------------
C

      IF( ((CGTYP.EQ. 'N') .OR. (CGTYP.EQ.'S')))THEN
         IF((IG4.LT. 32768))THEN
            XG1 = IG2 * 0.1
            XG2 = IG1 * 0.1
            XG3 = IG4 * 100.
            XG4 = IG3 * 0.01
         ELSE 
            JG3 = IG3
            JG4 = IG4
            JG4 = JG4 - 32768
            XG3 = IG1 * 100.
            IF((IG3 .GT. 32767))THEN
               XG3 = XG3 * 10.
               JG3 = JG3 - 32768
            ENDIF 
            XG4 = IG2 * .1
            IF((JG4.GT. 16383))THEN
               XG4 = 360. - XG4
               JG4 = JG4 - 16384
            ENDIF 
            DLAT = 90. -(JG4*180./16383.)
            DLON = (JG3*360./32767.)
            IHEM = 1
            IF(('S'.EQ.CGTYP))THEN
               IHEM = 2
            ENDIF 
            CALL XYFLL(XG1,XG2,DLAT,DLON,XG3,XG4,IHEM)
            XG1 = 1.0 - XG1
            XG2 = 1.0 - XG2
         ENDIF 
      ELSE 
         IF((CGTYP.EQ. 'C'))THEN
            XG1 = IG3 * 0.01 - 90.
            XG2 = IG4 * 0.01
            XG3 = 180. / IG1
            XG4 = 360. / IG2
         ELSE 
            IF( ((CGTYP.EQ. 'A') .OR. (CGTYP.EQ. 'B') .OR.   (CGTYP
     $      .EQ. 'G')))THEN
               XG1 = IG1
               XG2 = IG2
               XG3 = 0.
               XG4 = 0.
            ELSE 
               IF((CGTYP.EQ. 'L'))THEN
                  XG1 = IG3 * 0.01 - 90.
                  XG2 = IG4 * 0.01
                  XG3 = IG1 * 0.01
                  XG4 = IG2 * 0.01
               ELSE 
                  IF((CGTYP.EQ. 'H'))THEN
                     XG1 = IG3
                     XG2 = .01*IG4 - 90.
                     XG3 = 500*IG2
                     XG4 = IG1*.2
                  ELSE 
                     WRITE(6,600)
                  ENDIF 
               ENDIF 
            ENDIF 
         ENDIF 
      ENDIF 
600   FORMAT(' Error bad grid specification , (TYPE)'
     $,'(IGAXG)')
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      SUBROUTINE CXGAIG(CGTYP,IG1,IG2,IG3,IG4,XG1,XG2,XG3,XG4)
      LOGICAL VALIDE
      EXTERNAL VALIDE
      CHARACTER * 1 CGTYP
      LOGICAL LFLAG
C
C AUTEUR   - M. VALIN  -  FEV 82
C
C OBJET(XGAIG)
C         - PASSE DES PARAMETRES (REELS) DESCRIPTEURS DE GRILLE
C           AUX PARAMETRES ENTIERS.
C
C APPEL    - CALL XGAIG(CGTYP,IG1,IG2,IG3,IG4,XG1,XG2,XG3,XG4)
C
C ARGUMENTS
C   IN    - CGTYP - TYPE DE GRILLE (VOIR OUVRIR)
C   OUT   - IG1   - DESCRIPTEUR DE GRILLE (ENTIER) VOIR OUVRIR
C   OUT   - IG2   - DESCRIPTEUR DE GRILLE (ENTIER) VOIR OUVRIR
C   OUT   - IG3   - DESCRIPTEUR DE GRILLE (ENTIER) VOIR OUVRIR
C   OUT   - IG4   - DESCRIPTEUR DE GRILLE (ENTIER) VOIR OUVRIR
C   IN    - XG1   - ** DESCRIPTEUR DE GRILLE (REEL),
C   IN    - XG2   -    IGTYP = 'N', PI, PJ, D60, DGRW
C   IN    - XG3   -    IGTYP = 'L', LAT0, LON0, DLAT, DLON,
C   IN    - XG4   -    IGTYP = 'A', 'B', 'G', XG1 = 0, GLOBAL
C                                                 = 1, NORD
C                                                 = 2, SUD **
C
C MESSAGES - "ERREUR DANS LA DESCRIPTION DE LA GRILLE (IG1) (XGAIG)"
C           "ERREUR, MAUVAISE SPECIFICATION (LAT0) (XGAIG)"
C           "ERREUR, GRILLE INCONNUE (TYPE) (XGAIG)"
C
C* ------------------------------------------------------------------
C
C
      IF( (CGTYP.EQ. 'N' .OR. CGTYP.EQ.'S'))THEN
         IG1 = NINT(XG2 * 10.)
         IG2 = NINT(XG1 * 10.)
         IG3 = NINT(XG4 * 100.)
         IG4 = NINT(XG3 * 0.01)
23002    IF( (IG3.LT. 0))THEN
            IG3 = IG3 + 36000
            GOTO 23002
         ENDIF 
         IF((IG1.LT.0.OR.IG2.LT.0.OR.IG1.GT.2047.OR.IG2.GT.2047.OR.
     $   IG4.GT.32000))THEN
            IG1 = 0
            IG2 = 0
            IG3 = 0
            IG4 = 32768
            IF((XG3.GT. 204700))THEN
               IG3 = 32768
               IG1 = NINT(XG3*.001)
            ELSE 
               IG3 = 0
               IG1 = NINT(XG3*.01)
            ENDIF 
            IG2 = NINT(XG4*10)
            IF((IG2.LT.0))THEN
               IG2 = ABS(IG2)
               IG4 = IG4 + 16384
            ENDIF 
            IF((IG2.GT.1800))THEN
               IG2 = ABS(IG2 - 3600)
               IG4 = IG4 + 16384
            ENDIF 
            IHEM = 1
            IF(('S'.EQ.CGTYP))THEN
               IHEM = 2
            ENDIF 
            CALL LLFXY(DLAT,DLON,1.-XG1,1.-XG2,XG3,XG4,IHEM)
            DLAT = 90. - DLAT
            IF((DLON.LT.0))THEN
               DLON = DLON + 360.
            ENDIF 
            IG3 = IG3 + NINT(DLON*32767./360.)
            IG4 = IG4 + NINT(DLAT*16383./180.)
         ENDIF 
      ELSE 
         IF( (CGTYP.EQ. 'A' .OR. CGTYP.EQ. 'B' .OR.   CGTYP.EQ. 'G')
     $   )THEN
            IG1 = XG1
            IG2 = XG2
            IG3 = 0
            IG4 = 0
            LFLAG=VALIDE("IG1",IG1,0,2)
            LFLAG=VALIDE("IG2",IG2,0,1)
         ELSE 
            IF((CGTYP.EQ. 'C'))THEN
               IG1 = 180. / XG3 + 0.5
               IG2 = 360. / XG4 + 0.5
               IG3 = (90. + XG1) * 100. + 0.5
               IG4 = XG2 * 100. + 0.5
23020          IF( (IG4.LT. 0))THEN
                  IG4 = IG4 + 36000
                  GOTO 23020
               ENDIF 
               IF( (IG3.LT. 0))THEN
                  WRITE(6,601)
               ENDIF 
            ELSE 
               IF( (CGTYP.EQ. 'H'))THEN
                  IG1 = NINT(5.*XG4)
23026             IF( (IG1.LT. 0))THEN
                     IG1 = IG1 + 1800
                     GOTO 23026
                  ENDIF 
                  IG2 = NINT(.002*XG3)
                  IG3 = NINT(XG1)
                  IG4 = NINT(100.*(90.+XG2))
               ELSE 
                  IF( (CGTYP.EQ. 'L'))THEN
                     IG1 = XG3 * 100. + 0.5
                     IG2 = XG4 * 100. + 0.5
                     IG3 = (90. + XG1) * 100. + 0.5
                     IG4 = XG2 * 100. + 0.5
23030                IF( (IG4.LT. 0))THEN
                        IG4 = IG4 + 36000
                        GOTO 23030
                     ENDIF 
                     IF( (IG3.LT. 0))THEN
                        WRITE(6,601)
                     ENDIF 
                  ELSE 
                     WRITE(6,602)
                  ENDIF 
               ENDIF 
            ENDIF 
         ENDIF 
      ENDIF 
601   FORMAT(' Error, bad specification (LAT0) (XGAIG)')
602   FORMAT(' Error, unknown grid (TYPE) (XGAIG)')
      RETURN
      END
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
