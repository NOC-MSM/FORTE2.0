C
C Piece of code needed to replace the Nikos radiation scheme with
C the Morcrette radiation scheme in the igcm3.1.
C See morcrette3_1_1.readme for details.
C
C
c      program lh
c
c      parameter(NL=23)
c      real P(NL),t(NL),h2o(NL),o3(NL),htlw(NL),htsw(NL)
c      real swalb,doy
c       REAL CF(4,2)     !cloud fractions,lwps
c       INTEGER IC(4,2)   ! cloud positions
c       REAL RFLUXES(2,2,2)
c      open(11,file='mls23.dat')
c      read(11,*) NLEV
c      DOY=80
c      swalb=0.07
c      alat=40.0
c       doy=float(i)
c      do i=nlev,1,-1
c         read(11,*) P(i),t(i),h2o(i),adum,o3(i)
c         print *,i,p(i),t(i),h2o(i),o3(i)
c      enddo
c
c       ic(1,1)=3
c      ic(1,2)=17
c       ic(3,1)=6
c       ic(2,2)=6
c       cf(1,1)=0.3
c       cf(1,2)=45.0
c        cf(3,1)=0.8
c       cf(4,2)=20.0
c      alon=0.0
c      call morcigcm(p,t,h2o,o3,alat,htlw,htsw,doy,cf,ic,
c     $     rfluxes,swalb,alon)
c      enddo
c      do i=nl,1,-1
c         print '(i6,4F14.4)',i,p(i),t(i),htsw(i),htlw(i)
c      enddo
c      print *,RFLUXES(1,1,1), RFLUXES(1,1,2), RFLUXES(1,2,1),
c     $     RFLUXES(1,2,2)
c      print *,RFLUXES(2,1,1), RFLUXES(2,1,2), RFLUXES(2,2,1),
c     $     RFLUXES(2,2,2)
c
c
c      end
*DECK ZMORC
C PMF 13-8-97 Modified to provide interface to IGCM and include his
C dignostic clouds - similar ro morcigcm_clouds.f
C      PROGRAM NEWRAD
C
       SUBROUTINE MORCIGCM(PR,T,H2O,O3,alat,HTLW,HTSW,DOY,cf,ic,
     $     rfluxes,SWALB,alon)
C
C modified by Piers Forster for plug and play IGCM fun
C 29.10.96 and again 1-7-97, For clouds

C P  Pressure on Full levels(Pa) level 1 is ground
C T Temperature on Full levels(K) level 1 is ground
C H2O water vapour mmr on full levels level 1 is ground
C O3 ozone mmr on full levels level 1 is ground
C DOY Julian Day wanted (real)
C NLEV number of levles
C ALAT Latitude in degrees
c alon longitude in degrees
C HTLW Output: lomgwave Heating Rate full levels etc (K/day)
C HTSW Output: shortwave heating rate (K/DAY)
C RFLUXES  Array to hold fluxes at top and bottom of atmosphere
C     1st index - flux 1=SW, 2=LW
C     2nd index - Direction 1=DN, 2=UP
C     3rd index - Where 1=TOP, 2=SURFACE
C SWALB SW albedo
C                 -----------
C  Driving program for the extended atmospheric radiation scheme
C   of Morcrette.
*CALL ZPARB 
c       INCLUDE 'zparb.upd'
C
C Piers'  stuff (defines arrays)
       REAL PR(NRLEV),T(NRLEV),H2O(NRLEV),O3(NRLEV),alat
       real htlw(nrlev),htsw(nrlev),swalb,DOY,alon
       REAL CF(4,2)     !cloud fractions,lwps
       INTEGER IC(4,2)   ! cloud positions
       REAL RFLUXES(2,2,2)
C
C
C          1.     DEFINITIONS
C                 -----------
C
C definitions for temperature profile
 
      REAL ZTEMP(NLEVP1), ZPRES(NLEVP1), ZHEIG(NLEVP1)
 
C definitions for read in
 
c      REAL DUMMY, T(NLEV), H2O(NLEV), O3(NLEV)
c     *                ,ZTM1(NLON,NLEV)
       REAL DUMMY,ZTM1(NLON,NLEV),zzztm1(NLEV),zzzqm1(nlev),
     *  zzzo3(nlev)
 
C channel number
 
      INTEGER C3EQUIL, IBRUEHL
 
      CHARACTER*40 EQUILIB,IN_FILE
 
      PARAMETER (C3EQUIL=14)
 
C
C local arrays
C
C      DIMENSION ZCDIS(5)
      DIMENSION ZALP(66),ZFOZQ(11),ZFOZH(11),ZFAES(21),ZFAEL(21),
     #          ZFAEU(21),ZFAED(21), ZCDIS(5), ZCDEC(5), ZCEQT(5)
C
      INTEGER  ILAT, ILHT, ILST, IDIAL, IDIAS
c      INTEGER MMNTH(12), JDAY
c      DATA MMNTH/15,45,74,105,135,166,196,227,258,288,319,349/
c      REAL YTIME
C
      ITASK=3
C
C
C define all constants
C
      CALL INICON
C
C define the latitude points
C
C Set up latitude grid
 
      XLAT(1)=PI*alat/180.0
C calculate day
C
C  loop over latitudes
C
      DO 1111 ILAT=1,NLAT
C
C      WRITE(6,*) '***CALCULATE LATITUDE NUMBER ',ILAT,'***'
C
C Set up height grid (in terms of pressure)
C
      IF (NLP2 .NE. NLON) WRITE(6,*) ' problems with vertical velocity'
C      OPEN (11,FILE=IN_FILE,STATUS='OLD',FORM='FORMATTED')
C
C remark: Temp, vmrH2O, vmrO3 and vmrCO2 where changed to use
C            bruehl.dat (IBRUEHL=1)
C    important: see ZCARDI for changed vmrCO2, independent of IBRUEHL
C
      IBRUEHL=1
C
C      READ(11,*) TSUPAR
      DO 36 ILST=1,NLON
       DO 35 ILHT=1,NRLEV
          JKL = NRLEV + 1 - ILHT
          APM1(ILST,ILHT)=PR(JKL)
          ZZZTM1(ILHT)=T(JKL)
          ZZZQM1(ILHT)=H2O(JKL)
          ZZZO3(ILHT)=O3(JKL)
c          IF (JKL .NE. NRLEV) THEN
c             APHM1(ILST,JKL+1) = (APM1(ILST,JKL+1)+APM1(ILST,JKL))/2.
c          END IF
 35    CONTINUE
       DO 29 JKL=1,NRLEV 
          IF (JKL .NE. NRLEV) THEN
             APHM1(ILST,JKL+1) = (APM1(ILST,JKL+1)+APM1(ILST,JKL))/2.
          END IF
 29    CONTINUE

C
        TSUPAR = ZZZTM1(NRLEV)
        APHM1(ILST,1) = 0.
C        APHM1(ILST,NRLEVP1)=101325.
        APHM1(ILST,NRLEVP1)=APM1(ILST,NRLEV)
c        print *, 'surface press ',aphm1(ilst,nrlevp1)
C
C
C define moisture and vertical velocity  ==>  clouds
C
       DO 37 ILHT=1,NRLEV
c        print *, 'input ',ilht,apm1(ilst,ilht),zzztm1(ilht),
c     $   zzzqm1(ilht),zzzo3(ilht),aphm1(1,ilht)
        VERVEL(ILST,ILHT)=0.
        QM1(ILST,ILHT)=ZZZQM1(ILHT)
 37    CONTINUE
 
 36   CONTINUE
C
C      REWIND(11)
C      REWIND(12)
C
C define maximal relative humidity
C
      DO 38 JK=1,NLEV
         CCRH(JK) = 1.
 38   CONTINUE
C
      DO 33 J=1,NLEV
            TM1(1,J) = ZZZTM1(J)
 33   CONTINUE
C
      DO 39 JK=1,NLP2
        ALBM(JK) = SWALB
        SNM1M(JK) = SNPAR
        TSM1M(JK) = TSUPAR
        LOLAND(JK) = LOLPAR
        COSLON(JK) = 1.
        SINLON(JK) = 0.
 39   CONTINUE
      DATA ZALBICE/0.55/
      DATA ZALBSEA/0.07/
      DATA ZALBSNO/0.80/
      DATA ZSNOWAL/0.01/
C
C  remark:  ZCARDI is the CO2 concentration in vmr
C
C change co2 here!
      DATA ZCARDI/0.455739E-03/
ccc1      DATA ZCARDI/0.5318E-03/
C
      DATA ZEMISS/0.996/
      DATA ZSUPSAT/0.01/
      DATA ZCDIS/+1.000110,+0.034221,+0.001280,+0.000719,+0.000077/
      DATA CRAE/+0.1277E-02/
      DATA ZCDEC/+0.006918,-0.399912,+0.070257,-0.006758,+0.000907/
      DATA ZCEQT/+0.000075,+0.001868,-0.032077,-0.014615,-0.040849/
C
C*    PHYSICAL CONSTANTS.
C     -------- ----------
C
C          *ZQWSSAT* AND *ZQSNCR* ARE THE INVERSES OF CRITICAL VALUES
C     FOR SOIL WATER AND SNOW DEPTH (SEE *VDIFF*). IN THE CODE ALSO
C     APPEARS *ZCRH* FOR A MAXIMUM ALLOWED ATMOSPHERIC RELATIVE
C     HUMIDITY.
C
C      CQWSSAT = 1. / (0.75*0.02)
C      ZQWSSAT=CQWSSAT
C      ZQSNCR=CQSNCR
C
C*    SECURITY PARAMETER.
C     -------- ----------
C
C     *ZEPSEC* AVOIDS 0/0 IN THE DIAGNOSTIC OF TOTAL CLOUD COVER.
C     *ZEPCLC* IS A SECURITY TO AVOID ZERO OR ONE CLOUD COVERS AND
C     *ZEPH2O* IS A SECURITY TO AVOID WATER VAPOUR CONTENT IN A LAYER
C    *         TO BE MORE THEN THE RESPECTIVE VALUE AT SATURATION.
C     *ZEPALB* IS A SECURITY TO AVOID ZERO ALBEDOS.
C
      ZEPSEC=1.E-12
      ZEPCLC=1.E-12
      ZEPH2O=1.E-12
      ZEPALB=1.E-12
C
C Setup SW code
      SOLC=1376.0

      LDIUR=.FALSE.   !Diurnally averaged if false
C      LDIUR=.TRUE.   !Diurnally averaged if false
C
       YCLOCK=PI      !time of day in radians
C       YCLOCK= (DOY-FLOAT(INT(DOY))*2.*3.14159
c       print *,doy,int(doy)
C
      CALL SOLANG(LDIUR,DOY,YCLOCK,ALAT,ALON,AMU0,RDAYL(1),CDISSEM)
C

      ZSCT=SOLC*CDISSEM   ! Solar const * earth-sun distance
      PSOL=ZSCT*RDAYL(1)     ! * fractional day lengthC
C     OLD AEROSOL MODEL USED (NO DISTRIBUTION).
C
      CAEROS=0.1462E-16
C
C  ----------------------------------------------------------
C
C*         3.     FULL GRID COMPUTATIONS.
C                 ---- ---- -------------
C
  300 CONTINUE
C
C*         3.2     SATURATION SPECIFIC HUMIDITY AT ALL LAYERS.
C
  320 CONTINUE
      DO 322 JK=1,NLEV
      ZCRH=CCRH(JK)
      DO 321 JL=1,NLON
      LO=(TM1(JL,JK)-TMELT).GT.0.
      ZCVM3=CVMGT(C3LES,C3IES,LO)
      ZCVM4=CVMGT(C4LES,C4IES,LO)
      ZQS(JL,JK)=ZCRH*C2ES*EXP(ZCVM3*(TM1(JL,JK)-TMELT)/(TM1(JL,JK)
     *           -ZCVM4))/APM1(JL,JK)
      ZQS(JL,JK)=ZQS(JL,JK)/(1.-VTMPC1*ZQS(JL,JK))
  321 CONTINUE
  322 CONTINUE
c      print *,'2.2'
C
C*          3.3    EXTRAPOLATION AT THE TOP.
C
  330 CONTINUE
C      DO 331 JL=1,NLON
C      ZTI(JL,1)=TM1(JL,1)-APM1(JL,1)*(TM1(JL,1)-TM1(JL,2))
C     *                              /(APM1(JL,1)-APM1(JL,2))
  331 CONTINUE
c      print *,'3.3',nlev

C
C*         3.4     VERTICAL INTERPOLATION.
C
  340 CONTINUE
      DO 342 JK=2,NLEV
      DO 341 JL=1,NLON
c       print *,' vert ',jk,jl,aphm1(jl,jk)
      ZTI(JL,JK)=(TM1(JL,JK-1)*APM1(JL,JK-1)*(APM1(JL,JK)-APHM1(JL,JK))
     *           +TM1(JL,JK)*APM1(JL,JK)*(APHM1(JL,JK)-APM1(JL,JK-1)))
     *           *(1./(APHM1(JL,JK)*(APM1(JL,JK)-APM1(JL,JK-1))))
  341 CONTINUE
  342 CONTINUE
C
c      print *,'3.4'

C*         3.5     SURFACE VALUES AND TOP BOUNDARY TEMPERATURE.
C
  350 CONTINUE
      DO 351 JL=1,NLON
      ZTI(JL,NLEVP1)=TSM1M(JL)
      ZTI(JL,1)=TM1(JL,1)-APM1(JL,1)*(TM1(JL,1)-ZTI(JL,2))
     *                              /(APM1(JL,1)-APHM1(JL,2))
  351 CONTINUE
C
c      print *,'3.5'

C*         3.6     MOISTURE AND CLOUDS.
C
  360 CONTINUE
C   Prescribe cloud amout and height
      DO 361 JL=1,NLON
       ZCC(JL)=MIN(CF(1,1),1.0)
       ZCH(JL)=MIN(CF(2,1),1.0)
       ZCM(JL)=MIN(CF(3,1),1.0)
       ZCL(JL)=MIN(CF(4,1),1.0)
C Correction of the levels M.B. and PMF 27.06.01
       NTOP=NLEV+1-IC(1,2)
       NBASE=NLEV+1-IC(1,1)
       ITCC=NLEV+1-IC(1,1)
       ITH=NLEV+1-IC(2,1)
       ITM=NLEV+1-IC(3,1)
       ITL=NLEV+1-IC(4,1)
C***************************************
       DO JK=NRLEV,1,-1
        ZCLC(JL,JK)=0.0
        ZQLWP(JL,JK)=0.0
       ENDDO
c Piers diagnostic cloud 1-7-97
      IF (NBASE.GT.0) THEN
C      print *,'ITCC ',ZCC(JL),cf(1,1),NTOP,NBASE
c convective cloud 0.25*zcc everywhere but base
      DO ITCC=NTOP,NBASE-1
      ZCLC(JL,ITCC)=ZCC(JL)*0.25
      ZQLWP(JL,ITCC)=0.03*ZQS(JL,ITCC)*ZCLC(JL,ITCC)
      ENDDO
c 1*ZCC in convective cloud base
      ZCLC(JL,NBASE)=ZCC(JL)*0.25
      ZQLWP(JL,NBASE)=0.02*ZQS(JL,NBASE)*ZCLC(JL,NBASE)      
      ENDIF
C low,mid,high cloud add fractions
      ZCLC(JL,ITL)=ZCL(JL)+ZCLC(JL,ITL)
      ZCLC(JL,ITM)=ZCM(JL)+ZCLC(JL,ITM)
      ZCLC(JL,ITH)=ZCH(JL)+ZCLC(JL,ITH)
C Piers supersat kg/kg 2% * cloud fraction
      ZQLWP(JL,ITL)=0.02*ZQS(JL,ITL)*ZCLC(JL,ITL)
      ZQLWP(JL,ITM)=0.04*ZQS(JL,ITM)*ZCLC(JL,ITM)
      ZQLWP(JL,ITH)=0.1*ZQS(JL,ITH)*ZCLC(JL,ITH)
 361  CONTINUE
C
      DO 363 JK=IRLEV,1,-1
      DO 362 JL=1,NLON
      ZSW(JL,JK)=ZQS(JL,JK)
c      IF (ZCLC(JL,JK).GT.0.0) THEN
c       print '(a10,i5,2E13.3)','cloud ',jk,zclc(jl,jk),zqlwp(jl,jk)
c      ENDIF
      ZCLC(JL,JK)=MIN(MAX(ZCLC(JL,JK),ZEPCLC),1.-ZEPCLC)
      ZWV(JL,JK)=QM1(JL,JK)
c zqlwp now mmr not g/m-2
      LO=ZCLC(JL,JK).GT.ZEPCLC
C      ZQLWP(JL,JK)=CVMGT(ZQLWP(JL,JK)/ZCLC(JL,JK),0.0,LO)
C piers 10-6-97  as KPS changed ZQLWP i do too, LWP to g/m2
C piers 1-7-97 divided LWP by 4 to make max LWPs about 150 g/m-2
C complete fix i.e a super satuartion of about 0.2%
      ZQLWP(JL,JK)=CVMGT(ZQLWP(JL,JK),0.0,LO)/4.0
 362  CONTINUE
 363  CONTINUE
c         print *,' did clouds'
C
C*         3.7     INPUT DIAGNOSTICS FOR TEMPERATURE AND CLOUDS.
C
  370 CONTINUE
C
C*         3.8     ALBEDO.
C
  380 CONTINUE
C
      DO 381 JL=1,NLON
      LO1=TSM1M(JL)-CTFREEZ.GT.0.0
      ZALTE(JL)=1.0-ZEMISS
      ZALTE(JL)=MAX(ZALTE(JL),ZEPALB)
      ZALSO(JL)=ALBM(JL)
      LO=(.NOT.LOLAND(JL)).AND.(.NOT.LO1)
      ZALSO(JL)=CVMGT(ZALBICE,ZALSO(JL),LO)
      LO=(.NOT.LOLAND(JL)).AND.LO1
      ZALSO(JL)=CVMGT(ZALBSEA,ZALSO(JL),LO)
      ZALSO(JL)=CVMGT(ZALSO(JL)+(ZALBSNO-ZALSO(JL))*(SNM1M(JL)
     *         /(SNM1M(JL)+ZSNOWAL)),ZALSO(JL),LOLAND(JL))
      ZALSO(JL)=MAX(ZALSO(JL),ZEPALB)
  381 CONTINUE
C
C     -------------------------------------------------
C
c        print *,' 3.8'
C*         5.      SPLITTED LOOP OVER LATITUDE.
C*                 -------- ---- ---- ---------
C
  500 CONTINUE
C
      ISHIFT=0
      DO 551 JLOOP=1,NRPARTI
C remark: change ishift if you really want splitted loop
C
C
C*         5.1     INPUT: ALBEDO,ZENITH ANGLE, CO2, O3 AND AEROSOLS.
C
  510 CONTINUE
C
      DO 511 JL=1,IRLONI
      APRE(JL)=APHM1(JL+ISHIFT,NLEVP1)
c      print *, ' apre ',apre(jl),nlevp1
      SMU0(JL)=AMU0(JL+ISHIFT)
      ALSO(JL)=ZALSO(JL+ISHIFT)
      ALTE(JL)=ZALTE(JL+ISHIFT)
  511 CONTINUE
      DO 513 JK=1,IRLVP1
      DO 512 JL=1,IRLONI
      TI(JL,JK)=ZTI(JL+ISHIFT,JK)
  512 CONTINUE
  513 CONTINUE
      DO 514 JL=1,IRLONI
      ZDPO(JL)=APHM1(JL+ISHIFT,1)
      ZQCFO(JL)=ZCARDI*ZDPO(JL)
      ZAEQSO(JL)=CAEROS*APHM1(JL+ISHIFT,1)**3
c      ZQOFO(JL)=ZOZQ(JL+ISHIFT)*SQRT(ZDPO(JL)**3)/(SQRT(ZDPO(JL)**3)
c     *          +ZOZH(JL+ISHIFT))
  514 CONTINUE
      DO 517 JK=1,IRLEV
      DO 515 JL=1,IRLONI
      ZDPN(JL)=APHM1(JL+ISHIFT,JK+1)
      ZQCFN(JL)=ZCARDI*ZDPN(JL)
      ZAEQSN(JL)=CAEROS*APHM1(JL+ISHIFT,JK+1)**3
c      ZQOFN(JL)=ZOZQ(JL+ISHIFT)*SQRT(ZDPN(JL)**3)/(SQRT(ZDPN(JL)**3)
c     *          +ZOZH(JL+ISHIFT))
      DP(JL,JK)=ZDPN(JL)-ZDPO(JL)
      QCF(JL,JK)=ZQCFN(JL)-ZQCFO(JL)
      AEQ1(JL,JK)=ZAEQSN(JL)-ZAEQSO(JL)
      AADS(JL,JK)=0.001089*AEQ1(JL,JK)/DP(JL,JK)
c      QOF(JL,JK)=ZQOFN(JL)-ZQOFO(JL)
  515 CONTINUE
      DO 516 JL=1,IRLONI
      ZDPO(JL)=ZDPN(JL)
      ZQCFO(JL)=ZQCFN(JL)
c      ZQOFO(JL)=ZQOFN(JL)
      ZAEQSO(JL)=ZAEQSN(JL)
  516 CONTINUE
  517 CONTINUE
c       print *,'5.1'
C
C*         5.2     INPUT: MOISTURE AND CLOUDS.
C
  520 CONTINUE
C
      DO 522 JK=1,IRLEV
      DO 521 JL=1,IRLONI
      SSW(JL,JK)=ZSW(JL+ISHIFT,JK)
      WV(JL,JK)=ZWV(JL+ISHIFT,JK)
      CLC(JL,JK)=ZCLC(JL+ISHIFT,JK)
C      CLWA(JL,JK)=ZQLWP(JL+ISHIFT,JK)*ZCLC(JL+ISHIFT,JK)
C Piers LWP g/m2
      CLWA(JL,JK)=ZQLWP(JL+ISHIFT,JK)
  521 CONTINUE
  522 CONTINUE
c        print *,'5.2'
C
C*         5.3     CALL TO *RADIA*.
C
  530 CONTINUE
C
      IF (CAEROS.LT.1.E-14) THEN
         DO 532 JK = 1 , IRLEV
         DO 531 JL = 1 , IRLONI
         AEQ1(JL,JK) = CAEROS
         AEQ2(JL,JK) = CAEROS
         AEQ3(JL,JK) = CAEROS
         AEQ4(JL,JK) = CAEROS
         AEQ5(JL,JK) = CAEROS
 531     CONTINUE
 532     CONTINUE
      END IF
C
      KRLEV2 = 2 * NRLEV
      KNGL = 3 * NRLEV
      KNGLP1 = KNGL + 1
C
C  Changes in case of IBRUEHL=1
C
c      print *, 'got hre'
C  QOF is the mmr*DP
C
      IF (IBRUEHL .EQ. 1) THEN
         DO 533 JK=1,NLEV
c           WV(1,JK) = 0.
            WV(1,JK) = ZZZQM1(JK)
C            CLC(1,JK) = 0.
C            CLWA(1,JK) = 0.
C            QOF(1,JK) = 0.
C            QOF(1,JK) = O3(JK) * 48./28.96 * DP(1,JK)
             QOF(1,JK) = ZZZO3(JK)*DP(1,JK)
 533     CONTINUE
      END IF
C
      DO 538 JK=1,NLEV
        DO 538 JL=1,NLON
          ZTM1(JL,JK)=ZZZTM1(JK)
 538  CONTINUE
C
c      print *,' before call'
c      print *,ztm1,pierslwp
      CALL RADLSW (IRLONI, IRLST, IRLEV, IRLVP1, KRLEV2, KNGL, KNGLP1,
     S             STBO, zsct, ZCARDI, ITASK, ZTM1,PIERSLWP)
c      print *,' after call'
C
C
C*         5.4     STORAGE OF THE OUTPUT.
C
  540 CONTINUE
  550 CONTINUE
  551 CONTINUE
C***
C*         6.2    STORAGE OF TRANSMISSIVITY AND EMISSIVITY.
C
  620 CONTINUE
C
C
C*         6.3    OUTPUT DIAGNOSTICS FOR HEATING RATES AND FLUXES.
C
  630 CONTINUE
C
C write out damping rate at the various height levels
 
c         WRITE(C3EQUIL,*) '   '
c         IF (LDIUR) THEN
c           WRITE(C3EQUIL,188) JDAY, XLAT(ILAT),
c     #      YCLOCK*13750.987
 188       FORMAT(' jday  =', I4, 6X, ' external',
     #            ' latitude =', F8.2, 10X,
     #            ' time in seconds (GMT) =', F12.2)
c         ELSE
c            WRITE(C3EQUIL,1881) JDAY, XLAT(ILAT)
 1881       FORMAT(' jday  =', I4, 6X, ' external',
     #            ' latitude =', F8.2, 10X,
     #            ' diurnally averaged ')
c         END IF
 
c         WRITE(C3EQUIL,*) '   '
c         WRITE(C3EQUIL,*) '   '
c         DO 181 JK=1,NLEVP1
c            JKL = NLEVP1 + 1 - JK
c            WRITE(C3EQUIL,191) ZFLUX(1,1,JKL), -ZFLUX(1,2,JKL),
c     #               -FLT(1,JK),PPMB(1,JKL)*100., PTL(1,JKL),
c     #               ZFSDWN(1,JKL), ZFSUP(1,JKL), FLS(1,JK)
 191        FORMAT(8E15.4)
 181     CONTINUE
c         WRITE(C3EQUIL,192)
 192     FORMAT(6X,
     #     ' flxupLW ',6X,' flxdwLW ',6x,' netflxL ',6X,
     #     ' half pr ',6X,' half te ',6X,' flxdwSW ',6X,
     #     ' flxupSW ',6X,' netflxS ')
c         WRITE(C3EQUIL,*) '   '
c         WRITE(C3EQUIL,*) '   '
c         WRITE(C3EQUIL,*) '   '
c         DO 180 JL=1,NLON
c           DO 180 JK=1,NLEV
c            JKL = NLEV + 1 - JK
c            PKASCH = -ZCOOLR(JL,JKL) + ZHEATR(JL,JKL)
cccm            DUMMY = PQOF(JL,JK)/DP(1,JK)*28.96/48.
c            DUMMY = PQOF(JL,JK)/DP(1,JK)
c            WRITE(C3EQUIL,190) JL, ILAT, JK,
c     >             ZHEATR(JL,JKL), WV(JL,JK),
c     >             DUMMY, PCLDLW(JL,JK),
c     >             -ZCOOLR(JL,JKL), PKASCH, APM1(JL,JK),
c     >             TM1(JL,JK)
c            WRITE(8,*) -ZCOOLR(JL,JKL),APM1(JL,JK)
c            WRITE(9,*)  ZHEATR(JL,JKL),APM1(JL,JK)
c 180     CONTINUE
 190     FORMAT(I3,1x,I4,1x,I4,1x,8E14.4)
c         WRITE(C3EQUIL,189)
 189     FORMAT('Long  Lati  H ',4X,
     #     'solar h ',6X,'moistur ',6X,' O3conc ',6X,
     #     'cldfrac ',6x,' netlwh ',6X,' nethrt ',6X,
     #     'pressur ',6X,'tempera ')
c         WRITE(C3EQUIL,*) '   '
C
C  write out temperature and ozone column amounts
C
C      OPEN (77,FILE='MIDTO3B6.BIN',FORM='UNFORMATTED',STATUS='NEW')
C      DO 997 JL=1,NLON
C       ZCOLUM = 0.
C       DO 998 JK=1,NLEV
CC         ZCOLUM=ZCOLUM+PQOF(JL,JK)*48./29./G*
CC     #          (APHM1(JL,JK+1)-APHM1(JL,JK))
C         ZCOLUM=ZCOLUM+PQOF(JL,JK)/G*100.*8314.3*273.16/101325./48.
C         WRITE(77) ZCOLUM, APHM1(JL,JK), TM1(JL,JK)
CC         WRITE(6,*) 'JL,JK,ZCOLUM,APHM1:',JL,JK,ZCOLUM,APHM1(JL,JK)
C 998   CONTINUE
C 997  CONTINUE
C      CLOSE (77)
C
C  end loop over latitudes
C
 1111 CONTINUE
C
      DO JKL=1,NLEV
        HTLW(JKL)=-ZCOOLR(1,JKL)
        HTSW(JKL)=ZHEATR(1,JKL)
c        print *,'zmorc ',jkl,htsw(jkl),htlw(jkl)
      ENDDO
c level 1 surface nrlevp1=top
Crfluxes
C     1st index - flux 1=SW, 2=LW
C     2nd index - Direction 1=DN, 2=UP
C     3rd index - Where 1=TOP, 2=SURFACE
      rfluxes(1,1,1)=zfsdwn(1,nrlevp1)
      rfluxes(1,1,2)=zfsdwn(1,1)
      rfluxes(1,2,1)=zfsup(1,nrlevp1)
      rfluxes(1,2,2)=zfsup(1,1)
      rfluxes(2,1,1)=-zflux(1,2,nrlevp1)
      rfluxes(2,1,2)=-zflux(1,2,1)
      rfluxes(2,2,1)=zflux(1,1,nrlevp1)
      rfluxes(2,2,2)=zflux(1,1,1)

ccc1      CLOSE (C2TEMP)
C
C     ---------------------------------------------------
      END
C
*DECK DEDD 
      SUBROUTINE DEDD ( KDLON, KRLST, KFLEV, ITASK )
C
C**** *DEDD* - COMPUTES REFLECTIVITY, TRANSMISSIVITY OF A CLOUDY LAYER
C
C     PURPOSE.
C     --------
C           COMPUTES THE REFLECTIVITY AND TRANSMISSIVITY OF A CLOUDY
C     LAYER USING THE DELTA-EDDINGTON'S APPROXIMATION.
C
C**   INTERFACE.
C     ----------
C          *DEDD* IS CALLED BY *SW*.
C
C
C        IMPLICIT ARGUMENTS
C        --------------------
C
C     ==== INPUTS ===
C PGG    : (NDLON)             ; ASSYMETRY FACTOR
C PREF   : (NDLON)             ; REFLECTIVITY OF THE UNDERLYING LAYER
C PRMUZ  : (NDLON)             ; COSINE OF SOLAR ZENITH ANGLE
C PTO1   : (NDLON)             ; OPTICAL THICKNESS
C PW     : (NDLON)             ; SINGLE SCATTERING ALBEDO
C     ==== OUTPUTS ===
C PRE1   : (NDLON)             ; LAYER REFLECTIVITY ASSUMING NO
C                              ; REFLECTION FROM UNDERLYING LAYER
C PTR1   : (NDLON)             ; LAYER TRANSMISSIVITY ASSUMING NO
C                              ; REFLECTION FROM UNDERLYING LAYER
C PRE2   : (NDLON)             ; LAYER REFLECTIVITY ASSUMING
C                              ; REFLECTION FROM UNDERLYING LAYER
C PTR2   : (NDLON)             ; LAYER TRANSMISSIVITY ASSUMING
C                              ; REFLECTION FROM UNDERLYING LAYER
C
C     METHOD.
C     -------
C
C          STANDARD DELTA-EDDINGTON LAYER CALCULATIONS.
C
C     EXTERNALS.
C     ----------
C
C          NONE
C
C     REFERENCE.
C     ----------
C
C        SEE RADIATION'S PART OF THE MODEL'S DOCUMENTATION AND
C        ECMWF RESEARCH DEPARTMENT DOCUMENTATION
C
C     AUTHOR.
C     -------
C        JEAN-JACQUES MORCRETTE  *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C        ORIGINAL : 88-12-15
C     -----------------------------------------------------------
C
*CALL ZPARB
c      INCLUDE 'zparb.upd'
C
C     -------------------------------------------------------------
C
      DO 99 JL=1,NRLST
         PGG  (JL) = ZGG  (JL)
         PREF (JL) = ZREF (JL)
         PRE1 (JL) = ZRE1 (JL)
         PRE2 (JL) = ZRE2 (JL)
         PRMUZ(JL) = ZRMUZ(JL)
         PTO1 (JL) = ZTO1 (JL)
         PTR1 (JL) = ZTR1 (JL)
         PTR2 (JL) = ZTR2 (JL)
         PW   (JL) = ZW   (JL)
 99   CONTINUE
C
C*         1.      DELTA-EDDINGTON CALCULATIONS
C
 100  CONTINUE
C
      DO 131 JL   =   1 , KDLON
C
C*         1.1     SET UP THE DELTA-MODIFIED PARAMETERS
C
 110  CONTINUE
C
         ZFF = PGG(JL)*PGG(JL)
         ZGP = PGG(JL)/(1.+PGG(JL))
         ZTOP = (1.- PW(JL) * ZFF) * PTO1(JL)
         ZWCP = (1-ZFF)* PW(JL) /(1.- PW(JL) * ZFF)
         ZDT = 2./3.
         ZX1 = 1.-ZWCP*ZGP
         ZWM = 1.-ZWCP
         ZRM2 =  PRMUZ(JL) * PRMUZ(JL)
         ZZRK = SQRT(3.*ZWM*ZX1)
         ZX2 = 4.*(1.-ZZRK*ZZRK*ZRM2)
         ZRP = SQRT(3.*ZWM/ZX1)
         ZALPHA = 3.*ZWCP*ZRM2*(1.+ZGP*ZWM)/ZX2
         ZBETA = 3.*ZWCP* PRMUZ(JL) *(1.+3.*ZGP*ZRM2*ZWM)/ZX2
         ZEXMUO = EXP(-ZTOP/ PRMUZ(JL) )
         ZEXKP = EXP(ZZRK*ZTOP)
         ZEXKM = 1./ZEXKP
         ZXP2P = 1.+ZDT*ZRP
         ZXM2P = 1.-ZDT*ZRP
         ZAP2B = ZALPHA+ZDT*ZBETA
         ZAM2B = ZALPHA-ZDT*ZBETA
C
C*         1.2     WITHOUT REFLECTION FROM THE UNDERLYING LAYER
C
 120  CONTINUE
C
         ZA11 = ZXP2P
         ZA12 = ZXM2P
         ZA13 = ZAP2B
         ZA22 = ZXP2P*ZEXKP
         ZA21 = ZXM2P*ZEXKM
         ZA23 = ZAM2B*ZEXMUO
         ZDENA = ZA11 * ZA22 - ZA21 * ZA12
         ZC1A = (ZA22*ZA13-ZA12*ZA23)/ZDENA
         ZC2A = (ZA11*ZA23-ZA21*ZA13)/ZDENA
         ZRI0A = ZC1A+ZC2A-ZALPHA
         ZRI1A = ZRP*(ZC1A-ZC2A)-ZBETA
         PRE1(JL) = (ZRI0A-ZDT*ZRI1A)/ PRMUZ(JL)
         ZRI0B = ZC1A*ZEXKM+ZC2A*ZEXKP-ZALPHA*ZEXMUO
         ZRI1B = ZRP*(ZC1A*ZEXKM-ZC2A*ZEXKP)-ZBETA*ZEXMUO
         PTR1(JL) = ZEXMUO+(ZRI0B+ZDT*ZRI1B)/ PRMUZ(JL)
C
C*         1.3     WITH REFLECTION FROM THE UNDERLYING LAYER
C
 130  CONTINUE
C
         ZB21 = ZA21- PREF(JL) *ZXP2P*ZEXKM
         ZB22 = ZA22- PREF(JL) *ZXM2P*ZEXKP
         ZB23 = ZA23- PREF(JL) *ZEXMUO*(ZAP2B - PRMUZ(JL) )
         ZDENB = ZA11 * ZB22 - ZB21 * ZA12
         ZC1B = (ZB22*ZA13-ZA12*ZB23)/ZDENB
         ZC2B = (ZA11*ZB23-ZB21*ZA13)/ZDENB
         ZRI0C = ZC1B+ZC2B-ZALPHA
         ZRI1C = ZRP*(ZC1B-ZC2B)-ZBETA
         PRE2(JL) = (ZRI0C-ZDT*ZRI1C) / PRMUZ(JL)
         ZRI0D = ZC1B*ZEXKM + ZC2B*ZEXKP - ZALPHA*ZEXMUO
         ZRI1D = ZRP * (ZC1B*ZEXKM - ZC2B*ZEXKP) - ZBETA*ZEXMUO
         PTR2(JL) = ZEXMUO + (ZRI0D + ZDT*ZRI1D) / PRMUZ(JL)
C
 131  CONTINUE
      DO 132 JL=1,NRLST
         ZGG  (JL) = PGG  (JL)
         ZREF (JL) = PREF (JL)
         ZRE1 (JL) = PRE1 (JL)
         ZRE2 (JL) = PRE2 (JL)
         ZRMUZ(JL) = PRMUZ(JL)
         ZTO1 (JL) = PTO1 (JL)
         ZTR1 (JL) = PTR1 (JL)
         ZTR2 (JL) = PTR2 (JL)
         ZW   (JL) = PW   (JL)
 132     CONTINUE
      RETURN
      END
C
*DECK LW
      SUBROUTINE LW (KDLON,KRLST,KFLEV,KFLVP1,KFLEV2,KNGL,
     S                      KNGLP1, PSIG,PSCT,PCCO2,ITASK )
C
C**** *LW*   - COMPUTES LONGWAVE RADIATION QUANTITIES
C
C     PURPOSE.
C     --------
C           COMPUTES LONGWAVE FLUXES
C
C**   INTERFACE.
C     ----------
C
C        EXPLICIT ARGUMENTS
C        --------------------
C
C        IMPLICIT ARGUMENTS
C        --------------------
C
C     ==== INPUTS ===
C PEMIS  : (KDLON)             ; SURFACE EMISSIVITY
C PCCO2  :                     ; CONCENTRATION IN CO2 (PA/PA)
C PQOF   : (KDLON,KFLEV)       ; CONCENTRATION IN OZONE (PA/PA)
C PTAVE  : (KDLON,KFLEV)       ; TEMPERATURE
C PTL    : (KDLON,0:KFLEV)     ; HALF LEVEL TEMPERATURE
C PPMB   : (KDLON,0:KFLEV)     ; HALF LEVEL PRESSURE
C PWV    : (KDLSUR,KFLEV)      ; SPECIFIC HUMIDITY PA/PA
C PCLDLW : (KDLON,KFLEV)       ; CLOUD FRACTIONAL COVER
C PAER   : (KDLON,KFLEV,5)     ; OPTICAL THICKNESS OF THE AEROSOLS
C     ==== OUTPUTS ===
C PFLUX(KDLON,2,KFLEV)         ; RADIATIVE FLUXES
C                     1  ==>  UPWARD   FLUX TOTAL
C                     2  ==>  DOWNWARD FLUX TOTAL
C
C     METHOD.
C     -------
C
C          1. COMPUTES THE PRESSURE AND TEMPERATURE WEIGHTED AMOUNTS OF
C     ABSORBERS, AND THE VARIOUS RELEVANT PLANCK FUNCTIONS
C          2. PERFORMS THE VERTICAL INTEGRATION DISTINGUISHING THE CON-
C     TRIBUTIONS OF THE ADJACENT AND DISTANT LAYERS AND THOSE FROM THE
C     BOUNDARIES.
C          3. COMPUTES THE CLEAR-SKY DOWNWARD AND UPWARD EMISSIVITIES.
C          4. INTRODUCES THE EFFECTS OF THE CLOUDS ON THE FLUXES.
C
C     EXTERNALS.
C     ----------
C
C          *LWTT*
C
C     REFERENCE.
C     ----------
C
C        SEE RADIATION'S PART OF THE MODEL'S DOCUMENTATION
C
C     AUTHOR.
C     -------
C        JEAN-JACQUES MORCRETTE  *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C        ORIGINAL : 88-12-15
C------------------------------------------------------------------
C      IMPLICIT LOGICAL (L)
C
*CALL ZPARB 
c      INCLUDE 'zparb.upd'

C__________________________________________________________________
C
      REAL ZPHM6(NLON),ZPSM6(NLON),ZPHN6(NLON),
     *     ZPSN6(NLON)              
      REAL ZEPSH7(NLON),ZEPSH8(NLON),ZEPSH9(NLON)
      REAL ZFACTC1(NLON),ZFACTC2(NLON),
     *     ZFACTC3(NLON), ZFACTC4(NLON),ZFACTC5(NLON),
     *     ZFN10(NLON)
      INTEGER IXTP(NLON),IXTB(NLON),IXTS(NLON)
      REAL PGA(NLON,12,2,NLEV),PGB(NLON,12,2,NLEV),
     *     PPTY(NLON,12,NLEV),PETY(NLON,12,NLEV),
     *     PGATOP(NLON,12,2), PGBTOP(NLON,12,2),
     *     PGASUR(NLON,12,2), PGBSUR(NLON,12,2),
     *     PTYTOP(NLON,12),    ETYTOP(NLON,12),
     *     PTYSUR(NLON,12),    ETYSUR(NLON,12)
C
C  *******************************************************************   
C*  Concentration of the various trace gases (IPCC/SACC values for 1990) 
C        CO2         CH4        N2O        CFC11       CFC12             
C      353ppmv     1.76ppmv   350ppbv     280pptv     484pptv            
C
      ZAIRMWG = 28.970 
      ZCO2MWG = 44.011 
      ZCH4MWG = 16.043 
      ZN2OMWG = 44.013 
      ZC11MWG = 137.3686
      ZC12MWG = 120.9140
C                       
      RCH4    = 1.76E-06*ZCH4MWG/ZAIRMWG 
      RN2O    = 350.E-09*ZN2OMWG/ZAIRMWG 
      RCFC11  = 280.E-12*ZC11MWG/ZAIRMWG 
      RCFC12  = 484.E-12*ZC12MWG/ZAIRMWG 
C
      DO 97 JL=1,NRLST
         DO 98 JK=1,NRLEV
C            PCLD  (JL,JK) = PCLFR (JL,JK)
C            PQLW  (JL,JK) = PQLWP (JL,JK)
            PCLDLW(JL,JK) = ZCLDLW(JL,JK)
            DO 99 II=1,2
               PFLUX (JL,II,JK) = ZFLUX (JL,II,JK)
               PTAU  (JL,II,JK) = ZTAU  (JL,II,JK)
 99         CONTINUE
            PPMB  (JL,JK) = ZPMB  (JL,JK)
            PTAVE (JL,JK) = ZTAVE (JL,JK)
            PTL   (JL,JK) = ZTL   (JL,JK)
 98      CONTINUE
         DO 96 II=1,2
            PFLUX (JL,II,NRLEV+1) = ZFLUX (JL,II,NRLEV+1)
 96      CONTINUE
         PDT0(JL) = ZDT0(JL)
         PPMB(JL,NRLEV+1) = ZPMB(JL,NRLEV+1)
         PTL (JL,NRLEV+1) = ZTL (JL,NRLEV+1)
 97   CONTINUE
      ZCCO2 = PCCO2
C
C*         1.     COMPUTES ABSORBER AMOUNTS
C                 -------------------------
C
C*         1.0    INITIALIZATION
C                 --------------
C
 100  CONTINUE

      IMAXC = 0

! changing - marc 30/3/10
c      DO 101 JL = 1 , (KFLEV+1)*KDLON
c        ZADJD(JL,1) = 0.
c        ZADJU(JL,1) = 0.
c        ZBINT(JL,1) = 0.
c        ZDISTD(JL,1)= 0.
c        ZDISTU(JL,1)= 0.
 101  CONTINUE
      ZADJD(:,:)=0.0
      ZADJU(:,:)=0.0
      ZBINT(:,:)=0.0
      ZDISTD(:,:)=0.0
      ZDISTU(:,:)=0.0
!
      DO 114 JK=1,KFLEV                                                   
        DO 113 JL = 1 , KDLON
          PCTS(JL,JK)=0.
 113    CONTINUE
 114  CONTINUE
      DO 103 JL = 1 , KDLON
        IXTP(JL)    = 1
        ZBSUIN(JL) = 0.
        IMX(JL)    = 0
        IMXP(JL)   = 0
 103  CONTINUE
C
C*         1.1    SEARCH THE LAYER INDEX OF THE HIGHEST CLOUD
C                 -------------------------------------------
C
 110  CONTINUE

      DO 112 JK = 1 , KFLEV
        DO 111 JL = 1 , KDLON
          IF (PCLDLW(JL,JK).GT.ZEPSC) THEN
            IMXP(JL)=JK
           ELSE
            IMXP(JL)=IMX(JL)
          ENDIF
          IMAXC=MAX(IMXP(JL),IMAXC)
          IMX(JL)=IMXP(JL)
 111    CONTINUE
 112  CONTINUE
C
C
C*         1.2    PRESSURE OVER GAUSS SUB-LEVELS
C                 ------------------------------
C
 120  CONTINUE

      DO 121 JL = 1 , KDLON
        ZSSIG(JL, 1 ) = 1.0
 121  CONTINUE

      DO 126 JK = 1 , KFLEV
        JKJ=(JK-1)*NG1P1+1
        JKJR = JKJ
        JKJP = JKJ + NG1P1
        DO 123 JL = 1 , KDLON
          ZSSIG(JL,JKJP)=PPMB(JL,JK+1)/PPMB(JL,1)
 123    CONTINUE
        DO 125 IG1=1,NG1
          JKJ=JKJ+1
          DO 124 JL = 1 , KDLON
            ZSSIG(JL,JKJ)= (ZSSIG(JL,JKJR)+ZSSIG(JL,JKJP))*0.5
     *     + RT1(IG1) * (ZSSIG(JL,JKJP) - ZSSIG(JL,JKJR)) * 0.5
 124      CONTINUE
 125    CONTINUE
 126  CONTINUE
C
C
C*         1.3    PRESSURE-WEIGHTED TEMPERATURE AND TRANSMISSIVITY INDEX
C                 ------------------------------------------------------
C
 130  CONTINUE

      DO 131 JK = 1 , KFLEV
        DO 132 JL = 1 , KDLON
          ZDST1 = (PTAVE(JL,JK)-TINTP(1)) / TSTP
          IXTX = MAX( 1, MIN( MXIXT, INT( ZDST1 + 1. ) ) )
          ZDSTX = (PTAVE(JL,JK)-TINTP(IXTX))/TSTP
          IF (ZDSTX.LT.0.5) THEN
            IXTP(JL) = IXTX
           ELSE
            IXTP(JL) = IXTX+1
          ENDIF
 132    CONTINUE
        DO 133 JG=1,12
          DO 134 JL = 1 , KDLON
            INDT=IXTP(JL)
            PGA(JL,JG,1,JK)=GA(INDT,2*JG,1)
            PGA(JL,JG,2,JK)=GA(INDT,2*JG,2)
            PGB(JL,JG,1,JK)=GB(INDT,2*JG,1)
            PGB(JL,JG,2,JK)=GB(INDT,2*JG,2)
 134      CONTINUE
 133    CONTINUE
        DO 1331 JG=1,10
          DO 1341 JL = 1 , KDLON
            INDT=IXTP(JL)
            PPTY(JL,JG,JK)=PTY(INDT,2*JG)
            PETY(JL,JG,JK)=ETY(INDT,2*JG)
 1341     CONTINUE
 1331   CONTINUE
 131  CONTINUE

      DO 135 JL=1,KDLON
        ZDST1 = (PTL(JL,NLEV+1)-TINTP(1)) / TSTP
        IXTX = MAX( 1, MIN( MXIXT, INT( ZDST1 + 1. ) ) )
        ZDSTX = (PTL(JL,KFLEV+1)-TINTP(IXTX))/TSTP
        IF (ZDSTX.LT.0.5) THEN
          IXTB(JL)=IXTX
        ELSE
          IXTB(JL)=IXTX+1
        ENDIF
        ZDST1 = (PTL(JL,1)-TINTP(1)) / TSTP
        IXTX = MAX( 1, MIN( MXIXT, INT( ZDST1 + 1. ) ) )
        ZDSTX = (PTL(JL,1)-TINTP(IXTX))/TSTP
        IF (ZDSTX.LT.0.5) THEN
          IXTS(JL)=IXTX
        ELSE
          IXTS(JL)=IXTX+1
        END IF
 135  CONTINUE     

      DO 136 JG=1, 12
        DO 137 JL=1,KDLON
          INDSU=IXTS(JL)
          PGASUR(JL,JG,1)=GA(INDSU,2*JG-1,1)
          PGBSUR(JL,JG,1)=GB(INDSU,2*JG-1,1)
          PGASUR(JL,JG,2)=GA(INDSU,2*JG-1,2)
          PGBSUR(JL,JG,2)=GB(INDSU,2*JG-1,2)
          INDTP=IXTB(JL)
          PGATOP(JL,JG,1)=GA(INDTP,2*JG-1,1)
          PGBTOP(JL,JG,1)=GB(INDTP,2*JG-1,1)
          PGATOP(JL,JG,2)=GA(INDTP,2*JG-1,2)
          PGBTOP(JL,JG,2)=GB(INDTP,2*JG-1,2)
 137    CONTINUE
 136  CONTINUE
    
      DO 139 JG=1, 10
        DO 1391 JL=1,KDLON
          INDSU=IXTS(JL)
          PTYSUR(JL,JG)=PTY(INDSU,2*JG-1)
          ETYSUR(JL,JG)=ETY(INDSU,2*JG-1)
          INDTP=IXTB(JL)
          PTYTOP(JL,JG)=PTY(INDTP,2*JG-1)
          ETYTOP(JL,JG)=ETY(INDTP,2*JG-1)
 1391   CONTINUE
 139  CONTINUE    
C
C*         1.4    PRESSURE THICKNESS AND MEAN PRESSURE OF SUB-LAYERS
C                 --------------------------------------------------
C
 140  CONTINUE

      DO 142 JKI=1,3*KFLEV
        JKIP1=JKI+1
        DO 141 JL = 1 , KDLON
          ZABSLY(JL,5,JKI)=(ZSSIG(JL,JKI)+ZSSIG(JL,JKIP1))*0.5
          ZABSLY(JL,3,JKI)=(ZSSIG(JL,JKI)-ZSSIG(JL,JKIP1))
     *                     *PPSOL(JL)/(10.*G)
 141    CONTINUE
 142  CONTINUE

      DO 146 JK = 1 , KFLEV
        JKP1=JK+1
        JKL = KFLEV+1 - JK
        DO 143 JL = 1 , KDLON
          ZXWV(JL) = MAX(PWV(JL,JKL) , ZEPSCQ )
          ZXOZ(JL) = MAX(PQOF(JL,JKL) / PDP(JL,JKL) , ZEPSCO )
 143    CONTINUE
        JKJ=(JK-1)*NG1P1+1
        JKJPN=JKJ+NG1

        DO 145 JKK=JKJ,JKJPN
          DO 144 JL = 1 , KDLON
            ZDPM = ZABSLY(JL,3,JKK)
            ZUPM = ZABSLY(JL,5,JKK) * PPSOL(JL) * ZDPM / 101325.
            ZDUC(JL,JKK)=ZDPM

            ZABSLY(JL,12,JKK)=ZXOZ(JL)*ZDPM*208.333
            ZABSLY(JL,8,JKK) =PCCO2   *ZDPM*227.2159
            ZABSLY(JL,13,JKK)=ZXOZ(JL)*ZUPM*208.333 *1013.25
            ZABSLY(JL,7,JKK) =PCCO2   *ZUPM*227.2159*1013.25
            ZABSLY(JL,32,JKK)=ZXOZ(JL)*ZDPM*208.333 *PTAVE(JL,JK)
            ZABSLY(JL,33,JKK)=PCCO2   *ZDPM*227.2159*PTAVE(JL,JK)

            ZU6=ZXWV(JL) * ZUPM
            ZFPPW= 1.6078 *ZXWV(JL)/(1.+0.608*ZXWV(JL))
            ZABSLY(JL,6,JKK)=ZU6
     #            * (1. + DELTAH/ZABSLY(JL,5,JKK))
            ZABSLY(JL,11,JKK)=ZU6*ZFPPW
            ZABSLY(JL,10,JKK)=ZU6*(1.-ZFPPW)
            ZABSLY(JL,9,JKK) = PCCO2 * ZUPM
     #            * (1. + DELTAC/ZABSLY(JL,5,JKK))
 144      CONTINUE
 145    CONTINUE
 146  CONTINUE
C
C*         1.5    CUMULATIVE ABSORBER AMOUNTS FROM TOP OF ATMOSPHERE
C                 --------------------------------------------------
C
 150  CONTINUE

! changing - marc 30/3/10
c      DO 151 JL = 1 , NUA*KDLON
c        ZABSCU(JL,1,3*KFLEV+1)=0.
c 151  CONTINUE
      ZABSCU(:,1,3*KFLEV+1)=0.

      DO 159 JK = 1 , KFLEV
        JJ=(JK-1)*NG1P1+1
        JJPN=JJ+NG1
        JKL=KFLEV+1-JK
C
C*         1.5.1  CUMULATIVE AEROSOL AMOUNTS FROM TOP OF ATMOSPHERE
C                 --------------------------------------------------
C
 1510   CONTINUE

        JAE1=3*KFLEV+1-JJ
        JAE2=3*KFLEV+1-(JJ+1)
        JAE3=3*KFLEV+1-JJPN
        DO 1512 IAE=1,5
          DO 1511 JL = 1 , KDLON
            ZUAER(JL,IAE) = (CAER(IAE,1)*PAER(JL,JKL,1)
     *      +CAER(IAE,2)*PAER(JL,JKL,2)+CAER(IAE,3)*PAER(JL,JKL,3)
     *      +CAER(IAE,4)*PAER(JL,JKL,4)+CAER(IAE,5)*PAER(JL,JKL,5))
     *      /(ZDUC(JL,JAE1)+ZDUC(JL,JAE2)+ZDUC(JL,JAE3))
 1511     CONTINUE
 1512   CONTINUE
C
C*         1.5.2  INTRODUCES TEMPERATURE EFFECTS ON ABSORBER AMOUNTS
C
 1520   CONTINUE

        DO 1521 JL = 1 , KDLON
          ZTAVIC(JL)=PTAVE(JL,JKL)
C
C- TEMPERATURE DEPENDENCE FOR WATER VAPOR CONTINUUM (FROM CKD MODEL)
C
          ZFACTC(JL)  = 296./ZTAVIC(JL)
          ZZZZ         = (1.0-ZTAVIC(JL)/296.)
          ZFACTC1(JL) = EXP(3.3338*ZZZZ)
          ZFACTC2(JL) = EXP(5.2775*ZZZZ)
          ZFACTC3(JL) = EXP(5.9022*ZZZZ)
          ZFACTC4(JL) = EXP(6.2938*ZZZZ)
          ZFACTC5(JL) = EXP(7.0252*ZZZZ)

          ZTX=ZTAVIC(JL)-TREF
          ZTX2=ZTX*ZTX
          ZZABSLY = ZABSLY(JL,6,JAE1)
     *             +ZABSLY(JL,6,JAE2)
     *             +ZABSLY(JL,6,JAE3)
          ZUP=MIN(MAX( 0.5*X10E*LOG( ZZABSLY ) +5., 0.), 6.0)
          ZCAH1=AT(1,1)+ZUP*(AT(1,2)+ZUP*(AT(1,3)))
          ZCBH1=BT(1,1)+ZUP*(BT(1,2)+ZUP*(BT(1,3)))
          ZEPSH1(JL)=EXP( ZCAH1 * ZTX + ZCBH1 * ZTX2 )
          ZCAH2=AT(2,1)+ZUP*(AT(2,2)+ZUP*(AT(2,3)))
          ZCBH2=BT(2,1)+ZUP*(BT(2,2)+ZUP*(BT(2,3)))
          ZEPSH2(JL)=EXP( ZCAH2 * ZTX + ZCBH2 * ZTX2 )
          ZCAH3=AT(3,1)+ZUP*(AT(3,2)+ZUP*(AT(3,3)))
          ZCBH3=BT(3,1)+ZUP*(BT(3,2)+ZUP*(BT(3,3)))
          ZEPSH3(JL)=EXP( ZCAH3 * ZTX + ZCBH3 * ZTX2 )
          ZCAH4=AT(4,1)+ZUP*(AT(4,2)+ZUP*(AT(4,3)))
          ZCBH4=BT(4,1)+ZUP*(BT(4,2)+ZUP*(BT(4,3)))
          ZEPSH4(JL)=EXP( ZCAH4 * ZTX + ZCBH4 * ZTX2 )
          ZCAH5=AT(5,1)+ZUP*(AT(5,2)+ZUP*(AT(5,3)))
          ZCBH5=BT(5,1)+ZUP*(BT(5,2)+ZUP*(BT(5,3)))
          ZEPSH5(JL)=EXP( ZCAH5 * ZTX + ZCBH5 * ZTX2 )
          ZCAH6=AT(6,1)+ZUP*(AT(6,2)+ZUP*(AT(6,3)))
          ZCBH6=BT(6,1)+ZUP*(BT(6,2)+ZUP*(BT(6,3)))
          ZEPSH6(JL)=EXP( ZCAH6 * ZTX + ZCBH6 * ZTX2 )
C
C*--1250-1450 CM-1 H2O
C
          ZCAH7=AT(7,1)+ZUP*(AT(7,2)+ZUP*(AT(7,3)))
          ZCBH7=BT(7,1)+ZUP*(BT(7,2)+ZUP*(BT(7,3)))
          ZEPSH7(JL)=EXP( ZCAH7 * ZTX + ZCBH7 * ZTX2 )
C
C*--800-970 CM-1 H2O
C
          ZCAH8=AT(8,1)+ZUP*(AT(8,2)+ZUP*(AT(8,3)))
          ZCBH8=BT(8,1)+ZUP*(BT(8,2)+ZUP*(BT(8,3)))
          ZEPSH8(JL)=EXP( ZCAH8 * ZTX + ZCBH8 * ZTX2 )
C
C*--650-800 CM-1 H2O
C
          ZCAH9=AT(9,1)+ZUP*(AT(9,2)+ZUP*(AT(9,3)))
          ZCBH9=BT(9,1)+ZUP*(BT(9,2)+ZUP*(BT(9,3)))
          ZEPSH9(JL)=EXP( ZCAH9 * ZTX + ZCBH9 * ZTX2 )

          ZPHM6(JL)=EXP(-5.81E-4 * ZTX - 1.13E-6 * ZTX2)
          ZPSM6(JL)=EXP(-5.57E-4 * ZTX - 3.30E-6 * ZTX2)
          ZPHN6(JL)=EXP(-3.46E-5 * ZTX + 2.05E-6 * ZTX2)
          ZPSN6(JL)=EXP( 3.70E-3 * ZTX - 2.30E-6 * ZTX2)
 1521   CONTINUE

        DO 1522 JL = 1 , KDLON
          ZTAVIC(JL)=PTAVE(JL,JKL)
          ZTX=ZTAVIC(JL)-TREF
          ZTX2=ZTX*ZTX
          ZZABSLY= ZABSLY(JL,9,JAE1)
     *            +ZABSLY(JL,9,JAE2)
     *            +ZABSLY(JL,9,JAE3)
          ZALUP = X10E * LOG( ZZABSLY )
          ZUP   = MAX( 0.00 , 5.00 + 0.50 * ZALUP )
          ZEPSC2(JL) = (ZTAVIC(JL)/TREF) ** ZUP
          ZCAC8=AT(11,1)+ZUP*(AT(11,2)+ZUP*(AT(11,3)))
          ZCBC8=BT(11,1)+ZUP*(BT(11,2)+ZUP*(BT(11,3)))
          ZEPSC3(JL) = EXP( ZCAC8  * ZTX + ZCBC8  * ZTX2 )
          ZEPHIO(JL) = EXP( OCT(1) * ZTX + OCT(2) * ZTX2)
C          ZEPSIO(JL) = EXP( 2.* (OCT(3) * ZTX+OCT(4)*ZTX2))
 1522   CONTINUE

        DO 1524 JKK=JJ,JJPN
          JC=3*KFLEV+1-JKK
          JCP1=JC+1
          DO 1523 JL = 1 , KDLON
            ZABSCU(JL,10,JC)=ZABSCU(JL,10,JCP1)                  
     *                 +ZABSLY(JL,10,JC)  * ZFACTC(JL)
            ZABSCU(JL,25,JC)=ZABSCU(JL,25,JCP1)                  
     *                 +ZABSLY(JL,11,JC)  * ZFACTC2(JL)
            ZABSCU(JL,26,JC)=ZABSCU(JL,26,JCP1)                  
     *                 +ZABSLY(JL,11,JC)  * ZFACTC3(JL)
            ZABSCU(JL,27,JC)=ZABSCU(JL,27,JCP1)                  
     *                 +ZABSLY(JL,11,JC)  * ZFACTC4(JL)
            ZABSCU(JL,28,JC)=ZABSCU(JL,28,JCP1)                  
     *                 +ZABSLY(JL,11,JC)  * ZFACTC5(JL)
            ZABSCU(JL,11,JC)=ZABSCU(JL,11,JCP1)    
     *                 +ZABSLY(JL,11,JC)  * ZFACTC1(JL)
c
            ZABSCU(JL,12,JC)=ZABSCU(JL,12,JCP1)+ZABSLY(JL,12,JC)
            ZABSCU(JL,13,JC)=ZABSCU(JL,13,JCP1)+ZABSLY(JL,13,JC)
            ZABSCU(JL,32,JC)=ZABSCU(JL,32,JCP1)+ZABSLY(JL,32,JC)
            ZABSCU(JL, 7,JC)=ZABSCU(JL, 7,JCP1)+ZABSLY(JL, 7,JC)
            ZABSCU(JL,34,JC)=ZABSCU(JL,34,JCP1)+ZABSLY(JL, 8,JC)
            ZABSCU(JL,33,JC)=ZABSCU(JL,33,JCP1)+ZABSLY(JL,33,JC)
c 
            ZABSCU(JL,1,JC)=ZABSCU(JL,1,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH1(JL)
            ZABSCU(JL,2,JC)=ZABSCU(JL,2,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH2(JL)
            ZABSCU(JL,3,JC)=ZABSCU(JL,3,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH5(JL)       
            ZABSCU(JL,4,JC)=ZABSCU(JL,4,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH3(JL)
            ZABSCU(JL,5,JC)=ZABSCU(JL,5,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH4(JL)
 
            ZABSCU(JL,29,JC)=ZABSCU(JL,29,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH7(JL)
            ZABSCU(JL,30,JC)=ZABSCU(JL,30,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH8(JL)
            ZABSCU(JL,31,JC)=ZABSCU(JL,31,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH9(JL)

            ZABSCU(JL,6,JC)=ZABSCU(JL,6,JCP1)                    
     *                +ZABSLY(JL,6,JC)  * ZEPSH6(JL)       
 
            ZABSCU(JL,14,JC)=ZABSCU(JL,14,JCP1)                  
     *                +ZUAER(JL,1)    * ZDUC(JL,JC)*DIFF   
            ZABSCU(JL,15,JC)=ZABSCU(JL,15,JCP1)                  
     *                +ZUAER(JL,2)    * ZDUC(JL,JC)*DIFF   
            ZABSCU(JL,16,JC)=ZABSCU(JL,16,JCP1)                  
     *                +ZUAER(JL,3)    * ZDUC(JL,JC)*DIFF   
            ZABSCU(JL,17,JC)=ZABSCU(JL,17,JCP1)                  
     *                +ZUAER(JL,4)    * ZDUC(JL,JC)*DIFF   
            ZABSCU(JL,18,JC)=ZABSCU(JL,18,JCP1)                  
     *                +ZUAER(JL,5)    * ZDUC(JL,JC)*DIFF   
 
           ZABSCU(JL,19,JC)=ZABSCU(JL,19,JCP1)
     *         +ZABSLY(JL,8,JC)*RCH4/PCCO2/227.2159*ZPHM6(JL)*DIFF
            ZABSCU(JL,20,JC)=ZABSCU(JL,20,JCP1)                       
     *               +ZABSLY(JL,9,JC)*RCH4/PCCO2*ZPSM6(JL)*DIFF 
            ZABSCU(JL,21,JC)=ZABSCU(JL,21,JCP1)                       
     *         +ZABSLY(JL,8,JC)*RN2O/PCCO2/227.2159*ZPHN6(JL)*DIFF
            ZABSCU(JL,22,JC)=ZABSCU(JL,22,JCP1)                       
     *               +ZABSLY(JL,9,JC)*RN2O/PCCO2*ZPSN6(JL)*DIFF 
 
            ZABSCU(JL,23,JC)=ZABSCU(JL,23,JCP1)                       
     *               +ZABSLY(JL,8,JC)*RCFC11/PCCO2/227.2159  *DIFF
           ZABSCU(JL,24,JC)=ZABSCU(JL,24,JCP1)                       
     *               +ZABSLY(JL,8,JC)*RCFC12/PCCO2/227.2159  *DIFF

            ZABSCU(JL,8,JC)=ZABSCU(JL,8,JCP1)
     *               +ZABSLY(JL,9,JC)*ZEPSC3(JL)*DIFF
            ZABSCU(JL,9,JC)=ZABSCU(JL,9,JCP1)
     *               +ZABSLY(JL,9,JC)*ZEPSC3(JL)*DIFF
 1523     CONTINUE
 1524   CONTINUE
 159  CONTINUE
C
C*         1.6     PLANCK FUNCTIONS AND GRADIENTS
C                  ------------------------------
C
 160  CONTINUE

      DO 168 JNU=1,NINT
C
C*         1.6.1   LEVELS FROM SURFACE TO KFLEV
C                  ----------------------------
C
 1610   CONTINUE

        DO 1612 JK = 1 , KFLEV
          DO 1611 JL = 1 , KDLON
            ZTI(JL,JK)=(PTL(JL,JK)-TSTAND)/TSTAND
            ZRES(JL) = XP(1,JNU)+ZTI(JL,JK)*(XP(2,JNU)+ZTI(JL,JK)
     *      *(XP(3,JNU)
     *     +ZTI(JL,JK)*(XP(4,JNU)+ZTI(JL,JK)*(XP(5,JNU)+ZTI(JL,JK)
     *      *(XP(6,JNU)
     *       )))))
            ZBINT(JL,JK)=ZBINT(JL,JK)+ZRES(JL)
            PB(JL,JNU,JK)= ZRES(JL)
            ZBLEV(JL,JK) = ZRES(JL)
            ZTI2(JL)=(PTAVE(JL,JK)-TSTAND)/TSTAND
            ZRES2(JL)=XP(1,JNU)+ZTI2(JL)*(XP(2,JNU)+ZTI2(JL)*(XP(3,JNU)
     *     +ZTI2(JL)*(XP(4,JNU)+ZTI2(JL)*(XP(5,JNU)+ZTI2(JL)*(XP(6,JNU)
     *       )))))
            ZBLAY(JL,JK) = ZRES2(JL)
 1611     CONTINUE
 1612   CONTINUE
C
C*         1.6.2   TOP OF THE ATMOSPHERE AND SURFACE
C                  ---------------------------------
C
 1620   CONTINUE

        DO 1621 JL = 1 , KDLON
          ZTI(JL,JK)=(PTL(JL,KFLEV+1)-TSTAND)/TSTAND
          ZTI2(JL) = (PTL(JL,1) + PDT0(JL) - TSTAND) / TSTAND
          ZRES(JL) = XP(1,JNU)+ZTI(JL,JK)*(XP(2,JNU)+ZTI(JL,JK)
     *     *(XP(3,JNU)
     *     +ZTI(JL,JK)*(XP(4,JNU)+ZTI(JL,JK)*(XP(5,JNU)+ZTI(JL,JK)
     *     *(XP(6,JNU)
     *        )))))
          ZRES2(JL) = XP(1,JNU)+ZTI2(JL)*(XP(2,JNU)+ZTI2(JL)*(XP(3,JNU)
     *     +ZTI2(JL)*(XP(4,JNU)+ZTI2(JL)*(XP(5,JNU)+ZTI2(JL)*(XP(6,JNU)
     *        )))))
          ZBINT(JL,KFLEV+1) = ZBINT(JL,KFLEV+1)+ZRES(JL)
          PB(JL,JNU,KFLEV+1)= ZRES(JL)
          ZBLEV(JL,KFLEV+1) = ZRES(JL)
          ZBTOP(JL,JNU) = ZRES(JL)
          ZBSUR(JL,JNU) = ZRES2(JL)
          ZBSUIN(JL) = ZBSUIN(JL) + ZRES2(JL)
 1621   CONTINUE
C
C*         1.6.3   GRADIENTS IN SUB-LAYERS
C                  -----------------------
C
 1630   CONTINUE

        DO 1632 JK = 1 , KFLEV
          JK2 = 2 * JK
          JK1 = JK2 - 1
          DO 1631 JL = 1 , KDLON
            ZDBSL(JL,JNU,JK1) = ZBLAY(JL,JK  ) - ZBLEV(JL,JK)
            ZDBSL(JL,JNU,JK2) = ZBLEV(JL,JK+1) - ZBLAY(JL,JK)
 1631     CONTINUE
 1632   CONTINUE
 168  CONTINUE
C
C*         1.7     INITIALIZE TRANSMISSION FUNCTIONS
C                  ---------------------------------
C
 170  CONTINUE

! changing - marc 30/3/10
c      DO 171 JL = 1 , NTRA*KDLON
c        ZTT (JL,1) = 1.0
c        ZTT1(JL,1) = 1.0
c        ZTT2(JL,1) = 1.0
c 171  CONTINUE
      ZTT (:,:) = 1.0
      ZTT1(:,:) = 1.0
      ZTT2(:,:) = 1.0
C
C     --------------------------------------------------------
C
C*         2.      VERTICAL INTEGRATION
C                  --------------------
C
 200  CONTINUE
C
C*         2.1     CONTRIBUTION FROM ADJACENT LAYERS
C                  ---------------------------------
C
 210  CONTINUE

      DO 215 JK = 1 , KFLEV
C
C*         2.1.1   DOWNWARD LAYERS
C                  ---------------
C
 2110   CONTINUE

        KM12 = 2 * (JK - 1)
        KND = (JK - 1) * NG1P1 + 1
        KNU = JK * NG1P1 + 1

        DO 2111 JL = 1 , KDLON
          ZGLAYD(JL) = 0.
          ZGLAYU(JL) = 0.
 2111   CONTINUE

        DO 213 IG = 1 , NG1
          KBS = KM12 + IG
          KDD = KND + IG
! changing - marc 30/3/10
c          DO 2112 JL = 1 , NUAER*KDLON
c            ZUU(JL,1) = ZABSCU(JL,1,KND) - ZABSCU(JL,1,KDD)
c 2112     CONTINUE
          DO I=1,NUAER
             DO JL=1,KDLON
                ZUU(JL,I) = ZABSCU(JL,I,KND) - ZABSCU(JL,I,KDD)
             ENDDO
          ENDDO
C          
          CALL LWTT0(KDLON,KRLST,KFLEV,ITASK,
     *               PGA(1,1,1,JK),PGB(1,1,1,JK),
     *               PPTY(1,1,JK), PETY(1,1,JK))

          DO 2114 JL = 1 , KDLON
            ZWTR=ZDBSL(JL,1,KBS)*ZTT(JL,1)          *ZTT(JL,10)
     *          +ZDBSL(JL,2,KBS)*ZTT(JL,2)*ZTT(JL,7)*ZTT(JL,11)  
     *          +ZDBSL(JL,3,KBS)*ZTT(JL,4)*ZTT(JL,8)*ZTT(JL,12)  
     *          +ZDBSL(JL,4,KBS)*ZTT(JL,5)*ZTT(JL,9)*ZTT(JL,13)
     *          +ZDBSL(JL,5,KBS)*ZTT(JL,3)          *ZTT(JL,14)
     *          +ZDBSL(JL,6,KBS)*ZTT(JL,6)          *ZTT(JL,15)
            ZGLAYD(JL)=ZGLAYD(JL)+ZWTR*WG1(IG)
 2114     CONTINUE
C
C*         2.1.2   DOWNWARD LAYERS
C                  ---------------
C
 2120     CONTINUE

! changing - marc 30/3/10
c          DO 2121 JL = 1 , NUAER*KDLON
c            ZUU(JL,1) = ZABSCU(JL,1,KDD) - ZABSCU(JL,1,KNU)
c 2121     CONTINUE
         DO I=1,NUAER
            DO JL=1,KDLON
               ZUU(JL,I) = ZABSCU(JL,I,KDD) - ZABSCU(JL,I,KNU)
            ENDDO
         ENDDO

          CALL LWTT0(KDLON,KRLST,KFLEV,ITASK,
     *               PGA(1,1,1,JK),PGB(1,1,1,JK),
     *               PPTY(1,1,JK), PETY(1,1,JK))

          DO 2123 JL = 1 , KDLON
            ZWTR=ZDBSL(JL,1,KBS)*ZTT(JL,1)          *ZTT(JL,10)
     *          +ZDBSL(JL,2,KBS)*ZTT(JL,2)*ZTT(JL,7)*ZTT(JL,11)
     *          +ZDBSL(JL,3,KBS)*ZTT(JL,4)*ZTT(JL,8)*ZTT(JL,12)
     *          +ZDBSL(JL,4,KBS)*ZTT(JL,5)*ZTT(JL,9)*ZTT(JL,13)
     *          +ZDBSL(JL,5,KBS)*ZTT(JL,3)          *ZTT(JL,14)
     *          +ZDBSL(JL,6,KBS)*ZTT(JL,6)          *ZTT(JL,15)
            ZGLAYU(JL)=ZGLAYU(JL)+ZWTR*WG1(IG)
 2123     CONTINUE
 213    CONTINUE

        DO 214 JL = 1 , KDLON
          ZADJD(JL,JK) = ZGLAYD(JL)
          ZCNTRB(JL,JK,JK+1  ) = ZGLAYD(JL)
          ZADJU(JL,JK+1) = ZGLAYU(JL)
          ZCNTRB(JL,JK+1,JK  ) = ZGLAYU(JL)
          ZCNTRB(JL,JK  ,JK  ) = 0.0
 214    CONTINUE
 215  CONTINUE
C
C
      DO 218 JK = 1 , KFLEV
        JK2 = 2 * JK
        JK1 = JK2 - 1
! changing - marc 8/3/10
!        DO 216 JL = 1 , NINT*KDLON
!          ZDBDT(JL,1,JK) = ZDBSL(JL,1,JK1) + ZDBSL(JL,1,JK2)
! 216    CONTINUE
        DO I=1,NINT
           DO JL=1,KDLON
               ZDBDT(JL,I,JK) = ZDBSL(JL,I,JK1) + ZDBSL(JL,I,JK2)
            ENDDO
         ENDDO

 218  CONTINUE
C
C*         2.2     CONTRIBUTION FROM DISTANT LAYERS
C                  ---------------------------------
C
 220  CONTINUE
C
C*         2.2.1   DISTANT AND ABOVE LAYERS
C                  ------------------------
C
 2210 CONTINUE
C
C*         2.2.2   FIRST UPPER LEVEL
C                  -----------------
C
 2220 CONTINUE

      DO 225 JK = 1 , KFLEV-1
        JKP1=JK+1
        KN = (JK-1)*NG1P1+1
        KD1= KN+NG1P1

! changing - marc 30/3/10
c        DO 2222 JL = 1 , NUAER*KDLON
c          ZUU(JL,1) = ZABSCU(JL,1,KN) - ZABSCU(JL,1,KD1)
c 2222   CONTINUE
          DO I=1,NUAER
             DO JL=1,KDLON
               ZUU(JL,I) = ZABSCU(JL,I,KN) - ZABSCU(JL,I,KD1)
             ENDDO
          ENDDO

        CALL LWTT0(KDLON,KRLST,KFLEV,ITASK,
     *             PGA(1,1,1,JK),PGB(1,1,1,JK),
     *             PPTY(1,1,JK), PETY(1,1,JK))

! changing - marc 30/3/10
c        DO 2224 JL = 1 , NTRAER*KDLON
c          ZTT1(JL,1)=ZTT(JL,1)
c 2224   CONTINUE
        DO I=1,NTRAER
           DO JL=1,KDLON
              ZTT1(JL,I)=ZTT(JL,I)
           ENDDO
        ENDDO
C
C*         2.2.3   HIGHER UP
C                  ---------
C
 2230   CONTINUE

        DO 224 JKJ=JKP1,KFLEV
          KJP1=JKJ+1
          KD2= JKJ  *NG1P1+1

! changing - marc 30/3/10
c          DO 2232 JL = 1 , NUAER*KDLON
c            ZUU(JL,1) = ZABSCU(JL,1,KN) - ZABSCU(JL,1,KD2)
c 2232     CONTINUE
          DO I=1,NUAER
             DO JL=1,KDLON
                ZUU(JL,I) = ZABSCU(JL,I,KN) - ZABSCU(JL,I,KD2)
             ENDDO
          ENDDO

          CALL LWTT0(KDLON,KRLST,KFLEV,ITASK,
     *               PGA(1,1,1,JKJ),PGB(1,1,1,JKJ),
     *               PPTY(1,1,JKJ), PETY(1,1,JKJ))
! changing - marc 30/3/10
c          DO 2234 JL = 1 , NTRAER*KDLON
c            ZTT2(JL,1)=(ZTT1(JL,1)+ZTT(JL,1))*0.5
c            ZTT1(JL,1)=ZTT(JL,1)
c 2234     CONTINUE
          DO I=1,NTRAER
             DO JL=1,KDLON
                ZTT2(JL,I)=(ZTT1(JL,I)+ZTT(JL,I))*0.5
                ZTT1(JL,I)=ZTT(JL,I)
             ENDDO
          ENDDO

          DO 2236 JL = 1 , KDLON
            ZWW=ZDBDT(JL,1,JKJ)*ZTT2(JL,1)           *ZTT2(JL,10)
     *        + ZDBDT(JL,2,JKJ)*ZTT2(JL,2)*ZTT2(JL,7)*ZTT2(JL,11)
     *        + ZDBDT(JL,3,JKJ)*ZTT2(JL,4)*ZTT2(JL,8)*ZTT2(JL,12)
     *        + ZDBDT(JL,4,JKJ)*ZTT2(JL,5)*ZTT2(JL,9)*ZTT2(JL,13)
     *        + ZDBDT(JL,5,JKJ)*ZTT2(JL,3)           *ZTT2(JL,14)
     *        + ZDBDT(JL,6,JKJ)*ZTT2(JL,6)           *ZTT2(JL,15)
            ZGLAYD(JL)=ZWW
            ZDZXDG=ZGLAYD(JL)
            ZDISTD(JL,JK)=ZDISTD(JL,JK)+ZDZXDG
            ZCNTRB(JL,JK,KJP1)=ZDZXDG
 2236     CONTINUE
 224    CONTINUE
 225  CONTINUE
C
C*         2.2.4   DISTANT AND BELOW LAYERS
C                  ------------------------
C
 2240 CONTINUE
C
C*         2.2.5   FIRST LOWER LEVEL
C                  -----------------
C
 2250 CONTINUE

      DO 228 JK=3,KFLEV+1
        KM1= JK-1
        KJ=JK-2
        KN = KM1*NG1P1+1
        KU1= KN-NG1P1

! changing - marc 30/3/10
c        DO 2252 JL = 1 , NUAER*KDLON
c          ZUU(JL,1) = ZABSCU(JL,1,KU1) - ZABSCU(JL,1,KN)
c 2252   CONTINUE
        DO I=1,NUAER
           DO JL=1,KDLON
              ZUU(JL,I) = ZABSCU(JL,I,KU1) - ZABSCU(JL,I,KN)
           ENDDO
        ENDDO

        CALL LWTT0(KDLON,KRLST,KFLEV,ITASK,
     *             PGA(1,1,1,KJ),PGB(1,1,1,KJ),
     *             PPTY(1,1,KJ), PETY(1,1,KJ))


! changing - marc 30/3/10
c        DO 2254 JL = 1 , NTRAER*KDLON
c          ZTT1(JL,1)=ZTT(JL,1)
c 2254   CONTINUE
        DO I=1,NTRAER
           DO JL=1,KDLON
              ZTT1(JL,I)=ZTT(JL,I)
           ENDDO
        ENDDO
C
C
C*         2.2.6   DOWN BELOW
C                  ----------
C
 2260   CONTINUE

        DO 227 JLK=1,JK-2
          JKL=KM1-JLK
          KU2=(JKL-1)*NG1P1+1

! changing - marc 30/3/10
c          DO 2262 JL = 1 , NUAER*KDLON
c            ZUU(JL,1) = ZABSCU(JL,1,KU2) - ZABSCU(JL,1,KN)
c 2262     CONTINUE
        DO I=1,NUAER
           DO JL=1,KDLON
              ZUU(JL,I) = ZABSCU(JL,I,KU2) - ZABSCU(JL,I,KN)
           ENDDO
        ENDDO

          CALL LWTT0(KDLON,KRLST,KFLEV,ITASK,
     *               PGA(1,1,1,JKL),PGB(1,1,1,JKL),
     *               PPTY(1,1,JKL), PETY(1,1,JKL))

! changing - marc 30/3/10
c          DO 2264 JL = 1 , NTRAER*KDLON
c            ZTT2(JL,1)=(ZTT1(JL,1)+ZTT(JL,1))*0.5
c            ZTT1(JL,1)=ZTT(JL,1)
c 2264     CONTINUE
          DO I=1,NTRAER
             DO JL=1,KDLON
                ZTT2(JL,I)=(ZTT1(JL,I)+ZTT(JL,I))*0.5
                ZTT1(JL,I)=ZTT(JL,I)
             ENDDO
          ENDDO

          DO 2266 JL = 1 , KDLON
            ZWW=ZDBDT(JL,1,JKL)*ZTT2(JL,1)           *ZTT2(JL,10)
     *        + ZDBDT(JL,2,JKL)*ZTT2(JL,2)*ZTT2(JL,7)*ZTT2(JL,11)
     *        + ZDBDT(JL,3,JKL)*ZTT2(JL,4)*ZTT2(JL,8)*ZTT2(JL,12)
     *        + ZDBDT(JL,4,JKL)*ZTT2(JL,5)*ZTT2(JL,9)*ZTT2(JL,13)
     *        + ZDBDT(JL,5,JKL)*ZTT2(JL,3)           *ZTT2(JL,14)
     *        + ZDBDT(JL,6,JKL)*ZTT2(JL,6)           *ZTT2(JL,15)
            ZGLAYU(JL)=ZWW
            ZDZXMG=ZGLAYU(JL)
            ZDISTU(JL,JK)=ZDISTU(JL,JK)+ZDZXMG
            ZCNTRB(JL,JK,JKL)=ZDZXMG
 2266     CONTINUE
 227    CONTINUE
 228  CONTINUE
C
C*         2.3     EXCHANGE WITH TOP OF THE ATMOSPHERE
C                  -----------------------------------
C
 230  CONTINUE

      DO 235 JK = 1 , KFLEV
        KN=(JK-1)*NG1P1+1

! changing - marc 30/3/10
c        DO 231 JL = 1 , NUAER*KDLON
c          ZUU(JL,1) = ZABSCU(JL,1,KN)
c 231    CONTINUE
        DO I=1,NUAER
           DO JL=1,KDLON
              ZUU(JL,I) = ZABSCU(JL,I,KN)
           ENDDO
        ENDDO

        CALL LWTT1(KDLON,KRLST,KFLEV,ITASK,PGATOP,PGBTOP,
     *             PTYTOP,ETYTOP)

        DO 234 JL = 1 , KDLON
          ZCNTOP=ZBTOP(JL,1)*ZTT(JL,1)          *ZTT(JL,10)
     *         + ZBTOP(JL,2)*ZTT(JL,2)*ZTT(JL,7)*ZTT(JL,11)
     *         + ZBTOP(JL,3)*ZTT(JL,4)*ZTT(JL,8)*ZTT(JL,12)
     *         + ZBTOP(JL,4)*ZTT(JL,5)*ZTT(JL,9)*ZTT(JL,13)
     *         + ZBTOP(JL,5)*ZTT(JL,3)          *ZTT(JL,14)
     *         + ZBTOP(JL,6)*ZTT(JL,6)          *ZTT(JL,15)
          ZFD(JL,JK)=ZCNTOP-ZBINT(JL,JK)-ZDISTD(JL,JK)-ZADJD(JL,JK)
          ZFDN(JL,JK)=ZFD(JL,JK)
          PFLUC(JL,2,JK)=ZFD(JL,JK)
 234    CONTINUE
 235  CONTINUE

      JK = KFLEV+1  
                      
      DO 236 JL = 1 , KDLON 
        ZCNTOP= ZBTOP(JL,1) 
     *        + ZBTOP(JL,2)      
     *        + ZBTOP(JL,3)      
     *        + ZBTOP(JL,4)      
     *        + ZBTOP(JL,5)      
     *        + ZBTOP(JL,6)      
        ZFD(JL,JK)=ZCNTOP-ZBINT(JL,JK)-ZDISTD(JL,JK)-ZADJD(JL,JK) 
        ZFDN(JL,JK)=ZFD(JL,JK)   
        PFLUC(JL,2,JK)=ZFD(JL,JK)
 236  CONTINUE                
C
C
C*         2.4     COOLING-TO-SPACE OF LAYERS ABOVE 0.0001 HPA
C                  ---------------------------------------
C
 240  CONTINUE
C
C
C*         2.4.1   INITIALIZATION
C                  --------------
C
 2410 CONTINUE
C
      JLIM = KFLEV
C
      DO 2412 JK = KFLEV,1,-1
      IF(PPMB(1,JK).LT.0.0001) THEN
         JLIM=JK
      ENDIF   
 2412 CONTINUE
      KLIM=JLIM
C
! changing - marc 30/3/10
c      DO 2414 JL = 1 , NTRAER*KDLON
c        ZTT1(JL,1)=1.0
c 2414 CONTINUE
      DO I=1,NTRAER
         DO JL=1,KDLON
            ZTT1(JL,I)=1.0
         ENDDO
      ENDDO
C
C*         2.4.2   LOOP OVER LAYERS ABOVE 0.0001 HPA
C                  -----------------------------
C
 2420 CONTINUE
C
      DO 2427 JSTRA = KFLEV,JLIM,-1
      JSTRU=(JSTRA-1)*NG1P1+1
C
! changing - marc 30/3/10
c      DO 2422 JL = 1 , NUAER*KDLON
c        ZUU(JL,1) = ZABSCU(JL,1,JSTRU)
c 2422 CONTINUE
        DO I=1,NUAER
           DO JL=1,KDLON
              ZUU(JL,I) = ZABSCU(JL,I,JSTRU)
           ENDDO
        ENDDO
C
        CALL LWTT1(KDLON,KRLST,KFLEV,ITASK,PGATOP,PGBTOP,
     *             PTYTOP,ETYTOP)
c
      DO 2424 JL = 1, KDLON
      ZCTSTR =
     1 (PB(JL,1,JSTRA)+PB(JL,1,JSTRA+1))
     1     *(ZTT1(JL,1)           *ZTT1(JL,10)
     1     - ZTT (JL,1)           *ZTT (JL,10))
     2+(PB(JL,2,JSTRA)+PB(JL,2,JSTRA+1))
     2     *(ZTT1(JL,2)*ZTT1(JL,7)*ZTT1(JL,11)
     2     - ZTT (JL,2)*ZTT (JL,7)*ZTT (JL,11))
     3+(PB(JL,3,JSTRA)+PB(JL,3,JSTRA+1))
     3     *(ZTT1(JL,4)*ZTT1(JL,8)*ZTT1(JL,12)
     3     - ZTT (JL,4)*ZTT (JL,8)*ZTT (JL,12))
     4+(PB(JL,4,JSTRA)+PB(JL,4,JSTRA+1))
     4     *(ZTT1(JL,5)*ZTT1(JL,9)*ZTT1(JL,13)
     4     - ZTT (JL,5)*ZTT (JL,9)*ZTT (JL,13))
     5+(PB(JL,5,JSTRA)+PB(JL,5,JSTRA+1))
     5     *(ZTT1(JL,3)           *ZTT1(JL,14)
     5     - ZTT (JL,3)           *ZTT (JL,14))
     6+(PB(JL,6,JSTRA)+PB(JL,6,JSTRA+1))
     6     *(ZTT1(JL,6)           *ZTT1(JL,15)
     6     - ZTT (JL,6)           *ZTT (JL,15))
      PCTS(JL,JSTRA)=ZCTSTR*0.5
 2424 CONTINUE
! changing - marc 30/3/10
c      DO 2426 JL = 1 , NTRAER*KDLON
c        ZTT1(JL,1)=ZTT(JL,1)
c 2426 CONTINUE
      DO I=1,NTRAER
         DO JL=1,KDLON
            ZTT1(JL,I)=ZTT(JL,I)
         ENDDO
      ENDDO

 2427 CONTINUE
C
C*         2.5     EXCHANGE WITH LOWER LIMIT
C                  -------------------------
C
 250  CONTINUE

      DO 251 JL = 1 , KDLON
        ZBGND(JL)=ZBSUIN(JL)*PEMIS(JL)-(1.-PEMIS(JL))
     *           *PFLUC(JL,2,1)-ZBINT(JL,1)
 251  CONTINUE

! JK wasn't set before, I'm guessing what is needed - marc 30/3/10
      DO JK = 2 , KFLEV+1
!
      DO 252 JL = 1 , KDLON
        ZCNSOL = ZBSUR(JL,1)
     *          +ZBSUR(JL,2)
     *          +ZBSUR(JL,3)
     *          +ZBSUR(JL,4)
     *          +ZBSUR(JL,5)
     *          +ZBSUR(JL,6)
        ZCNSOL=ZCNSOL*ZBGND(JL)/ZBSUIN(JL)
        ZFU(JL,JK)=ZCNSOL+ZBINT(JL,1)-ZDISTU(JL,1)-ZADJU(JL,1)
        ZFUP(JL,1)=ZFU(JL,JK)
        PFLUC(JL,1,1)=ZFU(JL,JK)
 252  CONTINUE
      enddo

      DO 255 JK = 2 , KFLEV+1
        KN=(JK-1)*NG1P1+1

! changing -marc
c        DO 2521 JL = 1 , NUAER*KDLON
c          ZUU(JL,1) = ZABSCU(JL,1,1) - ZABSCU(JL,1,KN)
c2521    CONTINUE
        DO I=1,NUAER
           DO JL=1,KDLON
              ZUU(JL,I) = ZABSCU(JL,I,1) - ZABSCU(JL,I,KN)
           ENDDO
        ENDDO

        CALL LWTT1(KDLON,KRLST,KFLEV,ITASK,PGASUR,PGBSUR,
     *             PTYSUR,ETYSUR)

        DO 254 JL = 1 , KDLON
          ZCNSOL=ZBSUR(JL,1)*ZTT(JL,1)          *ZTT(JL,10)
     *          +ZBSUR(JL,2)*ZTT(JL,2)*ZTT(JL,7)*ZTT(JL,11)
     *          +ZBSUR(JL,3)*ZTT(JL,4)*ZTT(JL,8)*ZTT(JL,12)
     *          +ZBSUR(JL,4)*ZTT(JL,5)*ZTT(JL,9)*ZTT(JL,13)
     *          +ZBSUR(JL,5)*ZTT(JL,3)          *ZTT(JL,14)
     *          +ZBSUR(JL,6)*ZTT(JL,6)          *ZTT(JL,15)
          ZCNSOL  =ZCNSOL*ZBGND(JL)/ZBSUIN(JL)
          ZFU(JL,JK)=ZCNSOL+ZBINT(JL,JK)-ZDISTU(JL,JK)-ZADJU(JL,JK)
          ZFUP(JL,JK)=ZFU(JL,JK)
          PFLUC(JL,1,JK)=ZFU(JL,JK)
          ZCNTRB(JL,JK,JK) = 0.0
 254    CONTINUE
 255  CONTINUE
C
C*         2.6     HEATING-FROM-GROUND OF UPPERMOST LAYER
C                  --------------------------------------
C
 260  CONTINUE
C
C*         2.7     CLEAR-SKY FLUXES
C                  ----------------
C
 270  CONTINUE
C
      DO 271 JL = 1 , KDLON
      ZFN10(JL) = ZFUP(JL,JLIM) + ZFDN(JL,JLIM)
 271  CONTINUE
      DO 273 JK = JLIM+1,KFLEV+1
      DO 272 JL = 1 , KDLON
      ZFN10(JL) = ZFN10(JL) + PCTS(JL,JK-1)
      ZFUP(JL,JK) = ZFN10(JL)
      ZFDN(JL,JK) = 0.
 272  CONTINUE
 273  CONTINUE
      DO 275 JK = 1 , KFLEV+1
      DO 274 JL = 1 , KDLON
      PFLUC(JL,1,JK) = ZFUP(JL,JK)
      PFLUC(JL,2,JK) = ZFDN(JL,JK)
 274  CONTINUE
 275  CONTINUE
c      IF (IMP.LT.4) THEN
c        WRITE(NOUT,884) (PFLUX(1,1,JK),JK = 1 , KFLEV+1)
c        WRITE(NOUT,884) (PFLUX(1,2,JK),JK = 1 , KFLEV+1)
c      END IF
C
C     --------------------------------------------------------------
C
C*         3.      EFFECTIVE DOWNWARD AND UPWARD CLEAR-SKY EMISSIVITIES
C                  ----------------------------------------------------
C
 300  CONTINUE

      DO 302 JKL = 1 , KFLEV
        JK = KFLEV+1 - JKL
        DO 301 JL = 1 , KDLON
          ZDFNET = PFLUC(JL,1,JK+1) + PFLUC(JL,2,JK+1)
     *            -PFLUC(JL,1,JK  ) - PFLUC(JL,2,JK  )
          ZEMD(JL,JK)= (-PFLUC(JL,2,JK)+PFLUC(JL,2,JK+1))
     *             /(PSIG*PTAVE(JL,JK)**4.+PFLUC(JL,2,JK+1))
          ZEMU(JL,JKL)=(PFLUC(JL,1,JKL+1)-PFLUC(JL,1,JKL))
     *             /(PSIG*PTAVE(JL,JKL)**4.-PFLUC(JL,1,JKL))
 301    CONTINUE
 302  CONTINUE
C
C     ----------------------------------------------------------
C
C*         4.      EFFECT OF CLOUDINESS ON LONGWAVE FLUXES
C                  ---------------------------------------
C
      DO 4004 JK = 1 , KFLEV+1
        DO 4003 JL = 1 , KDLON
          PFLUX(JL,1,JK) = ZFUP(JL,JK)
          PFLUX(JL,2,JK) = ZFDN(JL,JK)
 4003   CONTINUE
 4004 CONTINUE
C
c      IF (IMP.LT.4) THEN
c        PRINT 886,IMAXC
c      END IF

      IF (IMAXC.GT.0) THEN
        IMXP1 = IMAXC + 1
        IMXM1 = IMAXC - 1
C
C*         4.0     INITIALIZE TO CLEAR-SKY FLUXES
C                  ------------------------------
C
 400    CONTINUE
c
        DO 403 JK1=1,KFLEV+1
          DO 402 JK2=1,KFLEV+1
            DO 401 JL = 1 , KDLON
              ZUPF(JL,JK2,JK1)=ZFUP(JL,JK1)
              ZDNF(JL,JK2,JK1)=ZFDN(JL,JK1)
 401        CONTINUE
 402      CONTINUE
 403    CONTINUE
C
C*         4.1     FLUXES FOR ONE OVERCAST UNITY EMISSIVITY CLOUD
C                  ----------------------------------------------
C
 410  CONTINUE

        DO 413 JKC = 1 , IMAXC
          JCLOUD=JKC
          JKCP1=JCLOUD+1
C
C*         4.1.1   ABOVE THE CLOUD
C                  ---------------
C
 4110     CONTINUE

          DO 4115 JK=JKCP1,KFLEV+1
            JKM1=JK-1
            DO 4111 JL = 1 , KDLON
              ZFU(JL,JK)=0.
 4111       CONTINUE
            IF (JK .GT. JKCP1) THEN
              DO 4113 JKJ=JKCP1,JKM1
                DO 4112 JL = 1 , KDLON
                  ZFU(JL,JK) = ZFU(JL,JK) + ZCNTRB(JL,JK,JKJ)
 4112           CONTINUE
 4113         CONTINUE
            ENDIF

            DO 4114 JL = 1 , KDLON
              ZUPF(JL,JKCP1,JK)=ZBINT(JL,JK)-ZFU(JL,JK)
 4114       CONTINUE
 4115     CONTINUE
C
C*         4.1.2   FLUXES FOR ONE OVERCAST UNITY EMISSIVITY CLOUD
C                  ----------------------------------------------
C
 4120     CONTINUE

          DO 4125 JK=1,JCLOUD
            JKP1=JK+1
            DO 4121 JL = 1 , KDLON
              ZFD(JL,JK)=0.
 4121       CONTINUE

            IF (JK .LT. JCLOUD) THEN
              DO 4123 JKJ=JKP1,JCLOUD
                DO 4122 JL = 1 , KDLON
                  ZFD(JL,JK) = ZFD(JL,JK) + ZCNTRB(JL,JK,JKJ)
 4122           CONTINUE
 4123         CONTINUE
            ENDIF
            DO 4124 JL = 1 , KDLON
              ZDNF(JL,JKCP1,JK)=-ZBINT(JL,JK)-ZFD(JL,JK)
 4124       CONTINUE
 4125     CONTINUE
 413    CONTINUE
c        IF (IMP.LT.2) THEN
c          WRITE(NOUT,884) ((ZUPF(1,JK1,JK),JK=1,KFLEV+1),JK1=1,IMXP1)
c          WRITE(NOUT,884) ((ZDNF(1,JK1,JK),JK=1,KFLEV+1),JK1=1,IMXP1)
c        END IF
C
C*         4.2     FLUXES FOR PARTIAL/MULTIPLE LAYERED CLOUDINESS
C                  ----------------------------------------------
C
 420    CONTINUE
C
C
C*         4.2.1   DOWNWARD FLUXES
C                  ---------------
C
 4210   CONTINUE

        DO 4212 JK= IMXP1 , KFLEV+1
          DO 4211 JL = 1 , KDLON
            PFLUX(JL,2,JK)=ZDNF(JL,1,JK)
 4211     CONTINUE
 4212   CONTINUE

        IF(IMXM1.GE.1) THEN
          DO 4217 JK=1,IMXM1
            JKP1=JK+1
            DO 4213 JL = 1 , KDLON
              ZCLOUD(JL)=1.
              ZFD(JL,JK)=ZDNF(JL,JKP1,JK)*PCLDLW(JL,JK)
 4213       CONTINUE
            DO 4215 JKJ=JK,IMXM1
              JKJ1=JKJ+1
              JKJ2=JKJ+2
              DO 4214 JL = 1 , KDLON
                ZCLOUD(JL)=ZCLOUD(JL)*(1.-PCLDLW(JL,JKJ))
                ZCCLD=ZCLOUD(JL)*PCLDLW(JL,JKJ1)
                ZFD(JL,JK)=ZFD(JL,JK)+ZDNF(JL,JKJ2,JK)*ZCCLD
 4214         CONTINUE
 4215       CONTINUE
            JKJ=IMAXC
            DO 4216 JL = 1 , KDLON
              ZCLOUD(JL)=ZCLOUD(JL)*(1.-PCLDLW(JL,JKJ))
              ZFD(JL,JK)=ZFD(JL,JK)+ZDNF(JL,1,JK)*ZCLOUD(JL)
              PFLUX(JL,2,JK)=ZFD(JL,JK)
 4216       CONTINUE
 4217     CONTINUE
        ENDIF
C
C*         4.2.2   UPWARD FLUX AT THE SURFACE
C                  --------------------------
C
 4220   CONTINUE

        JK=IMAXC
        JKP1=JK+1
        DO 4221 JL = 1 , KDLON
          ZFD(JL,JK)=ZDNF(JL,JKP1,JK)*PCLDLW(JL,JK)
     *           +ZDNF(JL,1,JK)*(1.-PCLDLW(JL,JK))
          PFLUX(JL,2,JK)=ZFD(JL,JK)
          PFLUX(JL,1,1)=PEMIS(JL)*ZBSUIN(JL)-(1.-PEMIS(JL))
     *                 *PFLUX(JL,2,1)
 4221   CONTINUE
C
C*         4.2.3   UPWARD FLUXES
C                  -------------
C
 4230   CONTINUE

        DO 4235 JK = 2 , KFLEV+1
          JK1=MIN(JK,IMXP1)
          JK2=JK1-1
          JK2M1=JK2-1
          DO 4231 JL = 1 , KDLON
            ZCLOUD(JL)=1.
            ZFU(JL,JK)=ZUPF(JL,JK1,JK)*PCLDLW(JL,JK2)
 4231     CONTINUE

          IF(JK2M1.GE.1) THEN
            DO 4233 JKJ=1,JK2M1
              JKIJ=JK1-JKJ
              DO 4232 JL = 1 , KDLON
                ZCLOUD(JL)=ZCLOUD(JL)*(1.-PCLDLW(JL,JKIJ))
                ZCCLD=ZCLOUD(JL)*PCLDLW(JL,JKIJ-1)
                ZFU(JL,JK)=ZFU(JL,JK)+ZUPF(JL,JKIJ,JK)*ZCCLD
 4232         CONTINUE
 4233       CONTINUE
          ENDIF

          JKJ=JK2
          JKIJ=1
          DO 4234 JL = 1 , KDLON
            ZCLOUD(JL)=ZCLOUD(JL)*(1.-PCLDLW(JL,JKIJ))
            ZFU(JL,JK)=ZFU(JL,JK)+ZUPF(JL,1,JK)*ZCLOUD(JL)
            PFLUX(JL,1,JK)=ZFU(JL,JK)
 4234     CONTINUE
 4235   CONTINUE
      ENDIF
C
C*         4.3     END OF CLOUD EFFECT COMPUTATIONS
C
 430  CONTINUE

C
      DO 431 JL = 1 , KDLON
        ZFN10(JL) = PFLUX(JL,1,KLIM) + PFLUX(JL,2,KLIM)
 431  CONTINUE
      DO 433 JK = KLIM+1 , KFLEV+1
        DO 432 JL = 1 , KDLON
          ZFN10(JL) = ZFN10(JL) + PCTS(JL,JK-1)
          PFLUX(JL,1,JK) = ZFN10(JL)
          PFLUX(JL,2,JK) = 0.0
 432    CONTINUE
 433  CONTINUE
C
c      DO 431 JL = 1 , KDLON
c        PFLUX(JL,1,KFLEV+1) = PFLUX(JL,1,KFLEV)
c 431  CONTINUE

c      IF (IMP.LT.4) THEN
c        WRITE(NOUT,884) (PFLUX(1,1,JK),JK = 1 , KFLEV+1)
c        WRITE(NOUT,884) (PFLUX(1,2,JK),JK = 1 , KFLEV+1)
c      END IF
C
      DO 501 JL=1,NRLST
         DO 502 JK=1,NRLEV
C            PCLFR (JL,JK) = PCLD  (JL,JK)
C            PQLWP (JL,JK) = PQLW  (JL,JK)
            ZCLDLW(JL,JK) = PCLDLW(JL,JK)
            DO 503 II=1,2
               ZFLUX (JL,II,JK) = PFLUX (JL,II,JK)
               ZTAU  (JL,II,JK) = PTAU  (JL,II,JK)
 503        CONTINUE
            ZPMB  (JL,JK) = PPMB  (JL,JK)
            ZTAVE (JL,JK) = PTAVE (JL,JK)
            ZTL   (JL,JK) = PTL   (JL,JK)
 502     CONTINUE
         DO 504 II=1,2
            ZFLUX (JL,II,NRLEV+1) = PFLUX (JL,II,NRLEV+1)
 504      CONTINUE
         ZDT0(JL) = PDT0(JL)
         ZPMB(JL,NRLEV+1) = PPMB(JL,NRLEV+1)
         ZTL (JL,NRLEV+1) = PTL (JL,NRLEV+1)
 501  CONTINUE
C
      RETURN
C
C-----------------------------------------------------------------
C                       FORMATS
C                       -------
C
 884  FORMAT (1X,F7.2,18F6.1,F7.2)
 886  FORMAT (1X,20I5)
 888  FORMAT (1X,13E9.2)
 890  FORMAT (1X,2I4,13F9.2)
      END
C__________________________________________________________________
C
*DECK LWTT0
C ii) Replace the old LWTT with the new improved version.
C
      SUBROUTINE LWTT0(KDLON,KRLST,KFLEV,ITASK,PGGA,PGGB,PPPTY,PPETY)
*CALL ZPARB 
c      INCLUDE 'zparb.upd'
      REAL PGGA(KDLON,12,2),PGGB(KDLON,12,2),
     *                 PPPTY(KDLON,12), PPETY(KDLON,12)

      DO 97 JL=1,NRLST
         DO 98 II=1,NUA
            PUU(JL,II) = ZUU(JL,II)
 98      CONTINUE
         DO 99 JJ=1,NTRA
            PTT(JL,JJ) = ZTT(JL,JJ)
 99      CONTINUE
 97       CONTINUE
C
C
C*         1.     HORNER'S ALGORITHM FOR H2O AND CO2 TRANSMISSION
C                 -----------------------------------------
C
 100  CONTINUE

! changing - marc 30/3/10
!      DO 101 JL = 1 , 6*KDLON
!        ZZ(JL) = SQRT (PUU(JL,1))
!        ZXN(JL) = PGGA(JL,1,1) + ZZ(JL)*(PGGA(JL,1,2))
!        ZXD(JL) = PGGB(JL,1,1) + ZZ(JL)*(PGGB(JL,1,2)+ZZ(JL))
!        PTT(JL,1) = ZXN(JL) / ZXD(JL)
! 101  CONTINUE
      DO I=1,6
         DO JL=1,KDLON
            ZZ(JL) = SQRT (PUU(JL,i))
            ZXN(JL) = PGGA(JL,i,1) + ZZ(JL)*(PGGA(JL,i,2))
            ZXD(JL) = PGGB(JL,i,1) + ZZ(JL)*(PGGB(JL,i,2)+ZZ(JL))
            PTT(JL,i) = ZXN(JL) / ZXD(JL)
         ENDDO
      ENDDO

      DO 1003 JL = 1 , KDLON
C
        ZZ(JL) = SQRT (PUU(JL,8))
        ZXN(JL) = PGGA(JL,8,1) + ZZ(JL)*(PGGA(JL,8,2))
        ZXD(JL) = PGGB(JL,8,1) + ZZ(JL)*(PGGB(JL,8,2)+ZZ(JL))
        PTT(JL,8) = ZXN(JL)/ZXD(JL)
C
C  1250-1450 CM-1 H2O
C
        ZZ(JL) = SQRT (PUU(JL,29))
        ZXN(JL)= PGGA(JL,9,1) + ZZ(JL)*(PGGA(JL,9,2) )
        ZXD(JL)= PGGB(JL,9,1) + ZZ(JL)*(PGGB(JL,9,2) + ZZ(JL))
        PTT(JL,16) = ZXN(JL)/ZXD(JL)
C
C  800-970 CM-1 H2O
C
        ZZ(JL) = SQRT (PUU(JL,30))
        ZXN(JL)= PGGA(JL,10,1) + ZZ(JL)*(PGGA(JL,10,2) )
        ZXD(JL)= PGGB(JL,10,1) + ZZ(JL)*(PGGB(JL,10,2) + ZZ(JL))
        PTT(JL,17) = ZXN(JL)/ZXD(JL)
C
C  650-800 CM-1 H2O
C
        ZZ(JL) = SQRT (PUU(JL,31))
        ZXN(JL)= PGGA(JL,11,1) + ZZ(JL)*(PGGA(JL,11,2) )
        ZXD(JL)= PGGB(JL,11,1) + ZZ(JL)*(PGGB(JL,11,2) + ZZ(JL))
        PTT(JL,18) = ZXN(JL)/ZXD(JL)
1003  CONTINUE
C     --------------------------------------------------------
C
C*         1.1    2-D INTERPOLATION BETWEEN LBL CO2 TRANSMISSION
C                 -----------------------------------------------
C     
      DO 1011 JL = 1 , KDLON
        CGU  = LOG(PUU(JL,34))
        CGP  = LOG(PUU(JL,7)/PUU(JL,34))
        CGT  = PUU(JL,33)/PUU(JL,34)
        DTX  = CGT-TREF
        JU   =INT((CGU+13.)*2.)+1
        JP   =INT((CGP+8.)*2.)+1
        XIU  = (CGU - ULG(JU))*2.0
        XIP  = (CGP - PLG(JP))*2.0
        JP1  = JP+1
        JU1  = JU+1 
        XUP0 = XIU * XIP
        XUP1 = 1.0 - XIP -XIU + XUP0
        XUP2 = XIU - XUP0
        XUP3 = XIP - XUP0
        TBUP = XUP1 * TB (  JU ,   JP ,   2)
     S       + XUP2 * TB (  JU1,   JP ,   2)      
     S       + XUP3 * TB (  JU ,   JP1,   2)      
     S       + XUP0 * TB (  JU1,   JP1,   2)      
        ALUP = XUP1 * ALP(  JU ,   JP,    2)
     S       + XUP2 * ALP(  JU1,   JP,    2)      
     S       + XUP3 * ALP(  JU ,   JP1,   2)      
     S       + XUP0 * ALP(  JU1,   JP1,   2)      
        BTUP = XUP1 * BET(  JU ,   JP,    2) 
     S       + XUP2 * BET(  JU1,   JP,    2)       
     S       + XUP3 * BET(  JU ,   JP1,   2) 
     S       + XUP0 * BET(  JU1,   JP1,   2)
        TTRR=1.0-EXP(-TBUP/1000.)
        PTT(JL,7)=TTRR - (1.-TTRR)*DTX*(ALUP + BTUP*DTX)
c        print*,'ptt7',ptt(jl,7)
 1011 CONTINUE
C
C*         1.2    2-D INTERPOLATION BETWEEN LBL O3 TRANSMISSION
C                 -----------------------------------------------
C
      DO 1021 JL = 1 , KDLON
        CGU  = MAX( LOG(PUU(JL,12)), -17.5)
        CGP  = MAX( LOG(PUU(JL,13)/PUU(JL,12)), -8.0)
        CGT  = PUU(JL,32)/PUU(JL,12)
        DTX  = CGT-TREF
        JU   = INT((CGU+17.5)*2.)+1.
        JP   = INT((CGP+8.)*2.)+1.
        XIU  = (CGU - ULGO(JU))*2.0
        XIP  = (CGP - PLG (JP))*2.0
        JU1  = JU+1
        JP1  = JP+1
        XUP0 = XIU * XIP
        XUP1 = 1.0 - XIP -XIU + XUP0
        XUP2 = XIU - XUP0
        XUP3 = XIP - XUP0

        TBUP = XUP1 * TBO3( JU,   JP )
     S       + XUP2 * TBO3( JU1,  JP )
     S       + XUP3 * TBO3( JU,   JP1)
     S       + XUP0 * TBO3( JU1,  JP1)
        ALUP = XUP1 * AO3(  JU,   JP )
     S       + XUP2 * AO3(  JU1,  JP )      
     S       + XUP3 * AO3(  JU,   JP1)      
     S       + XUP0 * AO3(  JU1,  JP1)      
        BTUP = XUP1 * BO3(  JU,   JP ) 
     S       + XUP2 * BO3(  JU1,  JP )       
     S       + XUP3 * BO3(  JU,   JP1) 
     S       + XUP0 * BO3(  JU1,  JP1)
        TBUP2= XUP1 * TO3B( JU,   JP )
     S       + XUP2 * TO3B( JU1,  JP )
     S       + XUP3 * TO3B( JU,   JP1)
     S       + XUP0 * TO3B( JU1,  JP1)
        ALUP2= XUP1 * O3A(  JU,   JP )
     S       + XUP2 * O3A(  JU1,  JP )      
     S       + XUP3 * O3A(  JU,   JP1)      
     S       + XUP0 * O3A(  JU1,  JP1)      
        BTUP2= XUP1 * O3B(  JU,   JP ) 
     S       + XUP2 * O3B(  JU1,  JP )       
     S       + XUP3 * O3B(  JU,   JP1) 
     S       + XUP0 * O3B(  JU1,  JP1)
        TTRR=1.0-EXP(-TBUP/100.)
        PTT(JL,13)=TTRR - (1.-TTRR)*DTX*(ALUP + BTUP*DTX)
        TTRR2=1.0-EXP(-TBUP2/100.)
        PTT(JL,16)=TTRR2 - (1.-TTRR2)*DTX*(ALUP2 + BTUP2*DTX)
 1021 CONTINUE
C     -------------------------------------------------------
C
 200  CONTINUE                                                            
                                                                         
      DO 201 JL = 1 , KDLON
        PTT(JL, 9) = PTT(JL, 8)
        PTT(JL, 7) = PTT(JL, 7)*PTT(JL,16)
C                             
C-  CONTINUUM ABSORPTION: E- AND P-TYPE
C      
        ZPU   = PUU(JL,10)
        ZPU2  = ZPU*ZPU
        ZPU10 = PPPTY(JL,1)*(ZPU+0.3*ZPU2)/(1.+9.*ZPU)
        ZPU11 = 0.7*PPPTY(JL,2)*(ZPU+0.34*ZPU2)/(1.+2.*ZPU)
        ZPU12A= PPPTY(JL,3)*(ZPU+0.066*ZPU2)/(1.+0.27*ZPU)
        ZPU12B= PPPTY(JL,4)*(ZPU+0.057*ZPU2)/(1.+0.095*ZPU)
        ZPU13 = 0.017 * ZPU  
        ZPU14 = 0.041 * ZPU  
        ZPU12 = 0.017 * ZPU  
        ZPU16 = PPPTY(JL,8)*(ZPU+0.19*ZPU2)/(1.+0.85*ZPU)
        ZPU17 = PPPTY(JL,9)*(ZPU+0.035*ZPU2)/(1.+0.71*ZPU)

        ZEUB  = PUU(JL,11)
        ZEUB2 = ZEUB*ZEUB
        ZEUC  = PUU(JL,25)
        ZEUD  = PUU(JL,26)
        ZEUE  = PUU(JL,27)
        ZEUF  = PUU(JL,28)

        ZEU10 = PPETY(JL,1)*(ZEUB+5.*ZEUB2)/(1.+150.*ZEUB)
        ZEU11 = PPETY(JL,2)*(ZEUB+15.5*ZEUB2)/(1.+42.*ZEUB)
        ZEU12A= PPETY(JL,3)*(1.+7.*ZEUC)/(1.+17.5*ZEUC)*ZEUC 
        ZEU12B= PPETY(JL,4)*(1.+4.2*ZEUD)/(1.+9.5*ZEUD)*ZEUD  
        ZEU13 = PPETY(JL,5)*(1.+3.8*ZEUE)/(1.+7.1*ZEUE)*ZEUE
        ZEU14 = 6.5 * ZEUC
        ZEU12 = 7.0 * ZEUD
        ZEU16 = PPETY(JL,8)*(ZEUB+4.3*ZEUB2)/(1.+11.8*ZEUB)
        ZEU17 = PPETY(JL,9)*(1.+1.*ZEUF)/(1.+9.*ZEUF)*ZEUF 
C
C
C     ---------------------------------------------------------  
C                                                                         
C*         2     OZONE AND AEROSOL TRANSMISSION FUNCTIONS    
C                 -----------------------------------------
C                                                                         
C-  OZONE ABSORPTION                                                      
C                                                                         
c        ZX = MAX(1.0E-15, PUU(JL,12))
c        ZY = MAX(1.0E-15, PUU(JL,13))
c        ZUXY = 4. * ZX * ZX / (PIALF0 * ZY)                                 
c        ZSQ1 = SQRT(1. + O1H * ZUXY ) - 1.                                
c        ZSQ2 = SQRT(1. + O2H * ZUXY ) - 1.                                
c        ZVXY = PIALF0 * ZY / (2. * ZX)                                      
c        ZAERCN = PUU(JL,17) + ZEU12 + ZPU12                                 
c        ZTO11 = EXP( - ZVXY * ZSQ1 - ZAERCN )                              
c        ZTO2 = EXP( - ZVXY * ZSQ2 - ZAERCN )                              
C                                                                         
C-- TRACE GASES (CH4, N2O, CFC-11, CFC-12)                                
C                                                                         
C* CH4 IN INTERVAL 800-970 + 1110-1250 CM-1                               
C
      NEXOTIC=0                                                           
      IF (NEXOTIC.EQ.1) THEN

      ZXCH4 = MAX(1.0D-12, PUU(JL,19))
      ZYCH4 = MAX(1.0D-12, PUU(JL,20))
      ZUXY = 4. * ZXCH4*ZXCH4/(0.103*ZYCH4)                               
      ZSQH41 = SQRT(1. + 33.7 * ZUXY) - 1.                              
      ZVXY = 0.103 * ZYCH4 / (2. * ZXCH4)                                 
      ZODH41 = ZVXY * ZSQH41                                              
C                                                                         
C* N2O IN INTERVAL 800-970 + 1110-1250 CM-1                               
C                                                                         
      ZXN2O = MAX(1.0D-12, PUU(JL,21))
      ZYN2O = MAX(1.0D-12, PUU(JL,22))
      ZUXY = 4. * ZXN2O*ZXN2O/(0.416*ZYN2O)                               
      ZSQN21 = SQRT(1. + 21.3 * ZUXY) - 1.                              
      ZVXY = 0.416 * ZYN2O / (2. * ZXN2O)                                 
      ZODN21 = ZVXY * ZSQN21                                              
C                                                                         
C* CH4 IN INTERVAL 1250-1450 + 1880-2820 CM-1                             
C                                                                         
      ZUXY = 4. * ZXCH4*ZXCH4/(0.113*ZYCH4)                               
      ZSQH42 = SQRT(1. + 400. * ZUXY) - 1.                              
      ZVXY = 0.113 * ZYCH4 / (2. * ZXCH4)                                 
      ZODH42 = ZVXY * ZSQH42                                              
C                                                                         
C* N2O IN INTERVAL 1250-1450 + 1880-2820 CM-1                             
C                                                                         
      ZUXY = 4. * ZXN2O*ZXN2O/(0.197*ZYN2O)                               
      ZSQN22 = SQRT(1. + 2000. * ZUXY) - 1.                             
      ZVXY = 0.416 * ZYN2O / (2. * ZXN2O)                                 
      ZODN22 = ZVXY * ZSQN22                                              
C                                                                         
C* CFC-11 IN INTERVAL 800-970 + 1110-1250 CM-1                            
C                                                                         
      ZA11 = 2. * PUU(JL,23) * 4.404E+05                                  
      ZTTF11 = 1. - ZA11 * 0.003225                                       
C                                                                         
C* CFC-12 IN INTERVAL 800-970 + 1110-1250 CM-1                            
C                                                                         
      ZA12 = 2. * PUU(JL,24) * 6.7435E+05                                 
      ZTTF12 = 1. - ZA12 * 0.003225                                       
      ELSE                                                                
         ZODH41=0.                                                        
         ZODN21=0.                                                        
         ZODH42=0.                                                        
         ZODN22=0.                                                        
         ZTTF11=1.                                                        
         ZTTF12=1.                                                        
      END IF                                                              
C                                                                         
      TC1=EXP(-ZEU12A-ZPU12A)
      TC2=EXP(-ZEU12B-ZPU12B)
      PTT2=PTT(JL, 2)
      PTTB=PTT(JL,18)
      PTT(JL, 2) = 0.5 *(PTTB*TC2 + (2.*PTT2-PTTB)*TC1)
C
      TC1=EXP(-ZEU13-ZPU13)
      TC2=EXP(-ZEU14-ZPU14)
      PTT4=PTT(JL, 4)
      PTTB=PTT(JL,17)
      PTT(JL, 4) =  PTT4*TC2 + 0.54839*PTTB*(TC1-TC2)
C
      TC1=EXP(-ZEU16-ZPU16)
      TC2=EXP(-ZEU17-ZPU17)
      PTT6=PTT(JL, 6)
      PTTB=PTT(JL,16)
      PTT(JL, 6) =  PTT6*TC2 + 0.17544*PTTB*(TC1-TC2)
C
      ZUU11 = - PUU(JL,15) 
      ZUU12 = - PUU(JL,16) - ZODH41 - ZODN21          
      PTT(JL,10) = EXP( - PUU(JL,14) -ZEU10 -ZPU10 )
      PTT(JL,11) = EXP( ZUU11 )                                         
      PTT(JL,12) = EXP( ZUU12 ) * ZTTF11 * ZTTF12                       
      PTT(JL,14) = EXP( -PUU(JL,14) - ZEU11 - ZPU11 )                  
      PTT(JL,15) = EXP ( - PUU(JL,14) - ZODH42 - ZODN22 )               
C
c        TC1=EXP(-ZEU12A-ZPU12A)
c        TC2=EXP(-ZEU12B-ZPU12B)
c        PTT2=PTT(JL, 2)
c        PTTB=PTT(JL,18)
c        PTT(JL, 2) = 0.5 *(PTTB*TC2 + (2.*PTT2-PTTB)*TC1)
c
c        TC1=EXP(-ZEU13-ZPU13)
c        TC2=EXP(-ZEU14-ZPU14)
c        PTT4=PTT(JL, 4)
c        PTTB=PTT(JL,17)
c        PTT(JL, 4) =  PTT4*TC2 + 0.54839*PTTB*(TC1-TC2)
c
c        TC1=EXP(-ZEU16-ZPU16)
c        TC2=EXP(-ZEU17-ZPU17)
c        PTT6=PTT(JL, 6)
c        PTTB=PTT(JL,16)
c        PTT(JL, 6) =  PTT6*TC2 + 0.17544*PTTB*(TC1-TC2)
c
c        ZUU11 = - PUU(JL,15)
c        ZUU12 = - PUU(JL,16)          
c        PTT(JL,10) = EXP( - PUU(JL,14) -ZEU10 -ZPU10 )
c        PTT(JL,11) = EXP( ZUU11 )                                         
c        PTT(JL,12) = EXP( ZUU12 )                              
c        PTT(JL,14) = EXP( -PUU(JL,14) - ZEU11 - ZPU11 )                  
c        PTT(JL,15) = EXP ( - PUU(JL,14) )               
 201  CONTINUE                                                            
      DO 301 JL=1,NRLST
         DO 302 II=1,NUA
           ZUU(JL,II) = PUU(JL,II)
 302     CONTINUE
         DO 303 JJ=1,NTRA
           ZTT(JL,JJ) = 1.0
 303     CONTINUE
         IF(ITASK.EQ.1)THEN
           ZTT(JL,1)  = PTT(JL,1)
           ZTT(JL,2)  = PTT(JL,2)
           ZTT(JL,3)  = PTT(JL,3)
           ZTT(JL,4)  = PTT(JL,4)
           ZTT(JL,5)  = PTT(JL,5)
           ZTT(JL,6)  = PTT(JL,6)
         ELSE IF(ITASK.EQ.2)THEN
           ZTT(JL,1)  = PTT(JL,1)
           ZTT(JL,2)  = PTT(JL,2)
           ZTT(JL,3)  = PTT(JL,3)
           ZTT(JL,4)  = PTT(JL,4)
           ZTT(JL,5)  = PTT(JL,5)
           ZTT(JL,6)  = PTT(JL,6)
           ZTT(JL,10) = PTT(JL,10)
           ZTT(JL,11) = PTT(JL,11)
           ZTT(JL,12) = PTT(JL,12)
           ZTT(JL,14) = PTT(JL,14)
           ZTT(JL,15) = PTT(JL,15)
         ELSE IF(ITASK.EQ.3)THEN
           DO 3031 JJ=1,NTRA
             ZTT(JL,JJ) = PTT(JL,JJ)
c        print*,'lwtt0 ptt(jj)=',ptt(jl,jj) , jj              
 3031       CONTINUE
         ENDIF
 301  CONTINUE
      RETURN                                                              
      END   
C________________________________________________
*DECK LWTT1
C ii) Replace the old LWTT1 with the new improved version.
C
      SUBROUTINE LWTT1(KDLON,KRLST,KFLEV,ITASK,PGGA,PGGB,PPPTY,PPETY)
*CALL ZPARB 
c      INCLUDE 'zparb.upd'
      REAL PGGA(KDLON,12,2),PGGB(KDLON,12,2),
     *                 PPPTY(KDLON,12), PPETY(KDLON,12)

      DO 97 JL=1,NRLST
         DO 98 II=1,NUA
            PUU(JL,II) = ZUU(JL,II)
 98      CONTINUE
         DO 99 JJ=1,NTRA
            PTT(JL,JJ) = ZTT(JL,JJ)
 99      CONTINUE
 97   CONTINUE
C
C*         1.     HORNER'S ALGORITHM FOR H2O AND CO2 TRANSMISSION
C                 -----------------------------------------------
C
 100  CONTINUE

! changing - marc 30/3/10
c      DO 101 JL = 1 , 6*KDLON
c        ZZ(JL) = SQRT (PUU(JL,1))
c        ZXN(JL) = PGGA(JL,1,1) + ZZ(JL)*(PGGA(JL,1,2))
c        ZXD(JL) = PGGB(JL,1,1) + ZZ(JL)*(PGGB(JL,1,2)+ZZ(JL))
c        PTT(JL,1) = ZXN(JL) / ZXD(JL)
c 101  CONTINUE
      DO I=1,6
         DO JL=1,KDLON
            ZZ(JL) = SQRT (PUU(JL,i))
            ZXN(JL) = PGGA(JL,i,1) + ZZ(JL)*(PGGA(JL,i,2))
            ZXD(JL) = PGGB(JL,i,1) + ZZ(JL)*(PGGB(JL,i,2)+ZZ(JL))
            PTT(JL,i) = ZXN(JL) / ZXD(JL)
         ENDDO
      ENDDO

      DO 1003 JL = 1 , KDLON
C
        ZZ(JL) = SQRT (PUU(JL,8))
        ZXN(JL) = PGGA(JL,8,1) + ZZ(JL)*(PGGA(JL,8,2))
        ZXD(JL) = PGGB(JL,8,1) + ZZ(JL)*(PGGB(JL,8,2)+ZZ(JL))
        PTT(JL,8) = ZXN(JL)/ZXD(JL)
C
C  1250-1450 CM-1 H2O
C
        ZZ(JL) = SQRT (PUU(JL,29))
        ZXN(JL)= PGGA(JL,9,1) + ZZ(JL)*(PGGA(JL,9,2) )
        ZXD(JL)= PGGB(JL,9,1) + ZZ(JL)*(PGGB(JL,9,2) + ZZ(JL))
        PTT(JL,16) = ZXN(JL)/ZXD(JL)
C
C  800-970 CM-1 H2O
C
        ZZ(JL) = SQRT (PUU(JL,30))
        ZXN(JL)= PGGA(JL,10,1) + ZZ(JL)*(PGGA(JL,10,2) )
        ZXD(JL)= PGGB(JL,10,1) + ZZ(JL)*(PGGB(JL,10,2) + ZZ(JL))
        PTT(JL,17) = ZXN(JL)/ZXD(JL)
C
C  650-800 CM-1 H2O
C
        ZZ(JL) = SQRT (PUU(JL,31))
        ZXN(JL)= PGGA(JL,11,1) + ZZ(JL)*(PGGA(JL,11,2) )
        ZXD(JL)= PGGB(JL,11,1) + ZZ(JL)*(PGGB(JL,11,2) + ZZ(JL))
        PTT(JL,18) = ZXN(JL)/ZXD(JL)
1003  CONTINUE
C     -----------------------------------------------------------
C
C*         1.1    2-D INTERPOLATION BETWEEN LBL CO2 TRANSMISSION
C                 -----------------------------------------------
C
      DO 1011 JL = 1 , KDLON
        CGU  = LOG(PUU(JL,34))
        CGP  = LOG(PUU(JL,7)/PUU(JL,34))
        CGT  = PUU(JL,33)/PUU(JL,34)
        DTX  = CGT-TREF
        JU   =INT((CGU+13.)*2.)+1
        JP   =INT((CGP+8.)*2.)+1
        XIU  = (CGU - ULG(JU))*2.0
        XIP  = (CGP - PLG(JP))*2.0
        JP1  = JP+1
        JU1  = JU+1 
        XUP0 = XIU * XIP
        XUP1 = 1.0 - XIP -XIU + XUP0
        XUP2 = XIU - XUP0
        XUP3 = XIP - XUP0
        TBUP = XUP1 * TB (  JU ,   JP ,   1)
     S       + XUP2 * TB (  JU1,   JP ,   1)      
     S       + XUP3 * TB (  JU ,   JP1,   1)      
     S       + XUP0 * TB (  JU1,   JP1,   1)      
        ALUP = XUP1 * ALP(  JU ,   JP,    1)
     S       + XUP2 * ALP(  JU1,   JP,    1)      
     S       + XUP3 * ALP(  JU ,   JP1,   1)      
     S       + XUP0 * ALP(  JU1,   JP1,   1)      
        BTUP = XUP1 * BET(  JU ,   JP,    1) 
     S       + XUP2 * BET(  JU1,   JP,    1)       
     S       + XUP3 * BET(  JU ,   JP1,   1) 
     S       + XUP0 * BET(  JU1,   JP1,   1)
        TTRR=1.0-EXP(-TBUP/1000.)
        PTT(JL,7)=TTRR - (1.-TTRR)*DTX*(ALUP + BTUP*DTX)
 1011 CONTINUE
C
C*         1.2    2-D INTERPOLATION BETWEEN LBL O3 TRANSMISSION
C                 -----------------------------------------------
C
      DO 1021 JL = 1 , KDLON
        CGU  = MAX( LOG(PUU(JL,12)), -17.5)
        CGP  = MAX( LOG(PUU(JL,13)/PUU(JL,12)), -8.0)
        CGT  = PUU(JL,32)/PUU(JL,12)
        DTX  = CGT-TREF
        JU   = INT((CGU+17.5)*2.)+1.
        JP   = INT((CGP+8.)*2.)+1.
        XIU  = (CGU - ULGO(JU))*2.0
        XIP  = (CGP - PLG (JP))*2.0
        JU1  = JU+1
        JP1  = JP+1
        XUP0 = XIU * XIP
        XUP1 = 1.0 - XIP -XIU + XUP0
        XUP2 = XIU - XUP0
        XUP3 = XIP - XUP0
        TBUP = XUP1 * TBO3( JU,   JP )
     S       + XUP2 * TBO3( JU1,  JP )
     S       + XUP3 * TBO3( JU,   JP1)
     S       + XUP0 * TBO3( JU1,  JP1)
        ALUP = XUP1 * AO3(  JU,   JP )
     S       + XUP2 * AO3(  JU1,  JP )      
     S       + XUP3 * AO3(  JU,   JP1)      
     S       + XUP0 * AO3(  JU1,  JP1)      
        BTUP = XUP1 * BO3(  JU,   JP ) 
     S       + XUP2 * BO3(  JU1,  JP )       
     S       + XUP3 * BO3(  JU,   JP1) 
     S       + XUP0 * BO3(  JU1,  JP1)
        TBUP2= XUP1 * TO3B( JU,   JP )
     S       + XUP2 * TO3B( JU1,  JP )
     S       + XUP3 * TO3B( JU,   JP1)
     S       + XUP0 * TO3B( JU1,  JP1)
        ALUP2= XUP1 * O3A(  JU,   JP )
     S       + XUP2 * O3A(  JU1,  JP )      
     S       + XUP3 * O3A(  JU,   JP1)      
     S       + XUP0 * O3A(  JU1,  JP1)      
        BTUP2= XUP1 * O3B(  JU,   JP ) 
     S       + XUP2 * O3B(  JU1,  JP )       
     S       + XUP3 * O3B(  JU,   JP1) 
     S       + XUP0 * O3B(  JU1,  JP1)
        TTRR=1.0-EXP(-TBUP/100.)
        PTT(JL,13)=TTRR - (1.-TTRR)*DTX*(ALUP + BTUP*DTX)
        TTRR2=1.0-EXP(-TBUP2/100.)
        PTT(JL,16)=TTRR2 - (1.-TTRR2)*DTX*(ALUP2 + BTUP2*DTX)
 1021 CONTINUE
C     -----------------------------------------------------
C
 200  CONTINUE                                                            
                                                                         
      DO 201 JL = 1 , KDLON
        PTT(JL, 9) = PTT(JL, 8)
        PTT(JL, 7) = PTT(JL, 7)*PTT(JL,16)
C                             
C-  CONTINUUM ABSORPTION: E- AND P-TYPE
C      
        ZPU   = PUU(JL,10)
        ZPU2  = ZPU*ZPU
        ZPU10 = PPPTY(JL,1)*(ZPU+0.3*ZPU2)/(1.+9.*ZPU)
        ZPU11 = 0.7*PPPTY(JL,2)*(ZPU+0.34*ZPU2)/(1.+2.*ZPU)
        ZPU12A= PPPTY(JL,3)*(ZPU+0.066*ZPU2)/(1.+0.27*ZPU)
        ZPU12B= PPPTY(JL,4)*(ZPU+0.057*ZPU2)/(1.+0.095*ZPU)
        ZPU13 = 0.017 * ZPU  
        ZPU14 = 0.041 * ZPU  
        ZPU12 = 0.017 * ZPU  
        ZPU16 = PPPTY(JL,8)*(ZPU+0.19*ZPU2)/(1.+0.85*ZPU)
        ZPU17 = PPPTY(JL,9)*(ZPU+0.035*ZPU2)/(1.+0.71*ZPU)

        ZEUB  = PUU(JL,11)
        ZEUB2 = ZEUB*ZEUB
        ZEUC  = PUU(JL,25)
        ZEUD  = PUU(JL,26)
        ZEUE  = PUU(JL,27)
        ZEUF  = PUU(JL,28)

        ZEU10 = PPETY(JL,1)*(ZEUB+5.*ZEUB2)/(1.+150.*ZEUB)
        ZEU11 = PPETY(JL,2)*(ZEUB+15.5*ZEUB2)/(1.+42.*ZEUB)
        ZEU12A= PPETY(JL,3)*(1.+7.*ZEUC)/(1.+17.5*ZEUC)*ZEUC 
        ZEU12B= PPETY(JL,4)*(1.+4.2*ZEUD)/(1.+9.5*ZEUD)*ZEUD  
        ZEU13 = PPETY(JL,5)*(1.+3.8*ZEUE)/(1.+7.1*ZEUE)*ZEUE
        ZEU14 = 6.5 * ZEUC
        ZEU12 = 7.0 * ZEUD
        ZEU16 = PPETY(JL,8)*(ZEUB+4.3*ZEUB2)/(1.+11.8*ZEUB)
        ZEU17 = PPETY(JL,9)*(1.+1.*ZEUF)/(1.+9.*ZEUF)*ZEUF 
C
C
C     ---------------------------------------------------------  
C                                                                         
C*         2     OZONE AND AEROSOL TRANSMISSION FUNCTIONS    
C                 -----------------------------------------
C                                                                         
C-  OZONE ABSORPTION                                                      
C                                                                         
c        ZX = MAX(1.0E-15, PUU(JL,12))
c        ZY = MAX(1.0E-15, PUU(JL,13))
c        ZUXY = 4. * ZX * ZX / (PIALF0 * ZY)                                 
c        ZSQ1 = SQRT(1. + O1H * ZUXY ) - 1.                                
c        ZSQ2 = SQRT(1. + O2H * ZUXY ) - 1.                                
c        ZVXY = PIALF0 * ZY / (2. * ZX)                                      
c        ZAERCN = PUU(JL,17) + ZEU12 + ZPU12                                 
c        ZTO11 = EXP( - ZVXY * ZSQ1 - ZAERCN )                              
c        ZTO2 = EXP( - ZVXY * ZSQ2 - ZAERCN )                              
C                                                                         
C-- TRACE GASES (CH4, N2O, CFC-11, CFC-12)                                
C                                                                         
C* CH4 IN INTERVAL 800-970 + 1110-1250 CM-1                               
C                                                                         
      NEXOTIC=0                                                           
      IF (NEXOTIC.EQ.1) THEN                                              
      ZXCH4 = MAX(1.0D-12, PUU(JL,19))
      ZYCH4 = MAX(1.0D-12, PUU(JL,20))
      ZUXY = 4. * ZXCH4*ZXCH4/(0.103*ZYCH4)                               
      ZSQH41 = SQRT(1. + 33.7 * ZUXY) - 1.                              
      ZVXY = 0.103 * ZYCH4 / (2. * ZXCH4)                                 
      ZODH41 = ZVXY * ZSQH41                                              
C                                                                         
C* N2O IN INTERVAL 800-970 + 1110-1250 CM-1                               
C                                                                         
      ZXN2O = MAX(1.0D-12, PUU(JL,21))
      ZYN2O = MAX(1.0D-12, PUU(JL,22))
      ZUXY = 4. * ZXN2O*ZXN2O/(0.416*ZYN2O)                               
      ZSQN21 = SQRT(1. + 21.3 * ZUXY) - 1.                              
      ZVXY = 0.416 * ZYN2O / (2. * ZXN2O)                                 
      ZODN21 = ZVXY * ZSQN21                                              
C                                                                         
C* CH4 IN INTERVAL 1250-1450 + 1880-2820 CM-1                             
C                                                                         
      ZUXY = 4. * ZXCH4*ZXCH4/(0.113*ZYCH4)                               
      ZSQH42 = SQRT(1. + 400. * ZUXY) - 1.                              
      ZVXY = 0.113 * ZYCH4 / (2. * ZXCH4)                                 
      ZODH42 = ZVXY * ZSQH42                                              
C                                                                         
C* N2O IN INTERVAL 1250-1450 + 1880-2820 CM-1                             
C                                                                         
      ZUXY = 4. * ZXN2O*ZXN2O/(0.197*ZYN2O)                               
      ZSQN22 = SQRT(1. + 2000. * ZUXY) - 1.                             
      ZVXY = 0.416 * ZYN2O / (2. * ZXN2O)                                 
      ZODN22 = ZVXY * ZSQN22                                              
C                                                                         
C* CFC-11 IN INTERVAL 800-970 + 1110-1250 CM-1                            
C                                                                         
      ZA11 = 2. * PUU(JL,23) * 4.404E+05                                  
      ZTTF11 = 1. - ZA11 * 0.003225                                       
C                                                                         
C* CFC-12 IN INTERVAL 800-970 + 1110-1250 CM-1                            
C                                                                         
      ZA12 = 2. * PUU(JL,24) * 6.7435E+05                                 
      ZTTF12 = 1. - ZA12 * 0.003225                                       
      ELSE    
         ZODH41=0.                                                        
         ZODN21=0.                                                        
         ZODH42=0.                                                        
         ZODN22=0.                                                        
         ZTTF11=1.                                                        
         ZTTF12=1.                                                        
      END IF                                                              
C                                                                         
      TC1=EXP(-ZEU12A-ZPU12A)
      TC2=EXP(-ZEU12B-ZPU12B)
      PTT2=PTT(JL, 2)
      PTTB=PTT(JL,18)
      PTT(JL, 2) = 0.5 *(PTTB*TC2 + (2.*PTT2-PTTB)*TC1)
C
      TC1=EXP(-ZEU13-ZPU13)
      TC2=EXP(-ZEU14-ZPU14)
      PTT4=PTT(JL, 4)
      PTTB=PTT(JL,17)
      PTT(JL, 4) =  PTT4*TC2 + 0.54839*PTTB*(TC1-TC2)
C
      TC1=EXP(-ZEU16-ZPU16)
      TC2=EXP(-ZEU17-ZPU17)
      PTT6=PTT(JL, 6)
      PTTB=PTT(JL,16)
      PTT(JL, 6) =  PTT6*TC2 + 0.17544*PTTB*(TC1-TC2)
C
      ZUU11 = - PUU(JL,15) 
      ZUU12 = - PUU(JL,16) - ZODH41 - ZODN21          
      PTT(JL,10) = EXP( - PUU(JL,14) -ZEU10 -ZPU10 )
      PTT(JL,11) = EXP( ZUU11 )                                         
      PTT(JL,12) = EXP( ZUU12 ) * ZTTF11 * ZTTF12                       
      PTT(JL,14) = EXP( -PUU(JL,14) - ZEU11 - ZPU11 )                  
      PTT(JL,15) = EXP ( - PUU(JL,14) - ZODH42 - ZODN22 )               
c
c        TC1=EXP(-ZEU12A-ZPU12A)
c        TC2=EXP(-ZEU12B-ZPU12B)
c        PTT2=PTT(JL, 2)
c        PTTB=PTT(JL,18)
c        PTT(JL, 2) = 0.5 *(PTTB*TC2 + (2.*PTT2-PTTB)*TC1)
c
c        TC1=EXP(-ZEU13-ZPU13)
c        TC2=EXP(-ZEU14-ZPU14)
c        PTT4=PTT(JL, 4)
c        PTTB=PTT(JL,17)
c        PTT(JL, 4) =  PTT4*TC2 + 0.54839*PTTB*(TC1-TC2)
c
c        TC1=EXP(-ZEU16-ZPU16)
c        TC2=EXP(-ZEU17-ZPU17)
c        PTT6=PTT(JL, 6)
c        PTTB=PTT(JL,16)
c        PTT(JL, 6) =  PTT6*TC2 + 0.17544*PTTB*(TC1-TC2)
c
c        ZUU11 = - PUU(JL,15)
c        ZUU12 = - PUU(JL,16)          
c        PTT(JL,10) = EXP( - PUU(JL,14) -ZEU10 -ZPU10 )
c        PTT(JL,11) = EXP( ZUU11 )                                         
c        PTT(JL,12) = EXP( ZUU12 )                              
c        PTT(JL,14) = EXP( -PUU(JL,14) - ZEU11 - ZPU11 )                  
c        PTT(JL,15) = EXP ( - PUU(JL,14) )
c        print*,'lwtt1 ptt(jj)=',ptt(jl,jj),jj               
 201  CONTINUE                                                            
      DO 301 JL=1,NRLST
         DO 302 II=1,NUA
           ZUU(JL,II) = PUU(JL,II)
 302     CONTINUE
         DO 303 JJ=1,NTRA
           ZTT(JL,JJ) = 1.0
 303     CONTINUE
         IF(ITASK.EQ.1)THEN
           ZTT(JL,1)  = PTT(JL,1)
           ZTT(JL,2)  = PTT(JL,2)
           ZTT(JL,3)  = PTT(JL,3)
           ZTT(JL,4)  = PTT(JL,4)
           ZTT(JL,5)  = PTT(JL,5)
           ZTT(JL,6)  = PTT(JL,6)
         ELSE IF(ITASK.EQ.2)THEN
           ZTT(JL,1)  = PTT(JL,1)
           ZTT(JL,2)  = PTT(JL,2)
           ZTT(JL,3)  = PTT(JL,3)
           ZTT(JL,4)  = PTT(JL,4)
           ZTT(JL,5)  = PTT(JL,5)
           ZTT(JL,6)  = PTT(JL,6)
           ZTT(JL,10) = PTT(JL,10)
           ZTT(JL,11) = PTT(JL,11)
           ZTT(JL,12) = PTT(JL,12)
           ZTT(JL,14) = PTT(JL,14)
           ZTT(JL,15) = PTT(JL,15)
         ELSE IF(ITASK.EQ.3)THEN
           DO 3031 JJ=1,NTRA
             ZTT(JL,JJ) = PTT(JL,JJ)
 3031       CONTINUE
         ENDIF
 301  CONTINUE
      RETURN                                                              
      END   
C
*DECK RADLSW
      SUBROUTINE RADLSW ( KDLON, KRLST, KFLEV, KFLVP1, KFLEV2,
     S            KNGL, KNGLP1, PSIG,  PSCT, PCARDI, ITASK, ZTM1
     S ,PIERSLWP)
C
C**** *RADLSW* - RADIATION INTERFACE
C
C     PURPOSE.
C     --------
C           CONTROLS RADIATION TRANSFER COMPUTATIONS
C
C**   INTERFACE.
C     ----------
C
C          *RADLSW* IS CALLED FROM *RADINT*
C
C        EXPLICIT ARGUMENTS
C        --------------------
C
C        IMPLICIT ARGUMENTS
C        --------------------
C
C     ==== INPUTS ===
C     ==== OUTPUTS ===
C FLS(KDLON,KFLEV)            ; NET SHORTWAVE RADIATIVE FLUXES
C FLT(KDLON,KFLEV)            ; NET LONGWAVE RADIATIVE FLUXES
C
C     METHOD.
C     -------
C        SEE DOCUMENTATION
C
C     EXTERNALS.
C     ----------
C
C     REFERENCE.
C     ----------
C        ECMWF MODEL DOCUMENTATION
C
C     AUTHORS.
C     --------
C        JEAN-JACQUES MORCRETTE  *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C        ORIGINAL : 88-02-04
C------------------------------------------------------------
C      IMPLICIT LOGICAL (L)
C
*CALL ZPARB 
c      INCLUDE 'zparb.upd'
      REAL ZTM1(NLON,NLEV)
      integer ipass
      data ipass/1/
C
C----------------------------------------------------------------
C
C     ----------------------------------------------------------
C
C
CDIR$ VFUNCTION ALOG,EXP
C     ----------------------
C
C    -----------------------------------------------------------
C     ----------------------------------------------------------
C
C*       0.3   SET-UP RADIATION ROUTINE COEFFICIENTS
C              -------------------------------------
C
  30  CONTINUE

      IF (IPASS.eq.1) THEN
       IPASS=0
      CALL SUAER
      CALL SULW
      CALL SUSW
       ENDIF
C
      IMP=2
ccc1      NOUT=6
cc      NOUT=2
cc      open(2,FILE='termout.dat',STATUS='unknown')
      IABS=5
      IAER=0
      LORAD = .TRUE.
C
      ZEPSC  = 1.E-04
      ZEPSCO = 1.E-10
      ZEPSCQ = 1.E-07
      ZEPSCT = 1.E-04
      ZEPSCW = 1.E-12
      ZEELOG = 1.E-10
C
      CDAY   = DAYL * G / CPD
      CH2O   = 5.3669274E-03
      CCO2   = 5.8269497E-03
      DIFF   = 1.66
      X10E   = 0.4342945
      CCLWMR = 0.2
C
C     ---------------------------------------------------------
C
C*         1.     SET-UP INPUT QUANTITIES FOR RADIATION
C                 -------------------------------------
C
 100  CONTINUE
      DO 101 JL=1,NRLST
       DO 102 JK=1,NRLEV
        PCLFR(JL,JK) = CLC (JL,JK)
        PT   (JL,JK) = TI  (JL,JK)
        PDP  (JL,JK) = DP  (JL,JK)
        PWV  (JL,JK) = WV  (JL,JK)
        PQOF (JL,JK) = QOF (JL,JK)
C        PQCF (JL,JK) = QCF (JL,JK)
        PQLWP(JL,JK) = CLWA(JL,JK)
        PAER (JL,JK,1) = AEQ1(JL,JK)
        PAER (JL,JK,2) = AEQ2(JL,JK)
        PAER (JL,JK,3) = AEQ3(JL,JK)
        PAER (JL,JK,4) = AEQ4(JL,JK)
        PAER (JL,JK,5) = AEQ5(JL,JK)
 102   CONTINUE
       PT (JL,NRLEV+1) = TI(JL,NRLEV+1)
       PPSOL (JL) = APRE(JL)
c       print *, ' ppsol ', ppsol(jl)
       PRMU0 (JL) = SMU0(JL)
       PEMIS (JL) = ALTE(JL)
       PALBSN(JL) = ALSO(JL)
 101  CONTINUE
C
C
C*         1.1    INITIALIZE VARIOUS FIELDS
C                 -------------------------
C
 110  CONTINUE
C
      DO 112 JK = 1 , KFLEV
      DO 111 JL = 1 , KDLON
      ZCOOLR(JL,JK) = 0.
      ZHEATR(JL,JK) = 0.
 111  CONTINUE
 112  CONTINUE
C
      DO 113 JL = 1 , KDLON
         ZALBSU(JL,1)=PALBSN(JL)
         ZALBSU(JL,2)=PALBSN(JL)
         ZFSUP(JL,KFLEV+1) = 0.
         ZFSDWN(JL,KFLEV+1) = ZEPSCO
         ZFLUX(JL,1,KFLEV+1) = 0.
         ZFLUX(JL,2,KFLEV+1) = 0.
         PEMIS(JL) = 1. - PEMIS(JL)
         ZPMB(JL,1) = PPSOL(JL)/100.
c         print *,' zpmb ',zpmb(jl,1)
         ZTL(JL,KFLEV+1) = PT(JL,1)
         ZDT0(JL) = 0.
 113  CONTINUE
C
      DO 115 JK = 1 , KFLEV
         JKP1 = JK + 1
         JKL = KFLEV+ 1 - JK
         JKLP1 = JKL + 1
         DO 114 JL = 1 , KDLON
         ZPMB(JL,JK+1) = ZPMB(JL,JK) - PDP(JL,JKL) / 100.
         ZTL(JL,JK) = PT(JL,JKLP1)
C         ZTAVE(JL,JK) = 0.5*(PT(JL,JKL)+PT(JL,JKLP1))
         ZTAVE(JL,JK) = ZTM1(JL,JKL)
         ZOZ(JL,JK)   = PQOF(JL,JKL) * 46.6968 / G
         ZCLDSW(JL,JK) = PCLFR(JL,JKL)
         ZCLDLW(JL,JK) = PCLFR(JL,JKL)

         ZTAU(JL,1,JK) = ZEPSCW
         ZTAU(JL,2,JK) = ZEPSCW
         ZOMEGA(JL,1,JK) = 0.9994
         ZOMEGA(JL,2,JK) = 0.9963
         ZCG(JL,1,JK) = 0.865
         ZCG(JL,2,JK) = 0.910
         ZCOOLR(JL,JK) = 0.
         ZHEATR(JL,JK) = 0.
         ZFSUP(JL,JK) = 0.
         ZFSDWN(JL,JK) = 0.
         ZFLUX(JL,1,JK) = 0.
         ZFLUX(JL,2,JK) = 0.
 114     CONTINUE
 115  CONTINUE
C
C     --------------------------------------------------------
C
C*         2.     CLOUD AND AEROSOL PARAMETERS
C                 ----------------------------
C
 200  CONTINUE
C
      PIERSLWP=0.0
      DO 202 JK = 2 , KFLEV-1
         JKL = KFLEV + 1 - JK
         DO 201 JL = 1 , KDLON
         LO1 = PCLFR(JL,JKL).GT.ZEPSC
         ZLWGKG = CVMGT(PQLWP(JL,JKL)*1000./PCLFR(JL,JKL),0.D0,LO1)
         ZFCCA = MIN( PCLFR(JL,JKL) , PCLFR(JL,JKL-1) )
         ZFCCB = MIN( PCLFR(JL,JKL) , PCLFR(JL,JKL+1) )
         ZFCC  = MAX( ZFCCA , ZFCCB )
         ZFCC = 0.0
         ZLWGKG = ZFCC * CCLWMR + (PCLFR(JL,JKL) - ZFCC) * ZLWGKG
         ZFLWP(JL) = CVMGT( ZLWGKG*PDP(JL,JKL)/(G*PCLFR(JL,JKL)) ,
     S                    ZEPSCW , LO1 )
         PIERSLWP=PIERSLWP+ZFLWP(JL)
         ZCLDSW(JL,JK) = PCLFR(JL,JKL)
         ZCLDLW(JL,JK) = PCLFR(JL,JKL)*(1. - EXP(-0.158 * ZFLWP(JL)))
         ZRADEF = 15.0
         ZTAUEQ = 1.5 * ZFLWP(JL) / ZRADEF
         ZTAU(JL,1,JK) = ZTAUEQ
         ZTAU(JL,2,JK) = ZTAUEQ
         ZOMEGA(JL,1,JK) = 0.9999 - 5.0E-04*EXP(-0.5 * ZTAUEQ)
         ZOMEGA(JL,2,JK) = 0.9988 - 2.5E-03*EXP(-0.05 * ZTAUEQ)
         ZCG(JL,1,JK)=0.865
         ZCG(JL,2,JK)=0.910
 201     CONTINUE
 202  CONTINUE
C
      DO 203 JL = 1 , KDLON
      ZPMB(JL,KFLEV+1)=0.0
      ZTAVE(JL,KFLEV)=ZTL(JL,KFLEV+1)
 203  CONTINUE
C
      NUAER = NUA
      NTRAER = NTRA
C
      IF (IAER.EQ.0) THEN
C         NUAER = 13
C         NUAER = 13
C         NTRAER = 14
         DO 206 JK = 1 , KFLEV
         DO 205 JAE = 1 , 5
         DO 204 JL = 1 , KDLON
         PAER(JL,JK,JAE)=1.E-15
 204     CONTINUE
 205     CONTINUE
 206     CONTINUE
      END IF
C
C
C      END IF
C
C     -----------------------------------------------------------------
C
C*         3.     CALL LONGWAVE RADIATION CODE
C                 ----------------------------
C
 300  CONTINUE
c      print *,' hi ',kflev+1,ZPMB(1,KFLEV+1),zpmb(1,1)
c      DO JK=KFLEV,1,-1
c       JKL=KFLEV+1-JK
c        print *,jk,zpmb(1,jk),pdp(1,jkl),ztave(1,jk),
c     $      qof(1,jkl)/pdp(1,jkl),wv(1,jkl),pqlwp(1,jkl)
c      enddo
c zflux is as tave and zpmb with level 1 the surface
C
      CALL LW ( KDLON, KRLST, KFLEV, KFLVP1, KFLEV2, KNGL, KNGLP1,
     S          PSIG, PSCT, PCARDI, ITASK                         )
C
C     -----------------------------------------------------------------
C
C*         4.     CALL SHORTWAVE RADIATION CODE
C                 -----------------------------
C
 400  CONTINUE
C
      ZZRMUZ=0.
      DO 401 JL = 1 , KDLON
         ZZRMUZ = MAX(ZZRMUZ, PRMU0(JL))
 401  CONTINUE
C
      IF (ZZRMUZ.GT.0.) THEN
C
         CALL SW ( KDLON, KRLST, KFLEV, KFLVP1, PSIG, PSCT, PCARDI,
     S             ITASK )
C
      END IF
C
C
C     ---------------------------------------------------------------
C
C*         5.     FILL UP THE MODEL NET LW AND SW RADIATIVE FLUXES
C                 ------------------------------------------------
C
 500  CONTINUE
C
      DO 501 JL = 1 , KDLON
       PEMIS(JL) = 1. - PEMIS(JL)
 501  CONTINUE
C
      DO 503 JKL = 1 , KFLEV+1
         JK = KFLVP1 + 1 - JKL
         DO 502 JL = 1 , KDLON
            FLS(JL,JKL) = ZFSDWN(JL,JK) - ZFSUP(JL,JK)
            FLT(JL,JKL) = - ZFLUX(JL,1,JK) - ZFLUX(JL,2,JK)
 502     CONTINUE
 503  CONTINUE
C
C
C*         5.1     HEATING/COOLING RATES (K/DAY)
C                  -----------------------------
 510  CONTINUE
C
      DO 512 JK = 1 , KFLEV
      JKL = KFLEV+1 - JK
      DO 511 JL = 1 , KDLON
      ZDFNET = ZFLUX(JL,1,JK+1) + ZFLUX(JL,2,JK+1)
     S              -ZFLUX(JL,1,JK  ) - ZFLUX(JL,2,JK  )
      ZCOOLR(JL,JK) = CDAY * ZDFNET / PDP(JL,JKL)
      ZDFNET = ZFSUP(JL,JK  ) - ZFSDWN(JL,JK  )
     S        -ZFSUP(JL,JK+1) + ZFSDWN(JL,JK+1)
      ZHEATR(JL,JK) = CDAY * ZDFNET / PDP(JL,JKL)
 511  CONTINUE
 512  CONTINUE
CCC  Combine the lowerest two layer as one layer to correspond 
CCC  the radiative convective model. (leave the lowerest level 
CCC  heating rate undefined---set to zero)   D.LI 8/8 1991
      DO 518 JL = 1, KDLON
      ZDFNET = ZFLUX(JL,1,3) + ZFLUX(JL,2,3)
     S        -ZFLUX(JL,1,1) - ZFLUX(JL,2,1)
      ZCOOLR(JL,2) = CDAY*ZDFNET/(PDP(JL,KFLEV-1)+PDP(JL,KFLEV))
      ZCOOLR(JL,1)=0.0D0
      ZDFNET = ZFSUP(JL,1) - ZFSDWN(JL,1)
     S        -ZFSUP(JL,3) + ZFSDWN(JL,3)
      ZHEATR(JL,2) = CDAY*ZDFNET/(PDP(JL,KFLEV-1)+PDP(JL,KFLEV))
      ZHEATR(JL,1)=0.0D0
 518  CONTINUE
CCC
C
c      IF (IMP.LT.4) THEN
c       JL = 1
c       WRITE(NOUT,891) (ZFLUX(JL,1,JK),JK = 1 , KFLVP1)
c       WRITE(NOUT,891) (ZFLUX(JL,2,JK),JK = 1 , KFLVP1)
c       WRITE(NOUT,889) (ZCOOLR(JL,JK),JK = 1 , KFLEV)
c      WRITE(NOUT,891) (ZFSUP(JL,JK),JK = 1 , KFLVP1)
c      WRITE(NOUT,891) (ZFSDWN(JL,JK),JK = 1 , KFLVP1)
c      WRITE(NOUT,889) (ZHEATR(JL,JK),JK = 1 , KFLEV)
C      END IF
C
C-----------------------------------------------------------------
C                       FORMATS
C                       -------
C
C
 883  FORMAT(2X,16E8.2)
 884  FORMAT(4X,18F7.4)
 885  FORMAT(4X,18F7.1)
 887  FORMAT(1X,10E12.6)
 889  FORMAT(4X,18F7.3)
 891  FORMAT(1X,18F7.1)
C
C     ------------------------------------------------------
C
      RETURN
      END
C
*DECK SW
      SUBROUTINE SW ( KDLON,KRLST,KFLEV,KFLVP1,PSIG,PSCT,PCARDI,ITASK )
C
C**** *SW* - COMPUTES THE SHORTWAVE RADIATION FLUXES.
C
C     PURPOSE.
C     --------
C           COMPUTES THE SHORTWAVE RADIATION FLUXES IN TWO SPECTRAL
C     INTERVALS FOLLOWING FOUQUART AND BONNEL (1980).
C
C**   INTERFACE.
C     ----------
C          *SW* IS CALLED BY *RADLSW*
C
C
C        IMPLICIT ARGUMENTS
C        --------------------
C
C     ==== INPUTS ===
C     ==== OUTPUTS ===
C
C     METHOD.
C     -------
C
C          1. COMPUTES ABSORBER AMOUNTS WITH TEMPERATURE AND PRESSURE
C     SCALING.
C          2. COMPUTES UPWARD AND DOWNWARD FLUXES IN THE 0.25-0.68
C     MICRON SPECTRAL INTERVAL.
C          3. COMPUTES UPWARD AND DOWNWARD FLUXES IN THE 0.68-4.0
C     MICRON SPECTRAL INTERVAL.
C
C     EXTERNALS.
C     ----------
C
C          *DEDD*, *SWTT*
C
C     REFERENCE.
C     ----------
C
C        SEE RADIATION'S PART OF THE ECMWF RESEARCH DEPARTMENT
C        DOCUMENTATION, AND FOUQUART AND BONNEL (1980)
C
C     AUTHOR.
C     -------
C        JEAN-JACQUES MORCRETTE  *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C        ORIGINAL : 88-12-15
C     ----------------------------------------------------------------
C
C      IMPLICIT LOGICAL (L)
C
*CALL ZPARB 
c      INCLUDE 'zparb.upd'
      REAL XL2, XL3, ALOS
      REAL FOXDWN(IRLST, IRLVP1)
C     ----------------------------------------------------------------
C
CDIR$ VFUNCTION ALOG, EXP
C     -----------------------
C
C
C     ----------------------------------------------------------------
C
C      WRITE(6,*) ' ***SUBROUTINE SW*** '
C
      DO 96 JL=1,NRLST
         DO 97 JK=1,NLEV
            PCLDSW(JL,JK) = ZCLDSW(JL,JK)
            POZ   (JL,JK) = ZOZ   (JL,JK)
            PPMB  (JL,JK) = ZPMB  (JL,JK)
            PTAVE (JL,JK) = ZTAVE (JL,JK)
            PFDOWN(JL,JK) = ZFSDWN(JL,JK)
            PFUP  (JL,JK) = ZFSUP (JL,JK)
            DO 98 II=1,2
               PCG   (JL,II,JK) = ZCG   (JL,II,JK)
               POMEGA(JL,II,JK) = ZOMEGA(JL,II,JK)
               PTAU  (JL,II,JK) = ZTAU  (JL,II,JK)
 98         CONTINUE
 97      CONTINUE
         DO 99 II=1,2
            PALBS(JL,II) = ZALBSU(JL,II)
 99      CONTINUE
         PPMB  (JL,NRLEV+1) = ZPMB  (JL,NRLEV+1)
         PFDOWN(JL,NRLEV+1) = ZFSDWN(JL,NRLEV+1)
         PFUP  (JL,NRLEV+1) = ZFSUP (JL,NRLEV+1)
 96   CONTINUE
C
C*         1.     COMPUTES AMOUNTS OF ABSORBERS
C                 -----------------------------
C
 100  CONTINUE
C
C
C*         1.1    INITIALIZES QUANTITIES
C                 ----------------------
C
 110  CONTINUE
C
      DO 111 JL = 1 , KDLON
         ZC1I(JL,KFLEV+1)=0.
         ZUD(JL,1,KFLEV+1)=0.
         ZUD(JL,2,KFLEV+1)=0.
         ZUD(JL,3,KFLEV+1)=0.
         ZFACT(JL)= PRMU0(JL) * PSCT * RDAYL(JL)
         ZRMU(JL)=SQRT(1224.* PRMU0(JL) * PRMU0(JL) + 1.) / 35.
         ZSEC(JL)=1./ZRMU(JL)
 111  CONTINUE
C
C
C*         1.2    OZONE FOR DOWNWARD LOOKING PATH
C                 -------------------------------
C
 120  CONTINUE
C
      DO 122 JK = 1 , KFLEV
         JKL = KFLEV+1 - JK
         JKLP1 = JKL + 1
         DO 121 JL = 1 , KDLON
            ZUD(JL,3,JKL) = ZUD(JL,3,JKLP1) + POZ(JL,JKL) * ZSEC(JL)
 121     CONTINUE
 122  CONTINUE
C
c          SCHUMANN-RUNGE and HERZBERG CONTINUUM (0.12 - 0.25 MICRON)
c          ----------------------------------------------------------
c
      ALOS=2.687E19
      DO 50 JL = 1 , KDLON
        FOXDWN(JL,1)=0.0
        IF (PRMU0(JL).GT.0.0001) THEN
          XL3=0.0
          DO 55 JK=1,KFLEV
            XL3=XL3+POZ(JL,JK)*ZSEC(JL)*ALOS
   55     CONTINUE
C
C set XL2O as mesopause value if too small
          XL2O=4.442E19*1.E-4*ZSEC(JL)
C
          DO 60 JK=1,KFLEV
            XL3=XL3-POZ(JL,JK)*ZSEC(JL)*ALOS
C            XL2=4.442E19*PPMB(JL,JK+1)*ZSEC(JL)*100.0
            XL2=MAX(4.442E19*PPMB(JL,JK+1)*ZSEC(JL)*100.0, XL2O)
C
            FOXDWN(JL,JK+1) =
     *      (  0.913 *EXP(-5.500E-24*XL2-6.215E-18*XL3)
     *    + 0.6308 *EXP(-1.342E-26*XL2-1.656E-18*XL3)
     *    + 0.4*2.30E-3*EXP(-3.159E-20*XL2-1.857E-06*XL2**0.33837)
     *    + 0.4*3.00E-3*EXP(-2.261E-24*XL2-1.917E-14*XL2**0.78903)
     *    + 0.3*3.50E-3*EXP(-5.399E-24*XL2-2.466E-17*XL2**0.92105)
     *    + 0.3*2.80E-3*EXP(-3.406E-24*XL2-7.787E-18*XL2**0.9269 )
     *    + 0.1*0.1466 *EXP(-2.915E-25*XL2**1.06135
     *                   - 1.866E-07*XL2**0.2895) )/SOLC
C
   60     CONTINUE
        ELSE
          DO 65 JK=KFLEV,1,-1
            FOXDWN(JL,JK)=0.0
   65     CONTINUE
        ENDIF
   50 CONTINUE
C
C*         1.3    OZONE FOR UPWARD LOOKING PATH AND OTHER ABSORBERS
C                 -------------------------------------------------
C
 130  CONTINUE
C
      DO 131 JL = 1 , KDLON
         ZUM(JL,1) = ZUD(JL,3,1)
         ZU1D(JL) = 0.
         ZU2D(JL) = 0.
         ZPSIG = PPSOL(JL) / 101325.
         ZP75(JL) = PPSOL(JL) * ZPSIG ** 0.75
         ZP90(JL) = PPSOL(JL) * ZPSIG ** 0.90
         ZO175(JL) = 1.0
         ZO190(JL) = 1.0
         ZSIGO(JL) = 1.0
 131  CONTINUE
C
      DO 133 JK = 1 , KFLEV
         JKP1 = JK + 1
         JKL = KFLEV+1 - JK
         DO 132 JL = 1 , KDLON
            ZUM(JL,JKP1) = ZUM(JL,JK) + POZ(JL,JK) * 1.66
            ZRT = 273.15 / PTAVE(JL,JK)
            ZWH2O = MAX(PWV(JL,JKL) , ZEPSCQ )
            ZSIGN(JL) = 100. * PPMB(JL,JKP1) / PPSOL(JL)
            ZDSIG(JL,JK) = ZSIGO(JL) - ZSIGN(JL)
            ZN175(JL) = ZSIGN(JL) ** 1.75
            ZN190(JL) = ZSIGN(JL) ** 1.90
            ZDSCO2 = ZO175(JL) - ZN175(JL)
            ZDSH2O = ZO190(JL) - ZN190(JL)
            ZUD(JL,1,JK) = ZP90(JL) * ZDSH2O*CH2O*ZWH2O  * ZRT **0.45
            ZUD(JL,2,JK) = ZP75(JL) * ZDSCO2*CCO2*PCARDI * ZRT **0.375
            ZU1D(JL) = ZU1D(JL) + ZUD(JL,1,JK)
            ZU2D(JL) = ZU2D(JL) + ZUD(JL,2,JK)
            ZSIGO(JL) = ZSIGN(JL)
            ZO175(JL) = ZN175(JL)
            ZO190(JL) = ZN190(JL)
 132     CONTINUE
 133  CONTINUE
C
C*         1.4    COMPUTES CLEAR-SKY GREY ABSORPTION COEFFICIENTS
C                 -----------------------------------------------
C
 140  CONTINUE
C
      DO 141 JL = 1 , KDLON
         ZU1D(JL) = ZU1D(JL) * ZSEC(JL)
         ZU2D(JL) = ZU2D(JL) * ZSEC(JL)
         ZW(JL) = ZU1D(JL)
 141  CONTINUE
C
      CALL SWTT ( 2, 1, KDLON,KRLST,KFLEV,ITASK )
C
      DO 142 JL = 1 , KDLON
         ZAKI(JL,1) = -LOG( ZR1 (JL)) / ZU1D(JL)
         ZW(JL) = ZU2D(JL)
 142  CONTINUE
C
      CALL SWTT ( 2, 2, KDLON,KRLST,KFLEV,ITASK )
C
      DO 143 JL = 1 , KDLON
         ZAKI(JL,2) = -LOG( ZR1 (JL)) / ZU2D(JL)
 143  CONTINUE
C
C     ---------------------------------------------------------------
C
C*         2.     FIRST SPECTRAL INTERVAL (0.25-0.68 MICRON)
C                 ----------------------- ------------------
C
 200  CONTINUE
C
      INU = 1
C
C*         2.1    OPTICAL THICKNESS FOR RAYLEIGH SCATTERING
C                 -----------------------------------------
C
 210  CONTINUE
C
      DO 211 JL = 1 , KDLON
          ZRAYL(JL) = CRAY(INU,1) + ZRMU(JL) * (CRAY(INU,2) + ZRMU(JL)
     S            * (CRAY(INU,3) + ZRMU(JL) * (CRAY(INU,4) + ZRMU(JL)
     S            * (CRAY(INU,5) + ZRMU(JL) *   CRAY(INU,6)       ))))
 211  CONTINUE
C
C*         2.2    OPTICAL PARAMETERS FOR AEROSOLS AND RAYLEIGH
C                 --------------------------------------------
C
 220  CONTINUE
C
      DO 225 JK = 1 , KFLEV
         DO 221 JL = 1 , KDLON
            ZCGAZ(JL,JK) = 0.
            ZPIZAZ(JL,JK) =  0.
            ZTAUAZ(JL,JK) = 0.
 221     CONTINUE
         DO 223 JAE=1,5
            DO 222 JL = 1 , KDLON
               ZTAUAZ(JL,JK)=ZTAUAZ(JL,JK)
     S                      +PAER(JL,JK,JAE)*TAUA(INU,JAE)
               ZPIZAZ(JL,JK)=ZPIZAZ(JL,JK)+PAER(JL,JK,JAE)
     S                   * TAUA(INU,JAE)*PIZA(INU,JAE)
               ZCGAZ(JL,JK) =  ZCGAZ(JL,JK) +PAER(JL,JK,JAE)
     S                   * TAUA(INU,JAE)*PIZA(INU,JAE)*CGA(INU,JAE)
 222        CONTINUE
 223     CONTINUE
C
         DO 224 JL = 1 , KDLON
            ZCGAZ(JL,JK) = CVMGT( 0.D0, ZCGAZ(JL,JK) / ZPIZAZ(JL,JK),
     S                            IAER.EQ.0)
             ZPIZAZ(JL,JK) = CVMGT( 1.D0, ZPIZAZ(JL,JK) / ZTAUAZ(JL,JK),
     S                            IAER.EQ.0)
            ZTRAY = ZRAYL(JL) * ZDSIG(JL,JK)
            ZRATIO = ZTRAY / (ZTRAY + ZTAUAZ(JL,JK))
            ZGAR = ZCGAZ(JL,JK)
            ZFF = ZGAR * ZGAR
            ZTAUAZ(JL,JK)=ZTRAY+ZTAUAZ(JL,JK)*(1.-ZPIZAZ(JL,JK)*ZFF)
            ZCGAZ(JL,JK) = ZGAR * (1. - ZRATIO) / (1. + ZGAR)
            ZPIZAZ(JL,JK) =ZRATIO+(1.-ZRATIO)*ZPIZAZ(JL,JK)*(1.-ZFF)
     S                    / (1. - ZPIZAZ(JL,JK) * ZFF)
 224     CONTINUE
 225  CONTINUE
C
C*         2.3    TOTAL EFFECTIVE CLOUDINESS ABOVE A GIVEN LEVEL
C                 ----------------------------------------------
C
 230  CONTINUE
C
      DO 231 JL = 1 , KDLON
         ZR23(JL) = 0.
         ZC1I(JL,KFLEV+1) = 0.
 231  CONTINUE
C
      DO 233 JK = 1 , KFLEV
         JKL = KFLEV+1 - JK
         JKLP1 = JKL + 1
         DO 232 JL = 1 , KDLON
            ZFAOA = 1.-ZPIZAZ(JL,JKL)*ZCGAZ(JL,JKL)*ZCGAZ(JL,JKL)
            ZFAOC = 1. - POMEGA(JL,INU,JKL) * PCG(JL,INU,JKL)
     S                                       * PCG(JL,INU,JKL)
            ZCORAE = ZFAOA * ZTAUAZ(JL,JKL) * ZSEC(JL)
            ZCORCD = ZFAOC * PTAU(JL,INU,JKL) * ZSEC(JL)
            ZR21(JL) = EXP(-ZCORAE   )
            ZR22(JL) = EXP(-ZCORCD   )
            ZSS1(JL) = PCLDSW(JL,JKL)*(1.0-ZR21(JL)*ZR22(JL))
     S               + (1.0-PCLDSW(JL,JKL))*(1.0-ZR21(JL))
            ZC1I(JL,JKL) = 1.0-(1.0-ZSS1(JL))*(1.0-ZC1I(JL,JKLP1))
 232     CONTINUE
 233  CONTINUE
C
C*         2.4    REFLECTIVITY/TRANSMISSIVITY FOR PURE SCATTERING
C                 -----------------------------------------------
C
 240  CONTINUE
C
      DO 241 JL = 1 , KDLON
         ZREFZ(JL,2,1) = PALBS(JL,INU)
         ZREFZ(JL,1,1) = PALBS(JL,INU)
 241  CONTINUE
C
      DO 246 JK = 2 , KFLEV+1
         JKM1 = JK-1
         DO 242 JL = 1 , KDLON
            ZRNEB(JL)= PCLDSW(JL,JKM1)
             ZRE1(JL)=0.
            ZTR1(JL)=0.
            ZRE2(JL)=0.
            ZTR2(JL)=0.
C
C*         2.4.1  EQUIVALENT ZENITH ANGLE
C                 -----------------------
C
 2410 CONTINUE
C
            ZMUE = (1.-ZC1I(JL,JK)) * ZSEC(JL)
     S            + ZC1I(JL,JK) * 1.66
            ZRMUE(JL,JK) = 1./ZMUE
C
C*         2.4.2  REFLECT./TRANSMISSIVITY DUE TO RAYLEIGH AND AEROSOLS
C                 ----------------------------------------------------
C
 2420 CONTINUE
C
            ZGAP = ZCGAZ(JL,JKM1)
            ZBMU0 = 0.5 - 0.75 * ZGAP / ZMUE
            ZWW = ZPIZAZ(JL,JKM1)
            ZTO = ZTAUAZ(JL,JKM1)
            ZDEN = 1. + (1. - ZWW + ZBMU0 * ZWW) * ZTO * ZMUE
     S           + (1-ZWW) * (1. - ZWW +2.*ZBMU0*ZWW)*ZTO*ZTO*ZMUE*ZMUE
            ZRAY1(JL,JKM1) = ZBMU0 * ZWW * ZTO * ZMUE / ZDEN
            ZTRA1(JL,JKM1) = 1. / ZDEN
C
            ZMU1 = 0.5
            ZBMU1 = 0.5 - 0.75 * ZGAP * ZMU1
            ZDEN1= 1. + (1. - ZWW + ZBMU1 * ZWW) * ZTO / ZMU1
     S           + (1-ZWW) * (1. - ZWW +2.*ZBMU1*ZWW)*ZTO*ZTO/ZMU1/ZMU1
            ZRAY2(JL,JKM1) = ZBMU1 * ZWW * ZTO / ZMU1 / ZDEN1
            ZTRA2(JL,JKM1) = 1. / ZDEN1
C
C*         2.4.3  EFFECT OF CLOUD LAYER
C                 ---------------------
C
 2430 CONTINUE
C
            PTAU(JL,INU,JKM1) = MAX( PTAU(JL,INU,JKM1) , ZEPSCT )
            ZW(JL) = POMEGA(JL,INU,JKM1)
            ZTO1(JL) = PTAU(JL,INU,JKM1)/ZW(JL)
     S            + ZTAUAZ(JL,JKM1)/ZPIZAZ(JL,JKM1)
            ZR21(JL) = PTAU(JL,INU,JKM1) + ZTAUAZ(JL,JKM1)
            ZR22(JL) = PTAU(JL,INU,JKM1) / ZR21(JL)
            ZGG(JL) = ZR22(JL) * PCG(JL,INU,JKM1)
     S              + (1. - ZR22(JL)) * ZCGAZ(JL,JKM1)
            ZW(JL) = ZR21(JL) / ZTO1(JL)
            ZREF(JL) = ZREFZ(JL,1,JKM1)
            ZRMUZ(JL) = ZRMUE(JL,JK)
 242     CONTINUE
C
         CALL DEDD ( KDLON,KRLST,KFLEV,ITASK )
C
         DO 245 JL = 1 , KDLON
C
             ZREFZ(JL,1,JK) = (1.-ZRNEB(JL)) * (ZRAY1(JL,JKM1)
     S                     + ZREFZ(JL,1,JKM1) * ZTRA1(JL,JKM1)
     S                     * ZTRA2(JL,JKM1)
     S                     /(1.-ZRAY2(JL,JKM1)*ZREFZ(JL,1,JKM1)))
     S                     + ZRNEB(JL) * ZRE2(JL)
C
            ZTR(JL,1,JKM1) = ZRNEB(JL) * ZTR2(JL) + (ZTRA1(JL,JKM1)
     S                     /(1.-ZRAY2(JL,JKM1)*ZREFZ(JL,1,JKM1)))
     S                     * (1.-ZRNEB(JL))
C
 245     CONTINUE
 246  CONTINUE
C
C*         2.5    REFLECT./TRANSMISSIVITY BETWEEN SURFACE AND LEVEL
C                 -------------------------------------------------
C
 250  CONTINUE
C
      JAJ = 2
      DO 251 JL = 1 , KDLON
         ZRJ(JL,JAJ,KFLEV+1) = 1.
         ZRK(JL,JAJ,KFLEV+1) = ZREFZ(JL, 1,KFLEV+1)
 251  CONTINUE
C
      DO 253 JK = 1 , KFLEV
         JKL = KFLEV+1 - JK
         JKLP1 = JKL + 1
         DO 252 JL = 1 , KDLON
            ZRE11= ZRJ(JL,JAJ,JKLP1) * ZTR(JL, 1,JKL)
            ZRJ(JL,JAJ,JKL) = ZRE11
            ZRK(JL,JAJ,JKL) = ZRE11 * ZREFZ(JL, 1,JKL)
 252     CONTINUE
 253  CONTINUE
C
C*         2.6    OZONE ABSORPTION AND FLUXES
C                 ---------------------------
C
 260  CONTINUE
C
      DO 264 JK = 1 , KFLEV+1
         JKL = KFLEV+1 - JK + 1
         DO 262 JL = 1 , KDLON
            ZW(JL) = ZUD(JL,3,JKL)
 262     CONTINUE
C
         CALL SWTT ( INU, 3, KDLON,KRLST,KFLEV,ITASK )
C
         DO 263 JL = 1 , KDLON
             ZFD(JL,JKL) = ZR1(JL) * ZRJ(JL,JAJ,JKL) * SUN(INU)
 263     CONTINUE
 264  CONTINUE
C
      DO 265 JL = 1 , KDLON
         ZFU(JL,1) = PALBS(JL,INU) * ZFD(JL,1)
 265  CONTINUE
C
      DO 268 JK = 1 , KFLEV+1
         DO 266 JL = 1 , KDLON
            ZW(JL) = ZUM(JL,JK)
 266     CONTINUE
C
         CALL SWTT ( INU, 3, KDLON,KRLST,KFLEV,ITASK )
C
         DO 267 JL = 1 , KDLON
            ZFU(JL,JK) = ZR1(JL) * ZRK(JL,JAJ,JK) * SUN(INU)
 267     CONTINUE
 268  CONTINUE
C
C
      IWHERE=268
C     PRINT 878,IWHERE
 878  FORMAT(1X,' LOOP ',I4,' SUCCESSFUL IN SW    INDEX=',I3)
C     ------------------------------------------------------------------
C
C*         3.     SECOND SPECTRAL INTERVAL (0.68-4.00 MICRON)
C                 ----------------------- -------------------
C
 300  CONTINUE
C
      INU = 2
C
C*         3.1    OPTICAL THICKNESS FOR RAYLEIGH SCATTERING
C                 -----------------------------------------
C
 310  CONTINUE
C
      DO 311 JL = 1 , KDLON
         ZRMUM1 = 1. - ZRMU(JL)
         ZRAYL(JL) = CRAY(INU,1) + ZRMUM1   * (CRAY(INU,2) + ZRMUM1
     S            * (CRAY(INU,3) + ZRMUM1   * (CRAY(INU,4) + ZRMUM1
     S            * (CRAY(INU,5) + ZRMUM1   *  CRAY(INU,6)       ))))
 311  CONTINUE
C
C*         3.2    OPTICAL PARAMETERS FOR AEROSOLS AND RAYLEIGH
C                 --------------------------------------------
C
 320  CONTINUE
C
      DO 325 JK = 1 , KFLEV
         DO 321 JL = 1 , KDLON
            ZCGAZ(JL,JK) = 0.
            ZPIZAZ(JL,JK) = 0.
            ZTAUAZ(JL,JK) = 0.
 321     CONTINUE
         DO 323 JAE=1,5
            DO 322 JL = 1 , KDLON
               ZTAUAZ(JL,JK) = ZTAUAZ(JL,JK) + PAER(JL,JK,JAE)
     S                       * TAUA(INU,JAE)
               ZPIZAZ(JL,JK) = ZPIZAZ(JL,JK) + PAER(JL,JK,JAE)
     S                       * TAUA(INU,JAE) * PIZA(INU,JAE)
               ZCGAZ(JL,JK) =  ZCGAZ(JL,JK) + PAER(JL,JK,JAE)
     S                   * TAUA(INU,JAE)*PIZA(INU,JAE)*CGA(INU,JAE)
 322        CONTINUE
 323     CONTINUE
         DO 324 JL = 1 , KDLON
            ZCGAZ(JL,JK) = CVMGT( 0.D0, ZCGAZ(JL,JK) / ZPIZAZ(JL,JK),
     S                            IAER.EQ.0)
            ZPIZAZ(JL,JK) = CVMGT( 1.D0, ZPIZAZ(JL,JK) / ZTAUAZ(JL,JK),
     S                             IAER.EQ.0)
            ZTRAY = ZRAYL(JL) * ZDSIG(JL,JK)
            ZRATIO = ZTRAY / (ZTRAY + ZTAUAZ(JL,JK))
            ZGAR = ZCGAZ(JL,JK)
            ZFF = ZGAR * ZGAR
            ZTAUAZ(JL,JK)=ZTRAY+ZTAUAZ(JL,JK)*(1.-ZPIZAZ(JL,JK)*ZFF)
            ZCGAZ(JL,JK) = ZGAR * (1. - ZRATIO) / (1. + ZGAR)
            ZPIZAZ(JL,JK) = ZRATIO+(1. - ZRATIO)*ZPIZAZ(JL,JK)*(1.-ZFF)
     S                    / (1. - ZPIZAZ(JL,JK) * ZFF)
 324     CONTINUE
 325  CONTINUE
C
C*         3.3    TOTAL EFFECTIVE CLOUDINESS ABOVE A GIVEN LEVEL
C                 ----------------------------------------------
C
 330  CONTINUE
C
      DO 331 JL = 1 , KDLON
         ZR23(JL) = 0.
         ZC1I(JL,KFLEV+1) = 0.
 331  CONTINUE
      DO 333 JK = 1 , KFLEV
         JKL = KFLEV+1 - JK
         JKLP1 = JKL + 1
         DO 332 JL = 1 , KDLON
            ZFAOA = 1.-ZPIZAZ(JL,JKL)*ZCGAZ(JL,JKL)*ZCGAZ(JL,JKL)
            ZFAOC = 1. - POMEGA(JL,INU,JKL) * PCG(JL,INU,JKL)
     S                                       * PCG(JL,INU,JKL)
            ZCORAE = ZFAOA * ZTAUAZ(JL,JKL) * ZSEC(JL)
            ZCORCD = ZFAOC * PTAU(JL,INU,JKL) * ZSEC(JL)
            ZR21(JL) = EXP(-ZCORAE   )
            ZR22(JL) = EXP(-ZCORCD   )
            ZSS1(JL) = PCLDSW(JL,JKL)*(1.0-ZR21(JL)*ZR22(JL))
     S               + (1.0-PCLDSW(JL,JKL))*(1.0-ZR21(JL))
            ZC1I(JL,JKL) = 1.0-(1.0-ZSS1(JL))*(1.0-ZC1I(JL,JKLP1))
 332     CONTINUE
 333  CONTINUE
C
C*         3.4    REFLECTIVITY/TRANSMISSIVITY FOR PURE SCATTERING
C                 -----------------------------------------------
C
 340  CONTINUE
C
      DO 341 JL = 1 , KDLON
         ZREFZ(JL,2,1) = PALBS(JL,INU)
         ZREFZ(JL,1,1) = PALBS(JL,INU)
 341  CONTINUE
C
      DO 346 JK = 2 , KFLEV+1
         JKM1 = JK - 1
         DO 342 JL = 1 , KDLON
            ZRNEB(JL) = PCLDSW(JL,JKM1)
            ZRE1(JL)=0.
            ZTR1(JL)=0.
            ZRE2(JL)=0.
            ZTR2(JL)=0.
C
C*         3.4.1  EQUIVALENT ZENITH ANGLE
C                 -----------------------
C
 3410 CONTINUE
C
            ZMUE = (1.-ZC1I(JL,JK)) * ZSEC(JL)
     S           + ZC1I(JL,JK) * 1.66
            ZRMUE(JL,JK) = 1./ZMUE
C
C*         3.4.2  REFLECT./TRANSMISSIVITY DUE TO RAYLEIGH AND AEROSOLS
C                 ----------------------------------------------------
C
 3420 CONTINUE
C
            ZGAP = ZCGAZ(JL,JKM1)
            ZBMU0 = 0.5 - 0.75 * ZGAP / ZMUE
            ZWW = ZPIZAZ(JL,JKM1)
            ZTO = ZTAUAZ(JL,JKM1)
            ZDEN = 1. + (1. - ZWW + ZBMU0 * ZWW) * ZTO * ZMUE
     S           + (1-ZWW)*(1.-ZWW+2.*ZBMU0*ZWW)*ZTO*ZTO*ZMUE*ZMUE
            ZRAY1(JL,JKM1) = ZBMU0 * ZWW * ZTO * ZMUE / ZDEN
            ZTRA1(JL,JKM1) = 1. / ZDEN
C
            ZMU1 = 0.5
            ZBMU1 = 0.5 - 0.75 * ZGAP * ZMU1
            ZDEN1= 1. + (1. - ZWW + ZBMU1 * ZWW) * ZTO / ZMU1
     S           + (1.-ZWW)*(1.-ZWW+2.*ZBMU1*ZWW)*ZTO*ZTO/ZMU1/ZMU1
            ZRAY2(JL,JKM1) = ZBMU1 * ZWW * ZTO / ZMU1 / ZDEN1
            ZTRA2(JL,JKM1) = 1. / ZDEN1
C
C*         3.4.3  EFFECT OF CLOUD LAYER
C                 ---------------------
C
 3430 CONTINUE
C
            PTAU(JL,INU,JKM1) = MAX( PTAU(JL,INU,JKM1) , ZEPSCT )
            ZW(JL) = POMEGA(JL,INU,JKM1)
            ZTO1(JL) = PTAU(JL,INU,JKM1)/ZW(JL)
     S               + ZTAUAZ(JL,JKM1)/ZPIZAZ(JL,JKM1)
            ZR21(JL) = PTAU(JL,INU,JKM1) + ZTAUAZ(JL,JKM1)
            ZR22(JL) = PTAU(JL,INU,JKM1) / ZR21(JL)
            ZGG(JL) = ZR22(JL) * PCG(JL,INU,JKM1)
     S              + (1. - ZR22(JL)) * ZCGAZ(JL,JKM1)
             ZW(JL) = ZR21(JL) / ZTO1(JL)
            ZREF(JL)=ZREFZ(JL,1,JKM1)
            ZRMUZ(JL)=ZRMUE(JL,JK)
 342     CONTINUE
C
      CALL DEDD ( KDLON,KRLST,KFLEV,ITASK )
C
         DO 345 JL = 1 , KDLON
C
            ZREFZ(JL,2,JK) = (1.-ZRNEB(JL)) * (ZRAY1(JL,JKM1)
     S                     + ZREFZ(JL,2,JKM1) * ZTRA1(JL,JKM1)
     S                     * ZTRA2(JL,JKM1) )
     S                     + ZRNEB(JL) * ZRE1(JL)
C
            ZTR(JL,2,JKM1) = ZRNEB(JL) * ZTR1(JL)
     S                     + ZTRA1(JL,JKM1) * (1.-ZRNEB(JL))
C
            ZREFZ(JL,1,JK) = (1.-ZRNEB(JL)) * (ZRAY1(JL,JKM1)
     S                     + ZREFZ(JL,1,JKM1) * ZTRA1(JL,JKM1)
     S                     * ZTRA2(JL,JKM1)
     S                     / (1.-ZRAY2(JL,JKM1)*ZREFZ(JL,1,JKM1)))
     S                     + ZRNEB(JL)*ZRE2(JL)
C
            ZTR(JL,1,JKM1) = ZRNEB(JL) * ZTR2(JL) + (ZTRA1(JL,JKM1)
     S                     / (1.-ZRAY2(JL,JKM1) * ZREFZ(JL,1,JKM1)))
     S                     * (1.-ZRNEB(JL))
C
 345     CONTINUE
 346  CONTINUE
      IWHERE=346
C     PRINT 878,IWHERE
C
C*         3.5    REFLECT./TRANSMISSIVITY BETWEEN SURFACE AND LEVEL
C                 -------------------------------------------------
C
 350  CONTINUE
C
      DO 354 JABS = 1 , 2
         DO 351 JL = 1 , KDLON
            ZRJ(JL,JABS,KFLEV+1) = 1.
            ZRK(JL,JABS,KFLEV+1) = ZREFZ(JL,JABS,KFLEV+1)
 351     CONTINUE
C
         DO 353 JK = 1 , KFLEV
            JKL = KFLEV+1 - JK
            JKLP1 = JKL + 1
            DO 352 JL = 1 , KDLON
               ZRE11 = ZRJ(JL,JABS,JKLP1) * ZTR(JL,JABS,JKL)
               ZRJ(JL,JABS,JKL) = ZRE11
               ZRK(JL,JABS,JKL) = ZRE11 * ZREFZ(JL,JABS,JKL)
 352        CONTINUE
 353     CONTINUE
 354  CONTINUE
      IWHERE=354
C     PRINT 878,IWHERE
C
C*         3.6    REFLECT./TRANSMISSIVITY WITH GREY ABSORPTION
C                 --------------------------------------------
C
 360  CONTINUE
C
      JN = 2
C
      DO 369 JABS=1,2
C
         DO 361 JL = 1 , KDLON
            ZREFZ(JL,2,1) = PALBS(JL,INU)
            ZREFZ(JL,1,1) = PALBS(JL,INU)
 361     CONTINUE
      IWHERE=361
C     PRINT 878,IWHERE,JABS
C
      DO 364 JK = 2 , KFLEV+1
         JKM1 = JK - 1
         DO 362 JL = 1 , KDLON
            ZRNEB(JL) = PCLDSW(JL,JKM1)
            ZAA = ZUD(JL,JABS,JKM1)
            ZRKI = ZAKI(JL,JABS)
            ZS(JL) = EXP(-ZRKI * ZAA * 1.66)
            ZG(JL) = EXP(-ZRKI * ZAA / ZRMUE(JL,JK))
            ZTR1(JL) = 0.
            ZRE1(JL) = 0.
            ZTR2(JL) = 0.
            ZRE2(JL) = 0.
C
C*         3.6.1  INTRODUCING CLOUD EFFECTS
C                 -------------------------
C
 3610 CONTINUE
C
            PTAU(JL,INU,JKM1) = MAX( PTAU(JL,INU,JKM1) , ZEPSCT )
            ZW(JL)= POMEGA(JL,INU,JKM1)
            ZTO1(JL) = PTAU(JL,INU,JKM1) / ZW(JL)
     S               + ZTAUAZ(JL,JKM1) / ZPIZAZ(JL,JKM1)
     S               + ZAA * ZRKI
            ZR21(JL) = PTAU(JL,INU,JKM1) + ZTAUAZ(JL,JKM1)
            ZR22(JL) = PTAU(JL,INU,JKM1) / ZR21(JL)
            ZGG(JL) = ZR22(JL) * PCG(JL,INU,JKM1)
     S              + (1. - ZR22(JL)) * ZCGAZ(JL,JKM1)
            ZW(JL) = ZR21(JL) / ZTO1(JL)
            ZREF(JL) = ZREFZ(JL,1,JKM1)
            ZRMUZ(JL) = ZRMUE(JL,JK)
 362     CONTINUE
      IWHERE=362
C     PRINT 878,IWHERE,JK
C
         CALL DEDD ( KDLON,KRLST,KFLEV,ITASK )
      IWHERE=3621
C     PRINT 878,IWHERE,JK
C
         DO 363 JL = 1 , KDLON
C
            ZREFZ(JL,2,JK) = (1.-ZRNEB(JL)) * (ZRAY1(JL,JKM1)
     S                     + ZREFZ(JL,2,JKM1) * ZTRA1(JL,JKM1)
     S                     * ZTRA2(JL,JKM1) ) * ZG(JL) * ZS(JL)
     S                     + ZRNEB(JL) * ZRE1(JL)
C
            ZTR(JL,2,JKM1)=ZRNEB(JL)*ZTR1(JL)
     S                    + (ZTRA1(JL,JKM1)) * ZG(JL) * (1.-ZRNEB(JL))
C
            ZREFZ(JL,1,JK)=(1.-ZRNEB(JL))*(ZRAY1(JL,JKM1)
     S                  +ZREFZ(JL,1,JKM1)*ZTRA1(JL,JKM1)*ZTRA2(JL,JKM1)
     S             /(1.-ZRAY2(JL,JKM1)*ZREFZ(JL,1,JKM1)))*ZG(JL)*ZS(JL)
     S             + ZRNEB(JL) * ZRE2(JL)
C
            ZTR(JL,1,JKM1)= ZRNEB(JL) * ZTR2(JL)
     S                    + (ZTRA1(JL,JKM1)/(1.-ZRAY2(JL,JKM1)
     S                    * ZREFZ(JL,1,JKM1)))
     S                    * ZG(JL) * (1. -ZRNEB(JL))
C
C
 363        CONTINUE
      IWHERE=363
C     PRINT 878,IWHERE,JK
 364     CONTINUE
      IWHERE=364
C     PRINT 878,IWHERE
C
C*         3.6.2  REFLECT./TRANSMISSIVITY BETWEEN SURFACE AND LEVEL
C                 -------------------------------------------------
C
 3620 CONTINUE
C
         DO 368 KREF=1,2
C
            JN = JN + 1
C
            DO 365 JL = 1 , KDLON
               ZRJ(JL,JN,KFLEV+1) = 1.
               ZRK(JL,JN,KFLEV+1) = ZREFZ(JL,KREF,KFLEV+1)
 365        CONTINUE
C
            DO 367 JK = 1 , KFLEV
               JKL = KFLEV+1 - JK
               JKLP1 = JKL + 1
               DO 366 JL = 1 , KDLON
                  ZRE11 = ZRJ(JL,JN,JKLP1) * ZTR(JL,KREF,JKL)
                  ZRJ(JL,JN,JKL) = ZRE11
                  ZRK(JL,JN,JKL) = ZRE11 * ZREFZ(JL,KREF,JKL)
 366           CONTINUE
 367        CONTINUE
 368     CONTINUE
 369  CONTINUE
C
C*         3.7    UPWARD (ZRK) AND DOWNWARD (ZRJ) PSEUDO-FLUXES
C                 ---------------------------------------------
C
 370  CONTINUE
C
      DO 374 JK = 1 , KFLEV+1
         DO 373 JAJ = 1 , 5 , 2
            JAJP = JAJ + 1
            DO 372 JL = 1 , KDLON
               ZRJ(JL,JAJ,JK)=        ZRJ(JL,JAJ,JK) - ZRJ(JL,JAJP,JK)
               ZRK(JL,JAJ,JK)=        ZRK(JL,JAJ,JK) - ZRK(JL,JAJP,JK)
               ZRJ(JL,JAJ,JK)= MAX( ZRJ(JL,JAJ,JK) , ZEELOG )
               ZRK(JL,JAJ,JK)= MAX( ZRK(JL,JAJ,JK) , ZEELOG )
 372        CONTINUE
 373     CONTINUE
 374  CONTINUE
C
      DO 377 JK = 1 , KFLEV+1
         DO 376 JAJ = 2 , 6 , 2
            DO 375 JL = 1 , KDLON
               ZRJ(JL,JAJ,JK)= MAX( ZRJ(JL,JAJ,JK) , ZEELOG )
               ZRK(JL,JAJ,JK)= MAX( ZRK(JL,JAJ,JK) , ZEELOG )
 375        CONTINUE
 376     CONTINUE
 377  CONTINUE
      IWHERE=377
C     PRINT 878,IWHERE
C
C*         3.8    EFFECTIVE ABSORBER AMOUNTS BY INVERSE LAPLACE
C                 ---------------------------------------------
C
 380  CONTINUE
C
      DO 387 JK = 1 , KFLEV+1
         JKKI = 1
         DO 385 JAJ = 1 , 2
            DO 384 JN = 1 , 2
               JN2J = JN + 2 * JAJ
               JKKP4 = JKKI + 4
C
C*         3.8.1  EFFECTIVE ABSORBER AMOUNTS
C                 ---------------------------------------------
C
 3810 CONTINUE
C
C
               DO 3811 JL = 1 , KDLON
                  ZW(JL) = LOG( ZRJ(JL,JN,JK) / ZRJ(JL,JN2J,JK))
     S                   / ZAKI(JL,JAJ)
 3811          CONTINUE
C
C*         3.8.2  TRANSMISSION FUNCTION
C                 ---------------------
C
 3820 CONTINUE
C
                CALL SWTT ( INU, JAJ, KDLON,KRLST,KFLEV,ITASK )
C
                DO 3821 JL = 1 , KDLON
                   ZRL(JL,JKKI) = ZR1(JL)
                   ZRUEF(JL,JKKI) = ZW(JL)
                   ZW(JL) = LOG( ZRK(JL,JN,JK) / ZRK(JL,JN2J,JK))
     S                    / ZAKI(JL,JAJ)
 3821           CONTINUE
C
                CALL SWTT ( INU, JAJ, KDLON,KRLST,KFLEV,ITASK )
C
                DO 383 JL = 1 , KDLON
                   ZRL(JL,JKKP4) = ZR1(JL)
                   ZRUEF(JL,JKKP4) = ZW(JL)
 383            CONTINUE
C
                JKKI=JKKI+1
 384         CONTINUE
 385      CONTINUE
      IWHERE=385
C     PRINT 878,IWHERE
C
C*         3.8.3  UPWARD AND DOWNWARD FLUXES WITH H2O AND UMG ABSORPTION
C                 ------------------------------------------------------
C
 3830 CONTINUE
C
          DO 386 JL = 1 , KDLON
             PFDOWN(JL,JK) = ZRJ(JL,1,JK) * ZRL(JL,1) * ZRL(JL,3)
     S                    + ZRJ(JL,2,JK) * ZRL(JL,2) * ZRL(JL,4)
             PFUP(JL,JK)   = ZRK(JL,1,JK) * ZRL(JL,5) * ZRL(JL,7)
     S                    + ZRK(JL,2,JK) * ZRL(JL,6) * ZRL(JL,8)
 386      CONTINUE
 387  CONTINUE
      IWHERE=387
C     PRINT 878,IWHERE
C
C*         3.9    INTRODUCTION OF OZONE ABSORPTION
C                 --------------------------------
C
 390  CONTINUE
C
      JABS=3
      DO 395 JK = 1 , KFLEV+1
         DO 392 JL = 1 , KDLON
            ZW(JL) = ZUD(JL,JABS,JK)
 392     CONTINUE
C
         CALL SWTT ( INU, JABS, KDLON,KRLST,KFLEV,ITASK )
C
         DO 393 JL = 1 , KDLON
            PFDOWN(JL,JK) = ZR1(JL) * PFDOWN(JL,JK) * SUN(INU)
            ZW(JL) = ZUM(JL,JK)
 393     CONTINUE
C
         CALL SWTT ( INU, JABS, KDLON,KRLST,KFLEV,ITASK )
C
         DO 394 JL = 1 , KDLON
            PFUP(JL,JK) = ZR1(JL) * PFUP(JL,JK) * SUN(INU)
 394     CONTINUE
 395  CONTINUE
      IWHERE=395
C     PRINT 878,IWHERE
C
C     ------------------------------------------------------------
C
C*         4.     NET TOTAL SHORTWAVE FLUXES
C                 --------------------------
C
C 400  CONTINUE
CZ      DO 402 JK = 1 , KFLEV+1
C         DO 401 JL = 1 , KDLON
C            PFUP(JL,JK)   = (PFUP(JL,JK)   + ZFU(JL,JK)) * ZFACT(JL)
C            PFDOWN(JL,JK) = (PFDOWN(JL,JK) + ZFD(JL,JK)) * ZFACT(JL)
C 401     CONTINUE
C 402  CONTINUE
C      IWHERE=402
C     PRINT 878,IWHERE
C
C
C
C*         4.     NET TOTAL SHORTWAVE FLUXES
C                 --------------------------
C
 400  CONTINUE
      DO 402 JK = 1 , KFLEV+1
         DO 401 JL = 1 , KDLON
c            IF(ITASK.EQ.1)THEN
c              PFUP(JL,JK)   = ZFU(JL,JK) * ZFACT(JL)
c              PFDOWN(JL,JK) = ZFD(JL,JK) * ZFACT(JL)
c            ELSE IF(ITASK.EQ.2)THEN
c              PFUP(JL,JK)   = PFUP(JL,JK) * ZFACT(JL)
c              PFDOWN(JL,JK) = PFDOWN(JL,JK) * ZFACT(JL)
c            ELSE IF(ITASK.EQ.3)THEN
              PFUP(JL,JK)   = (PFUP(JL,JK)   + ZFU(JL,JK)) * ZFACT(JL)
              PFDOWN(JL,JK) = (PFDOWN(JL,JK) + ZFD(JL,JK)
     *                      + FOXDWN(JL,JK)) * ZFACT(JL)
c            ENDIF
 401     CONTINUE
 402  CONTINUE
      IWHERE=402
C     PRINT 878,IWHERE
C
      DO 501 JL=1,NRLST
         DO 502 JK=1,NLEV
            ZCLDSW(JL,JK) = PCLDSW(JL,JK)
            ZOZ   (JL,JK) = POZ   (JL,JK)
            ZPMB  (JL,JK) = PPMB  (JL,JK)
            ZTAVE (JL,JK) = PTAVE (JL,JK)
            ZFSDWN(JL,JK) = PFDOWN(JL,JK)
            ZFSUP (JL,JK) = PFUP  (JL,JK)
            DO 503 II=1,2
               ZCG   (JL,II,JK) = PCG   (JL,II,JK)
               ZOMEGA(JL,II,JK) = POMEGA(JL,II,JK)
               ZTAU  (JL,II,JK) = PTAU  (JL,II,JK)
 503        CONTINUE
 502     CONTINUE
         DO 504 II=1,2
            ZALBSU(JL,II) = PALBS(JL,II)
 504     CONTINUE
         ZPMB  (JL,NRLEV+1) = PPMB  (JL,NRLEV+1)
         ZFSDWN(JL,NRLEV+1) = PFDOWN(JL,NRLEV+1)
         ZFSUP (JL,NRLEV+1) = PFUP  (JL,NRLEV+1)
 501  CONTINUE
C
      RETURN
      END
C
*DECK SWTT
      SUBROUTINE SWTT ( KNU, KA, KDLON, KRLST, KFLEV, ITASK)
C
C**** *SWTT* - COMPUTES THE SHORTWAVE TRANSMISSION FUNCTIONS
C
C     PURPOSE.
C     --------
C           THIS ROUTINE COMPUTES THE TRANSMISSION FUNCTIONS FOR ALL THE
C     ABSORBERS (H2O, UNIFORMLY MIXED GASES, AND O3) IN THE TWO SPECTRAL
C     INTERVALS.
C
C**   INTERFACE.
C     ----------
C          *SWTT* IS CALLED FROM *SW*.
C
C        EXPLICIT ARGUMENTS
C        --------------------
C KNU    :                     ; INDEX OF THE SPECTRAL INTERVAL
C KA     :                     ; INDEX OF THE ABSORBER
C
C        IMPLICIT ARGUMENTS
C        --------------------
C
C     ==== INPUTS ===
C PU     : (KDLON)             ; ABSORBER AMOUNT
C     ==== OUTPUTS ===
C PTR    : (KDLON)             ; TRANSMISSION FUNCTION
C
C     METHOD.
C     -------
C
C          TRANSMISSION FUNCTION ARE COMPUTED USING PADE APPROXIMANTS
C     AND HORNER'S ALGORITHM.
C
C     EXTERNALS.
C     ----------
C
C          NONE
C
C     REFERENCE.
C     ----------
C
C        SEE RADIATION'S PART OF THE MODEL'S DOCUMENTATION
C
C     AUTHOR.
C     -------
C        JEAN-JACQUES MORCRETTE  *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C        ORIGINAL : 88-12-15
C------------------------------------------------------------------
C      IMPLICIT LOGICAL (L)
C
*CALL ZPARB 
c      INCLUDE 'zparb.upd'
C
C-----------------------------------------------------------------
C
      DO 99 JL=1,NRLST
         PTR(JL) = ZR1(JL)
         PU (JL) = ZW (JL)
 99   CONTINUE
C
C-----------------------------------------------------------------
C
C*         1.      HORNER'S ALGORITHM TO COMPUTE TRANSMISSION FUNCTION
C
 100  CONTINUE
C
      DO 101 JL = 1 , KDLON
         PTR(JL) = APAD(KNU,KA,1) + PU(JL) * (APAD(KNU,KA,2) + PU(JL)
     S         * ( APAD(KNU,KA,3) + PU(JL) * (APAD(KNU,KA,4) + PU(JL)
     S         * ( APAD(KNU,KA,5) + PU(JL) * (APAD(KNU,KA,6) + PU(JL)
     S         * ( APAD(KNU,KA,7) ))))))
C
         ZR2(JL) = BPAD(KNU,KA,1) + PU(JL) * (BPAD(KNU,KA,2) + PU(JL)
     S         * ( BPAD(KNU,KA,3) + PU(JL) * (BPAD(KNU,KA,4) + PU(JL)
     S         * ( BPAD(KNU,KA,5) + PU(JL) * (BPAD(KNU,KA,6) + PU(JL)
     S         * ( BPAD(KNU,KA,7) ))))))
 101  CONTINUE
C
C*         2.      ADD THE BACKGROUND TRANSMISSION
C
 200  CONTINUE
C
      DO 201 JL = 1 , KDLON
         PTR(JL) = (PTR(JL) / ZR2(JL)) * (1. - D(KNU,KA)) + D(KNU,KA)
 201  CONTINUE
C
C
      DO 301 JL=1,NRLST
         ZR1(JL) = PTR(JL)
         ZW (JL) = PU (JL)
 301  CONTINUE
C
      RETURN
      END
C
*DECK SUAER
      SUBROUTINE SUAER
C
C**** *SUAER*   - INITIALIZE COMMON YOMAER
C
C     PURPOSE.
C     --------
C           INITIALIZE YOMAER, THE COMMON THAT CONTAINS THE
C           RADIATIVE CHARACTERISTICS OF THE AEROSOLS
C
C**   INTERFACE.
C     ----------
C        *CALL* *SUAER*
C
C        EXPLICIT ARGUMENTS
C        --------------------
C        NONE
C
C        IMPLICIT ARGUMENTS
C        --------------------
C        COMMON YOMAER
C
C     METHOD.
C     -------
C        SEE DOCUMENTATION
C
C     EXTERNALS.
C     ----------
C
C     REFERENCE.
C     ----------
C        ECMWF RESEARCH DEPARTMENT DOCUMENTATION
C
C     AUTHOR.
C     -------
C        JEAN-JACQUES MORCRETTE *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C        ORIGINAL : 88-02-15
C     ---------------------------------------------------------------
C
*CALL ZPARB 
c      INCLUDE 'zparb.upd'
C
C      -------------------------------------------------------------
C
C*       1.    SHORTWAVE COEFFICIENTS
C              ----------------------
C
      DATA ((TAUA(IN,JA),JA=1,5),IN=1,2) /
     S .730719, .912819, .725059, .745405, .682188 ,
     S .730719, .912819, .725059, .745405, .682188 /
      DATA ((PIZA(IN,JA),JA=1,5),IN=1,2) /
     S .872212, .982545, .623143, .944887, .997975 ,
     S .872212, .982545, .623143, .944887, .997975 /
      DATA ((CGA (IN,JA),JA=1,5),IN=1,2) /
     S .647596, .739002, .580845, .662657, .624246 ,
     S .647596, .739002, .580845, .662657, .624246 /
C      -------------------------------------------------------------
C
C*       2.    LONGWAVE COEFFICIENTS
C              ---------------------
C
      DATA CAER / .038520, .037196, .040532, .054934, .038520
     1          , .12613 , .18313 , .10357 , .064106, .126130
     2          , .012579, .013649, .018652, .025181, .012579
     3          , .011890, .016142, .021105, .028908, .011890
     4          , .013792, .026810, .052203, .066338, .013792 /
C
      RETURN
      END
*DECK SULW
        SUBROUTINE SULW
*CALL ZPARB 
c        INCLUDE "zparb.upd"
C
C Yet again we have to change the hard wired memory allocation for
C the radiation blocks.
C
C iii) Totally replace SULW
C
C      ---------------------------------------------------------
C
C*       1.    SET VALUES.
C              -----------
C
C      DATA NINT, NIPD, NIPD2, NTR, NTRA, NUA, NG1, NG1P1
C     S  /   6  ,    8,    16,  11,   19,  18,   2,     3  /
C
C-- REFERENCE TEMPERATURES FOR THE PLANCK FUNCTIONS ----- TSTAND
C          TEMPERATURE DEPENDENCE OF THE ABSORPTION ----- TREF
      DATA TSTAND,TREF / 250.0, 250.0 /
C
C-- ROOTS AND WEIGHTS FOR THE 2-POINT GAUSSIAN QUADRATURE
      DATA (RT1(IG1),IG1=1,2) / -0.577350269, +0.577350269 /
      DATA (WG1(IG1),IG1=1,2) /  1.0        ,  1.0         /
C
C-- TEMPERATURE STEP AND MAXIMUM INDEX FOR THE TRANSMISSIVITIES
      DATA TSTP/ 12.5 /,  MXIXT / 10 /
C
C-- REFERENCE TEMPERATURE FOR THE TRANSMISSIVITIES
      DATA TINTP / 187.5, 200., 212.5, 225., 237.5, 250.,
     S             262.5, 275., 287.5, 300., 312.5 /
C
C-- COEFFICIENTS OF THE POLYNOMIALS GIVING THE PLANCK FUNCTIONS
C
c      DATA (( XP(  J,K),J=1,6),       K=1,6) /
c     S 0.46430621E+02, 0.12928299E+03, 0.20732648E+03,
c     S 0.31398411E+03, 0.18373177E+03,-0.11412303E+03,
c     S 0.73604774E+02, 0.27887914E+03, 0.27076947E+03,
c     S-0.57322111E+02,-0.64742459E+02, 0.87238280E+02,
c     S 0.37050866E+02, 0.20498759E+03, 0.37558029E+03,
c     S 0.17401171E+03,-0.13350302E+03,-0.37651795E+02,
c     S 0.14930141E+02, 0.89161160E+02, 0.17793062E+03,
c     S 0.93433860E+02,-0.70646020E+02,-0.26373150E+02,
c     S 0.40386780E+02, 0.10855270E+03, 0.50755010E+02,
c     S-0.31496190E+02, 0.12791300E+02, 0.18017770E+01,
c     S 0.90811926E+01, 0.75073923E+02, 0.24654438E+03,
c     S 0.39332612E+03, 0.29385281E+03, 0.89107921E+02 /
c
C
C-- COEFFICIENTS OF THE POLYNOMIALS GIVING THE PLANCK FUNCTIONS 
C
c      DATA (( XP(  J,K),J=1,6),       K=1,6) /
c     * 0.46440912E+02, 0.12928373E+03, 0.20721063E+03,
c     * 0.31465410E+03, 0.18532753E+03,-0.12081117E+03,
c     * 0.73615150E+02, 0.27892884E+03, 0.27075955E+03,
c     *-0.57109865E+02,-0.63336785E+02, 0.85537973E+02,
c     * 0.37048742E+02, 0.20503052E+03, 0.37594173E+03,
c     * 0.17258251E+03,-0.13796046E+03,-0.23295511E+02,
c     * 0.14928616E+02, 0.89179092E+02, 0.17810427E+03,
c     * 0.92742845E+02,-0.72842724E+02,-0.19361244E+02,
c     * 0.40395378E+02, 0.10858185E+03, 0.50773924E+02,
c     *-0.31496423E+02, 0.12828971E+02, 0.16837921E+01,
c     * 0.90587573E+01, 0.74736166E+02, 0.24423578E+03,
c     * 0.38511585E+03, 0.26543217E+03, 0.26930073E+02 /
C
C-- COEFFICIENTS OF THE POLYNOMIALS GIVING THE PLANCK FUNCTIONS
C
      DATA (( XP(  J,K),J=1,6),       K=1,6) /
     * 0.46448212E+02, 0.12950083E+03, 0.20616074E+03,
     * 0.30440182E+03, 0.20088503E+03,-0.19180727E+02,
     * 0.73614893E+02, 0.27896120E+03, 0.27077036E+03,
     *-0.58840871E+02,-0.63235846E+02, 0.10332900E+03,
     * 0.37042681E+02, 0.20484389E+03, 0.37681519E+03,
     * 0.18143404E+03,-0.15093530E+03,-0.11119628E+03,
     * 0.14925225E+02, 0.89067009E+02, 0.17859760E+03,
     * 0.98102682E+02,-0.80224280E+02,-0.72734490E+02,
     * 0.40397268E+02, 0.10862521E+03, 0.50512477E+02,
     *-0.33484892E+02, 0.16614982E+02, 0.21239268E+02,
     * 0.90605881E+01, 0.74792389E+02, 0.24397449E+03,
     * 0.38243354E+03, 0.26928532E+03, 0.53743993E+02 /
C
C
C-- COEFFICIENTS OF THE POLYNOMIALS GIVING THE TEMPERATURE DEPENDENCE OF
C     THE ABSORPTION
C--------H2O
      DATA (AT(1,IR),IR=1,3)/ 
     S  2.34995778846E-03, -2.76713384294E-04,  1.16480338097E-06/
      DATA (BT(1,IR),IR=1,3)/ 
     S -1.58165952771E-05, -2.39167473702E-05,  1.80148335147E-07/
      DATA (AT(2,IR),IR=1,3)/ 
     S  2.52595953359E-02, -6.72079319635E-04,  5.74103783815E-06/
      DATA (BT(2,IR),IR=1,3)/ 
     S -8.26196996910E-05, -1.05514645370E-05,  7.02984234993E-08/
      DATA (AT(3,IR),IR=1,3)/ 
     S  3.55525229363E-02, -7.94992134865E-04,  6.64848193374E-06/
      DATA (BT(3,IR),IR=1,3)/ 
     S -9.71124908025E-05, -2.95218285256E-06,  2.23374952695E-08/
      DATA (AT(4,IR),IR=1,3)/ 
     S  5.47044522259E-02, -1.19105877667E-03,  9.74676865100E-06/
      DATA (BT(4,IR),IR=1,3)/ 
     S -2.10153035427E-04, -6.93815560155E-06,  5.50381928956E-08/
      DATA (AT(5,IR),IR=1,3) /
     S  1.88063066950E-02, -1.11250846983E-04,  1.66977895508E-06/
      DATA (BT(5,IR),IR=1,3) /
     S -5.72683436311E-05, -2.48448308453E-05,  1.88450887138E-07/
      DATA (AT(6,IR),IR=1,3) /
     S  1.29526952131E-02, -3.81424455035E-04,  3.07118138204E-06/
      DATA (BT(6,IR),IR=1,3) /
     S -4.20789327775E-05, -1.25055291318E-05,  8.58839668934E-08/
C--1250-1450
      DATA (AT(7,IR),IR=1,3)/ 
     S  1.31749957617E-02, -5.90100320028E-05,  6.25471011857E-07/
      DATA (BT(7,IR),IR=1,3)/ 
     S -4.30693119882E-05, -3.14712185336E-07,  3.69886377880E-09/
C--800-970
      DATA (AT(8,IR),IR=1,3)/ 
     S  4.88895361215E-02, -1.10191335476E-03,  9.12442493536E-06/
      DATA (BT(8,IR),IR=1,3)/ 
     S -1.07667174073E-04,  2.47171336712E-06, -1.96942567968E-08/
C--650-800
      DATA (AT(9,IR),IR=1,3) /
     S  3.53337502532E-02, -9.07920050431E-04,  7.57030599278E-06/
      DATA (BT(9,IR),IR=1,3) /
     S -1.09062411235E-04, -9.99616246128E-07,  7.90293124935E-09/
C-------CO2
      DATA (AT(10,IR),IR=1,3) /
     S 0.250073E-03,0.455875E-03,0.109242E-03 /
      DATA (BT(10,IR),IR=1,3) /
     S 0.199846E-05,-.216313E-05,0.175991E-06 /
      DATA (AT(11,IR),IR=1,3) /
     S 0.307423E-01,0.110879E-02,-.322172E-03 /
      DATA (BT(11,IR),IR=1,3) /
     S-0.108482E-03,0.258096E-05,-.814575E-06 /
C
C--  COEFFICIENTS FOR OZONE ABSORPTION
C
      DATA O1H,O2H,PIALF0,(OCT(IT),IT=1,4) /
     S 2230., 100., 2., -.326E-03, -.102E-05, .137E-02, -.535E-05 /
C
C--  COEFFICIENTS FOR H2O E-TYPE CONTINUUM ABSORPTION
C
C      DATA CNTNU /   70.0, 26.77, 8.45, 5.78  /
C
C--------------------------------------------------------
C  PADE APPROXIMANTS FOR THE TRANSMISSIVITIES
C  ==========================================
C-- WATER VAPOR -- INT.1 -- 0- 350 + 1450- 1880 CM-1 --------
C
C----- INTERVAL = 1 ----- T =    187.50
C
      DATA (GA( 1, 1,JC),JC=1,3) /
     S 0.25000224E-02, 0.18701843E-02, 0.00000000E+00/
      DATA (GB( 1, 1,JC),JC=1,3) /
     S 0.25000224E-02, 0.61593968E-01, 0.10000000E+01/
      DATA (GA( 1, 2,JC),JC=1,3) /
     S 0.34131105E-02, 0.55153039E-03, 0.00000000E+00/
      DATA (GB( 1, 2,JC),JC=1,3) /
     S 0.34131105E-02, 0.73127832E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    200.00
C
      DATA (GA( 2, 1,JC),JC=1,3) /
     S 0.26344616E-02, 0.16894582E-02, 0.00000000E+00/
      DATA (GB( 2, 1,JC),JC=1,3) /
     S 0.26344616E-02, 0.63633167E-01, 0.10000000E+01/
      DATA (GA( 2, 2,JC),JC=1,3) /
     S 0.36120928E-02, 0.44720171E-03, 0.00000000E+00/
      DATA (GB( 2, 2,JC),JC=1,3) /
     S 0.36120928E-02, 0.75062171E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    212.50
C
      DATA (GA( 3, 1,JC),JC=1,3) /
     S 0.27743234E-02, 0.15245487E-02, 0.00000000E+00/
      DATA (GB( 3, 1,JC),JC=1,3) /
     S 0.27743234E-02, 0.65651733E-01, 0.10000000E+01/
      DATA (GA( 3, 2,JC),JC=1,3) /
     S 0.38453121E-02, 0.42382776E-03, 0.00000000E+00/
      DATA (GB( 3, 2,JC),JC=1,3) /
     S 0.38453121E-02, 0.77088315E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    225.00
C
      DATA (GA( 4, 1,JC),JC=1,3) /
     S 0.29265376E-02, 0.13719090E-02, 0.00000000E+00/
      DATA (GB( 4, 1,JC),JC=1,3) /
     S 0.29265376E-02, 0.67739270E-01, 0.10000000E+01/
      DATA (GA( 4, 2,JC),JC=1,3) /
     S 0.41211460E-02, 0.42401401E-03, 0.00000000E+00/
      DATA (GB( 4, 2,JC),JC=1,3) /
     S 0.41211460E-02, 0.79241307E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    237.50
C
      DATA (GA( 5, 1,JC),JC=1,3) /
     S 0.30979053E-02, 0.12223308E-02, 0.00000000E+00/
      DATA (GB( 5, 1,JC),JC=1,3) /
     S 0.30979053E-02, 0.69968340E-01, 0.10000000E+01/
      DATA (GA( 5, 2,JC),JC=1,3) /
     S 0.43966504E-02, 0.67198946E-03, 0.00000000E+00/
      DATA (GB( 5, 2,JC),JC=1,3) /
     S 0.43966504E-02, 0.80947310E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    250.00
C
      DATA (GA( 6, 1,JC),JC=1,3) /
     S 0.32813184E-02, 0.11573810E-02, 0.00000000E+00/
      DATA (GB( 6, 1,JC),JC=1,3) /
     S 0.32813184E-02, 0.72183750E-01, 0.10000000E+01/
      DATA (GA( 6, 2,JC),JC=1,3) /
     S 0.46792452E-02, 0.99691013E-03, 0.00000000E+00/
      DATA (GB( 6, 2,JC),JC=1,3) /
     S 0.46792452E-02, 0.82404099E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    262.50
C
      DATA (GA( 7, 1,JC),JC=1,3) /
     S 0.34869261E-02, 0.11110812E-02, 0.00000000E+00/
      DATA (GB( 7, 1,JC),JC=1,3) /
     S 0.34869261E-02, 0.74507149E-01, 0.10000000E+01/
      DATA (GA( 7, 2,JC),JC=1,3) /
     S 0.49313571E-02, 0.15144806E-02, 0.00000000E+00/
      DATA (GB( 7, 2,JC),JC=1,3) /
     S 0.49313571E-02, 0.83301687E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    275.00
C
      DATA (GA( 8, 1,JC),JC=1,3) /
     S 0.37169013E-02, 0.10586459E-02, 0.00000000E+00/
      DATA (GB( 8, 1,JC),JC=1,3) /
     S 0.37169013E-02, 0.76930655E-01, 0.10000000E+01/
      DATA (GA( 8, 2,JC),JC=1,3) /
     S 0.51606177E-02, 0.20799953E-02, 0.00000000E+00/
      DATA (GB( 8, 2,JC),JC=1,3) /
     S 0.51606177E-02, 0.83904484E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    287.50
C
      DATA (GA( 9, 1,JC),JC=1,3) /
     S 0.39482491E-02, 0.11280761E-02, 0.00000000E+00/
      DATA (GB( 9, 1,JC),JC=1,3) /
     S 0.39482491E-02, 0.79096624E-01, 0.10000000E+01/
      DATA (GA( 9, 2,JC),JC=1,3) /
     S 0.53608496E-02, 0.26755638E-02, 0.00000000E+00/
      DATA (GB( 9, 2,JC),JC=1,3) /
     S 0.53608496E-02, 0.84271466E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    300.00
C
      DATA (GA(10, 1,JC),JC=1,3) /
     S 0.41849825E-02, 0.12500965E-02, 0.00000000E+00/
      DATA (GB(10, 1,JC),JC=1,3) /
     S 0.41849825E-02, 0.81087000E-01, 0.10000000E+01/
      DATA (GA(10, 2,JC),JC=1,3) /
     S 0.55372909E-02, 0.32512851E-02, 0.00000000E+00/
      DATA (GB(10, 2,JC),JC=1,3) /
     S 0.55372909E-02, 0.84546756E-01, 0.10000000E+01/
C
C----- INTERVAL = 1 ----- T =    312.50
C
      DATA (GA(11, 1,JC),JC=1,3) /
     S 0.44204714E-02, 0.14195697E-02, 0.00000000E+00/
      DATA (GB(11, 1,JC),JC=1,3) /
     S 0.44204714E-02, 0.82840020E-01, 0.10000000E+01/
      DATA (GA(11, 2,JC),JC=1,3) /
     S 0.56935299E-02, 0.38054592E-02, 0.00000000E+00/
      DATA (GB(11, 2,JC),JC=1,3) /
     S 0.56935299E-02, 0.84847060E-01, 0.10000000E+01/
C
C-- WATER VAPOR --- 500 - 800 CM-1
C
C
C----- INTERVAL = 2 ----- T =    187.50
C
      DATA (GA( 1, 3,JC),JC=1,3) /
     S 0.15053751E+01, 0.45361026E+00, 0.00000000E+00/
      DATA (GB( 1, 3,JC),JC=1,3) /
     S 0.15053751E+01, 0.15789584E+01, 0.10000000E+01/
      DATA (GA( 1, 4,JC),JC=1,3) /
     S 0.18920793E+01, 0.53457554E+00, 0.00000000E+00/
      DATA (GB( 1, 4,JC),JC=1,3) /
     S 0.18920793E+01, 0.18418304E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    200.00
C
      DATA (GA( 2, 3,JC),JC=1,3) /
     S 0.15977537E+01, 0.48084314E+00, 0.00000000E+00/
      DATA (GB( 2, 3,JC),JC=1,3) /
     S 0.15977537E+01, 0.16476468E+01, 0.10000000E+01/
      DATA (GA( 2, 4,JC),JC=1,3) /
     S 0.20435568E+01, 0.55893717E+00, 0.00000000E+00/
      DATA (GB( 2, 4,JC),JC=1,3) /
     S 0.20435568E+01, 0.19386656E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    212.50
C
      DATA (GA( 3, 3,JC),JC=1,3) /
     S 0.16913237E+01, 0.50454053E+00, 0.00000000E+00/
      DATA (GB( 3, 3,JC),JC=1,3) /
     S 0.16913237E+01, 0.17144170E+01, 0.10000000E+01/
      DATA (GA( 3, 4,JC),JC=1,3) /
     S 0.21978186E+01, 0.57905459E+00, 0.00000000E+00/
      DATA (GB( 3, 4,JC),JC=1,3) /
     S 0.21978186E+01, 0.20334044E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    225.00
C
      DATA (GA( 4, 3,JC),JC=1,3) /
     S 0.17856476E+01, 0.52514194E+00, 0.00000000E+00/
      DATA (GB( 4, 3,JC),JC=1,3) /
     S 0.17856476E+01, 0.17792823E+01, 0.10000000E+01/
      DATA (GA( 4, 4,JC),JC=1,3) /
     S 0.23533355E+01, 0.59558005E+00, 0.00000000E+00/
      DATA (GB( 4, 4,JC),JC=1,3) /
     S 0.23533355E+01, 0.21256517E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    237.50
C
      DATA (GA( 5, 3,JC),JC=1,3) /
     S 0.18802811E+01, 0.54304459E+00, 0.00000000E+00/
      DATA (GB( 5, 3,JC),JC=1,3) /
     S 0.18802811E+01, 0.18422412E+01, 0.10000000E+01/
      DATA (GA( 5, 4,JC),JC=1,3) /
     S 0.25086500E+01, 0.60908958E+00, 0.00000000E+00/
      DATA (GB( 5, 4,JC),JC=1,3) /
     S 0.25086500E+01, 0.22150389E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    250.00
C
      DATA (GA( 6, 3,JC),JC=1,3) /
     S 0.19747954E+01, 0.55860186E+00, 0.00000000E+00/
      DATA (GB( 6, 3,JC),JC=1,3) /
     S 0.19747954E+01, 0.19032856E+01, 0.10000000E+01/
      DATA (GA( 6, 4,JC),JC=1,3) /
     S 0.26624407E+01, 0.62008317E+00, 0.00000000E+00/
      DATA (GB( 6, 4,JC),JC=1,3) /
     S 0.26624407E+01, 0.23012482E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    262.50
C
      DATA (GA( 7, 3,JC),JC=1,3) /
     S 0.20687911E+01, 0.57212480E+00, 0.00000000E+00/
      DATA (GB( 7, 3,JC),JC=1,3) /
     S 0.20687911E+01, 0.19624068E+01, 0.10000000E+01/
      DATA (GA( 7, 4,JC),JC=1,3) /
     S 0.28135635E+01, 0.62898892E+00, 0.00000000E+00/
      DATA (GB( 7, 4,JC),JC=1,3) /
     S 0.28135635E+01, 0.23840281E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    275.00
C
      DATA (GA( 8, 3,JC),JC=1,3) /
     S 0.21619083E+01, 0.58388519E+00, 0.00000000E+00/
      DATA (GB( 8, 3,JC),JC=1,3) /
     S 0.21619083E+01, 0.20195993E+01, 0.10000000E+01/
      DATA (GA( 8, 4,JC),JC=1,3) /
     S 0.29610693E+01, 0.63616954E+00, 0.00000000E+00/
      DATA (GB( 8, 4,JC),JC=1,3) /
     S 0.29610693E+01, 0.24631984E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    287.50
C
      DATA (GA( 9, 3,JC),JC=1,3) /
     S 0.22538305E+01, 0.59411934E+00, 0.00000000E+00/
      DATA (GB( 9, 3,JC),JC=1,3) /
     S 0.22538305E+01, 0.20748638E+01, 0.10000000E+01/
      DATA (GA( 9, 4,JC),JC=1,3) /
     S 0.31042087E+01, 0.64192944E+00, 0.00000000E+00/
      DATA (GB( 9, 4,JC),JC=1,3) /
     S 0.31042087E+01, 0.25386513E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    300.00
C
      DATA (GA(10, 3,JC),JC=1,3) /
     S 0.23442867E+01, 0.60303198E+00, 0.00000000E+00/
      DATA (GB(10, 3,JC),JC=1,3) /
     S 0.23442867E+01, 0.21282086E+01, 0.10000000E+01/
      DATA (GA(10, 4,JC),JC=1,3) /
     S 0.32424163E+01, 0.64652280E+00, 0.00000000E+00/
      DATA (GB(10, 4,JC),JC=1,3) /
     S 0.32424163E+01, 0.26103424E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =    312.50
C
      DATA (GA(11, 3,JC),JC=1,3) /
     S 0.24330507E+01, 0.61080006E+00, 0.00000000E+00/
      DATA (GB(11, 3,JC),JC=1,3) /
     S 0.24330507E+01, 0.21796505E+01, 0.10000000E+01/
      DATA (GA(11, 4,JC),JC=1,3) /
     S 0.33752974E+01, 0.65016031E+00, 0.00000000E+00/
      DATA (GB(11, 4,JC),JC=1,3) /
     S 0.33752974E+01, 0.26782849E+01, 0.10000000E+01/
C
C
C- WATER VAPOR - INT. 3 -- 800-970 + 1110-1250 CM-1 
C
C
C----- INTERVAL = 3 ----- T =    187.50
C
      DATA (GA( 1, 7,JC),JC=1,3) /
     S 0.14334746E+02, 0.11649992E+02, 0.00000000E+00/
      DATA (GB( 1, 7,JC),JC=1,3) /
     S 0.14334746E+02, 0.12059903E+02, 0.10000000E+01/
      DATA (GA( 1, 8,JC),JC=1,3) /
     S 0.11596027E+02, 0.11051587E+02, 0.00000000E+00/
      DATA (GB( 1, 8,JC),JC=1,3) /
     S 0.11596027E+02, 0.11390898E+02, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    200.00
C
      DATA (GA( 2, 7,JC),JC=1,3) /
     S 0.12924521E+02, 0.11365906E+02, 0.00000000E+00/
      DATA (GB( 2, 7,JC),JC=1,3) /
     S 0.12924521E+02, 0.11739611E+02, 0.10000000E+01/
      DATA (GA( 2, 8,JC),JC=1,3) /
     S 0.10445365E+02, 0.10682810E+02, 0.00000000E+00/
      DATA (GB( 2, 8,JC),JC=1,3) /
     S 0.10445365E+02, 0.10993927E+02, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    212.50
C
      DATA (GA( 3, 7,JC),JC=1,3) /
     S 0.11774060E+02, 0.11080218E+02, 0.00000000E+00/
      DATA (GB( 3, 7,JC),JC=1,3) /
     S 0.11774060E+02, 0.11424740E+02, 0.10000000E+01/
      DATA (GA( 3, 8,JC),JC=1,3) /
     S 0.95571840E+01, 0.10328058E+02, 0.00000000E+00/
      DATA (GB( 3, 8,JC),JC=1,3) /
     S 0.95571840E+01, 0.10618404E+02, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    225.00
C
      DATA (GA( 4, 7,JC),JC=1,3) /
     S 0.10839967E+02, 0.10800025E+02, 0.00000000E+00/
      DATA (GB( 4, 7,JC),JC=1,3) /
     S 0.10839967E+02, 0.11121318E+02, 0.10000000E+01/
      DATA (GA( 4, 8,JC),JC=1,3) /
     S 0.88719975E+01, 0.99930444E+01, 0.00000000E+00/
      DATA (GB( 4, 8,JC),JC=1,3) /
     S 0.88719975E+01, 0.10268470E+02, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    237.50
C
      DATA (GA( 5, 7,JC),JC=1,3) /
     S 0.10082160E+02, 0.10529965E+02, 0.00000000E+00/
      DATA (GB( 5, 7,JC),JC=1,3) /
     S 0.10082160E+02, 0.10832955E+02, 0.10000000E+01/
      DATA (GA( 5, 8,JC),JC=1,3) /
     S 0.83425443E+01, 0.96805504E+01, 0.00000000E+00/
      DATA (GB( 5, 8,JC),JC=1,3) /
     S 0.83425443E+01, 0.99456138E+01, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    250.00
C
      DATA (GA( 6, 7,JC),JC=1,3) /
     S 0.94663507E+01, 0.10272823E+02, 0.00000000E+00/
      DATA (GB( 6, 7,JC),JC=1,3) /
     S 0.94663507E+01, 0.10561525E+02, 0.10000000E+01/
      DATA (GA( 6, 8,JC),JC=1,3) /
     S 0.79324712E+01, 0.93914198E+01, 0.00000000E+00/
      DATA (GB( 6, 8,JC),JC=1,3) /
     S 0.79324712E+01, 0.96496576E+01, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    262.50
C
      DATA (GA( 7, 7,JC),JC=1,3) /
     S 0.89643981E+01, 0.10030051E+02, 0.00000000E+00/
      DATA (GB( 7, 7,JC),JC=1,3) /
     S 0.89643981E+01, 0.10307714E+02, 0.10000000E+01/
      DATA (GA( 7, 8,JC),JC=1,3) /
     S 0.76141974E+01, 0.91252916E+01, 0.00000000E+00/
      DATA (GB( 7, 8,JC),JC=1,3) /
     S 0.76141974E+01, 0.93794463E+01, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    275.00
C
      DATA (GA( 8, 7,JC),JC=1,3) /
     S 0.85537123E+01, 0.98021793E+01, 0.00000000E+00/
      DATA (GB( 8, 7,JC),JC=1,3) /
     S 0.85537123E+01, 0.10071426E+02, 0.10000000E+01/
      DATA (GA( 8, 8,JC),JC=1,3) /
     S 0.73668729E+01, 0.88811055E+01, 0.00000000E+00/
      DATA (GB( 8, 8,JC),JC=1,3) /
     S 0.73668729E+01, 0.91333042E+01, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    287.50
C
      DATA (GA( 9, 7,JC),JC=1,3) /
     S 0.82163567E+01, 0.95891296E+01, 0.00000000E+00/
      DATA (GB( 9, 7,JC),JC=1,3) /
     S 0.82163567E+01, 0.98520772E+01, 0.10000000E+01/
      DATA (GA( 9, 8,JC),JC=1,3) /
     S 0.71747025E+01, 0.86574340E+01, 0.00000000E+00/
      DATA (GB( 9, 8,JC),JC=1,3) /
     S 0.71747025E+01, 0.89093260E+01, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    300.00
C
      DATA (GA(10, 7,JC),JC=1,3) /
     S 0.79381525E+01, 0.93904375E+01, 0.00000000E+00/
      DATA (GB(10, 7,JC),JC=1,3) /
     S 0.79381525E+01, 0.96487960E+01, 0.10000000E+01/
      DATA (GA(10, 8,JC),JC=1,3) /
     S 0.70256573E+01, 0.84526940E+01, 0.00000000E+00/
      DATA (GB(10, 8,JC),JC=1,3) /
     S 0.70256573E+01, 0.87055560E+01, 0.10000000E+01/
C
C----- INTERVAL = 3 ----- T =    312.50
C
      DATA (GA(11, 7,JC),JC=1,3) /
     S 0.77078849E+01, 0.92054098E+01, 0.00000000E+00/
      DATA (GB(11, 7,JC),JC=1,3) /
     S 0.77078849E+01, 0.94605633E+01, 0.10000000E+01/
      DATA (GA(11, 8,JC),JC=1,3) /
     S 0.69105145E+01, 0.82652769E+01, 0.00000000E+00/
      DATA (GB(11, 8,JC),JC=1,3) /
     S 0.69105145E+01, 0.85200926E+01, 0.10000000E+01/
C
C-- WATER VAPOR --- 970 -1110 CM-1
C
C
C----- INTERVAL = 4 ----- T =    187.500
C
      DATA (GA( 1, 9,JC),JC=1,3) /
     S 0.94868790E+02, 0.17437229E+02, 0.00000000E+00/
      DATA (GB( 1, 9,JC),JC=1,3) /
     S 0.94868790E+02, 0.18796329E+02, 0.10000000E+01/
      DATA (GA( 1,10,JC),JC=1,3) /
     S 0.95419113E+02, 0.17234291E+02, 0.00000000E+00/
      DATA (GB( 1,10,JC),JC=1,3) /
     S 0.95419113E+02, 0.18632164E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    200.000
C
      DATA (GA( 2, 9,JC),JC=1,3) /
     S 0.95143959E+02, 0.17335575E+02, 0.00000000E+00/
      DATA (GB( 2, 9,JC),JC=1,3) /
     S 0.95143959E+02, 0.18714111E+02, 0.10000000E+01/
      DATA (GA( 2,10,JC),JC=1,3) /
     S 0.95675995E+02, 0.17129913E+02, 0.00000000E+00/
      DATA (GB( 2,10,JC),JC=1,3) /
     S 0.95675995E+02, 0.18547161E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    212.500
C
      DATA (GA( 3, 9,JC),JC=1,3) /
     S 0.95363609E+02, 0.17245064E+02, 0.00000000E+00/
      DATA (GB( 3, 9,JC),JC=1,3) /
     S 0.95363609E+02, 0.18640270E+02, 0.10000000E+01/
      DATA (GA( 3,10,JC),JC=1,3) /
     S 0.95911537E+02, 0.17036016E+02, 0.00000000E+00/
      DATA (GB( 3,10,JC),JC=1,3) /
     S 0.95911537E+02, 0.18470697E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    225.000
C
      DATA (GA( 4, 9,JC),JC=1,3) /
     S 0.95551239E+02, 0.17163479E+02, 0.00000000E+00/
      DATA (GB( 4, 9,JC),JC=1,3) /
     S 0.95551239E+02, 0.18573282E+02, 0.10000000E+01/
      DATA (GA( 4,10,JC),JC=1,3) /
     S 0.96128136E+02, 0.16952074E+02, 0.00000000E+00/
      DATA (GB( 4,10,JC),JC=1,3) /
     S 0.96128136E+02, 0.18402708E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    237.500
C
      DATA (GA( 5, 9,JC),JC=1,3) /
     S 0.95763680E+02, 0.17089018E+02, 0.00000000E+00/
      DATA (GB( 5, 9,JC),JC=1,3) /
     S 0.95763680E+02, 0.18513403E+02, 0.10000000E+01/
      DATA (GA( 5,10,JC),JC=1,3) /
     S 0.96294296E+02, 0.16877150E+02, 0.00000000E+00/
      DATA (GB( 5,10,JC),JC=1,3) /
     S 0.96294296E+02, 0.18341324E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    250.000
C
      DATA (GA( 6, 9,JC),JC=1,3) /
     S 0.95913246E+02, 0.17022213E+02, 0.00000000E+00/
      DATA (GB( 6, 9,JC),JC=1,3) /
     S 0.95913246E+02, 0.18458538E+02, 0.10000000E+01/
      DATA (GA( 6,10,JC),JC=1,3) /
     S 0.96462036E+02, 0.16808561E+02, 0.00000000E+00/
      DATA (GB( 6,10,JC),JC=1,3) /
     S 0.96462036E+02, 0.18285398E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    262.500
C
      DATA (GA( 7, 9,JC),JC=1,3) /
     S 0.96069473E+02, 0.16961018E+02, 0.00000000E+00/
      DATA (GB( 7, 9,JC),JC=1,3) /
     S 0.96069473E+02, 0.18408932E+02, 0.10000000E+01/
      DATA (GA( 7,10,JC),JC=1,3) /
     S 0.96610298E+02, 0.16746571E+02, 0.00000000E+00/
      DATA (GB( 7,10,JC),JC=1,3) /
     S 0.96610298E+02, 0.18234850E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    275.000
C
      DATA (GA( 8, 9,JC),JC=1,3) /
     S 0.96201309E+02, 0.16905493E+02, 0.00000000E+00/
      DATA (GB( 8, 9,JC),JC=1,3) /
     S 0.96201309E+02, 0.18363655E+02, 0.10000000E+01/
      DATA (GA( 8,10,JC),JC=1,3) /
     S 0.96754684E+02, 0.16690109E+02, 0.00000000E+00/
      DATA (GB( 8,10,JC),JC=1,3) /
     S 0.96754684E+02, 0.18189079E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    287.500
C
      DATA (GA( 9, 9,JC),JC=1,3) /
     S 0.96342339E+02, 0.16853893E+02, 0.00000000E+00/
      DATA (GB( 9, 9,JC),JC=1,3) /
     S 0.96342339E+02, 0.18321957E+02, 0.10000000E+01/
      DATA (GA( 9,10,JC),JC=1,3) /
     S 0.96883301E+02, 0.16638573E+02, 0.00000000E+00/
      DATA (GB( 9,10,JC),JC=1,3) /
     S 0.96883301E+02, 0.18147303E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    300.000
C
      DATA (GA(10, 9,JC),JC=1,3) /
     S 0.96457764E+02, 0.16806755E+02, 0.00000000E+00/
      DATA (GB(10, 9,JC),JC=1,3) /
     S 0.96457764E+02, 0.18283590E+02, 0.10000000E+01/
      DATA (GA(10,10,JC),JC=1,3) /
     S 0.96989792E+02, 0.16591677E+02, 0.00000000E+00/
      DATA (GB(10,10,JC),JC=1,3) /
     S 0.96989792E+02, 0.18108814E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =    312.500
C
      DATA (GA(11, 9,JC),JC=1,3) /
     S 0.96549957E+02, 0.16763689E+02, 0.00000000E+00/
      DATA (GB(11, 9,JC),JC=1,3) /
     S 0.96549957E+02, 0.18248074E+02, 0.10000000E+01/
      DATA (GA(11,10,JC),JC=1,3) /
     S 0.97097275E+02, 0.16548479E+02, 0.00000000E+00/
      DATA (GB(11,10,JC),JC=1,3) /
     S 0.97097275E+02, 0.18073788E+02, 0.10000000E+01/
C
C-- WATER VAPOR -- INT.5 --  350- 500 CM-1 
C
C
C----- INTERVAL = 5 ----- T =    187.500
C
      DATA (GA( 1, 5,JC),JC=1,3) /
     S 0.13134047E+00,-0.23649048E-01, 0.00000000E+00/
      DATA (GB( 1, 5,JC),JC=1,3) /
     S 0.13134047E+00, 0.54008597E+00, 0.10000000E+01/
      DATA (GA( 1, 6,JC),JC=1,3) /
     S 0.14049581E+00,-0.26231026E-01, 0.00000000E+00/
      DATA (GB( 1, 6,JC),JC=1,3) /
     S 0.14049581E+00, 0.54143775E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    200.000
C
      DATA (GA( 2, 5,JC),JC=1,3) /
     S 0.13330981E+00,-0.24178915E-01, 0.00000000E+00/
      DATA (GB( 2, 5,JC),JC=1,3) /
     S 0.13330981E+00, 0.54094988E+00, 0.10000000E+01/
      DATA (GA( 2, 6,JC),JC=1,3) /
     S 0.14229582E+00,-0.26725627E-01, 0.00000000E+00/
      DATA (GB( 2, 6,JC),JC=1,3) /
     S 0.14229582E+00, 0.54189402E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    212.500
C
      DATA (GA( 3, 5,JC),JC=1,3) /
     S 0.13504511E+00,-0.24650861E-01, 0.00000000E+00/
      DATA (GB( 3, 5,JC),JC=1,3) /
     S 0.13504511E+00, 0.54169583E+00, 0.10000000E+01/
      DATA (GA( 3, 6,JC),JC=1,3) /
     S 0.14384151E+00,-0.27153090E-01, 0.00000000E+00/
      DATA (GB( 3, 6,JC),JC=1,3) /
     S 0.14384151E+00, 0.54222512E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    225.000
C
      DATA (GA( 4, 5,JC),JC=1,3) /
     S 0.13651314E+00,-0.25044570E-01, 0.00000000E+00/
      DATA (GB( 4, 5,JC),JC=1,3) /
     S 0.13651314E+00, 0.54191625E+00, 0.10000000E+01/
      DATA (GA( 4, 6,JC),JC=1,3) /
     S 0.14517485E+00,-0.27523417E-01, 0.00000000E+00/
      DATA (GB( 4, 6,JC),JC=1,3) /
     S 0.14517485E+00, 0.54246801E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    237.500
C
      DATA (GA( 5, 5,JC),JC=1,3) /
     S 0.13790466E+00,-0.25431205E-01, 0.00000000E+00/
      DATA (GB( 5, 5,JC),JC=1,3) /
     S 0.13790466E+00, 0.54262739E+00, 0.10000000E+01/
      DATA (GA( 5, 6,JC),JC=1,3) /
     S 0.14633319E+00,-0.27845515E-01, 0.00000000E+00/
      DATA (GB( 5, 6,JC),JC=1,3) /
     S 0.14633319E+00, 0.54265642E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    250.000
C
      DATA (GA( 6, 5,JC),JC=1,3) /
     S 0.13909847E+00,-0.25759069E-01, 0.00000000E+00/
      DATA (GB( 6, 5,JC),JC=1,3) /
     S 0.13909847E+00, 0.54293746E+00, 0.10000000E+01/
      DATA (GA( 6, 6,JC),JC=1,3) /
     S 0.14734478E+00,-0.28128289E-01, 0.00000000E+00/
      DATA (GB( 6, 6,JC),JC=1,3) /
     S 0.14734478E+00, 0.54279846E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    262.500
C
      DATA (GA( 7, 5,JC),JC=1,3) /
     S 0.14014673E+00,-0.26045626E-01, 0.00000000E+00/
      DATA (GB( 7, 5,JC),JC=1,3) /
     S 0.14014673E+00, 0.54306209E+00, 0.10000000E+01/
      DATA (GA( 7, 6,JC),JC=1,3) /
     S 0.14823231E+00,-0.28376250E-01, 0.00000000E+00/
      DATA (GB( 7, 6,JC),JC=1,3) /
     S 0.14823231E+00, 0.54290944E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    275.000
C
      DATA (GA( 8, 5,JC),JC=1,3) /
     S 0.14110570E+00,-0.26310772E-01, 0.00000000E+00/
      DATA (GB( 8, 5,JC),JC=1,3) /
     S 0.14110570E+00, 0.54324186E+00, 0.10000000E+01/
      DATA (GA( 8, 6,JC),JC=1,3) /
     S 0.14901793E+00,-0.28597040E-01, 0.00000000E+00/
      DATA (GB( 8, 6,JC),JC=1,3) /
     S 0.14901793E+00, 0.54300690E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    287.500
C
      DATA (GA( 9, 5,JC),JC=1,3) /
     S 0.14198618E+00,-0.26556855E-01, 0.00000000E+00/
      DATA (GB( 9, 5,JC),JC=1,3) /
     S 0.14198618E+00, 0.54346353E+00, 0.10000000E+01/
      DATA (GA( 9, 6,JC),JC=1,3) /
     S 0.14971229E+00,-0.28792191E-01, 0.00000000E+00/
      DATA (GB( 9, 6,JC),JC=1,3) /
     S 0.14971229E+00, 0.54307669E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    300.000
C
      DATA (GA(10, 5,JC),JC=1,3) /
     S 0.14277160E+00,-0.26774861E-01, 0.00000000E+00/
      DATA (GB(10, 5,JC),JC=1,3) /
     S 0.14277160E+00, 0.54357564E+00, 0.10000000E+01/
      DATA (GA(10, 6,JC),JC=1,3) /
     S 0.15033126E+00,-0.28966904E-01, 0.00000000E+00/
      DATA (GB(10, 6,JC),JC=1,3) /
     S 0.15033126E+00, 0.54313493E+00, 0.10000000E+01/
C
C----- INTERVAL = 5 ----- T =    312.500
C
      DATA (GA(11, 5,JC),JC=1,3) /
     S 0.14349319E+00,-0.26976623E-01, 0.00000000E+00/
      DATA (GB(11, 5,JC),JC=1,3) /
     S 0.14349319E+00, 0.54369903E+00, 0.10000000E+01/
      DATA(GA(11, 6,JC),JC=1,3) /
     S 0.15088280E+00,-0.29121898E-01, 0.00000000E+00/
      DATA (GB(11, 6,JC),JC=1,3) /
     S 0.15088280E+00, 0.54318237E+00, 0.10000000E+01/
C
C-WATER VAPOR-WINGS OF VIBRATION-ROTATION BAND - 1250-1450+1880-2820-
C
C
C----- INTERVAL = 6 ----- T =    187.50
C
      DATA (GA( 1,11,JC),JC=1,3) /
     S 0.74737163E+00,-0.60590485E-01, 0.00000000E+00/
      DATA (GB( 1,11,JC),JC=1,3) /
     S 0.74737163E+00, 0.17888287E+01, 0.10000000E+01/
      DATA (GA( 1,12,JC),JC=1,3) /
     S 0.64412124E+00,-0.16340195E-01, 0.00000000E+00/
      DATA (GB( 1,12,JC),JC=1,3) /
     S 0.64412124E+00, 0.16133750E+01, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    200.00
C
      DATA (GA( 2,11,JC),JC=1,3) /
     S 0.67637182E+00,-0.27088422E-01, 0.00000000E+00/
      DATA (GB( 2,11,JC),JC=1,3) /
     S 0.67637182E+00, 0.16680907E+01, 0.10000000E+01/
      DATA (GA( 2,12,JC),JC=1,3) /
     S 0.55131795E+00, 0.32252772E-01, 0.00000000E+00/
      DATA (GB( 2,12,JC),JC=1,3) /
     S 0.55131795E+00, 0.14332603E+01, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    212.50
C
      DATA (GA( 3,11,JC),JC=1,3) /
     S 0.59891051E+00, 0.12874948E-01, 0.00000000E+00/
      DATA (GB( 3,11,JC),JC=1,3) /
     S 0.59891051E+00, 0.15230591E+01, 0.10000000E+01/
      DATA (GA( 3,12,JC),JC=1,3) /
     S 0.44063067E+00, 0.91024989E-01, 0.00000000E+00/
      DATA (GB( 3,12,JC),JC=1,3) /
     S 0.44063067E+00, 0.12039660E+01, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    225.00
C
      DATA (GA( 4,11,JC),JC=1,3) /
     S 0.51061614E+00, 0.59883548E-01, 0.00000000E+00/
      DATA (GB( 4,11,JC),JC=1,3) /
     S 0.51061614E+00, 0.13461757E+01, 0.10000000E+01/
      DATA (GA( 4,12,JC),JC=1,3) /
     S 0.30439727E+00, 0.15980470E+00, 0.00000000E+00/
      DATA (GB( 4,12,JC),JC=1,3) /
     S 0.30439727E+00, 0.91241774E+00, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    237.50
C
      DATA (GA( 5,11,JC),JC=1,3) /
     S 0.40673842E+00, 0.11398935E+00, 0.00000000E+00/
      DATA (GB( 5,11,JC),JC=1,3) /
     S 0.40673842E+00, 0.11289965E+01, 0.10000000E+01/
      DATA (GA( 5,12,JC),JC=1,3) /
     S 0.15005620E+00, 0.23446409E+00, 0.00000000E+00/
      DATA (GB( 5,12,JC),JC=1,3) /
     S 0.15005620E+00, 0.58547602E+00, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    250.00
C
      DATA (GA( 6,11,JC),JC=1,3) /
     S 0.28375203E+00, 0.17444874E+00, 0.00000000E+00/
      DATA (GB( 6,11,JC),JC=1,3) /
     S 0.28375203E+00, 0.86693115E+00, 0.10000000E+01/
      DATA (GA( 6,12,JC),JC=1,3) /
     S 0.51658133E-01, 0.29464628E+00, 0.00000000E+00/
      DATA (GB( 6,12,JC),JC=1,3) /
     S 0.51658133E-01, 0.40170363E+00, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    262.50
C
      DATA (GA( 7,11,JC),JC=1,3) /
     S 0.15136005E+00, 0.23761536E+00, 0.00000000E+00/
      DATA (GB( 7,11,JC),JC=1,3) /
     S 0.15136005E+00, 0.58911683E+00, 0.10000000E+01/
      DATA (GA( 7,12,JC),JC=1,3) /
     S 0.18932679E-01, 0.33516250E+00, 0.00000000E+00/
      DATA (GB( 7,12,JC),JC=1,3) /
     S 0.18932679E-01, 0.36796152E+00, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    275.00
C
      DATA (GA( 8,11,JC),JC=1,3) /
     S 0.61752765E-01, 0.28965454E+00, 0.00000000E+00/
      DATA (GB( 8,11,JC),JC=1,3) /
     S 0.61752765E-01, 0.41975793E+00, 0.10000000E+01/
      DATA (GA( 8,12,JC),JC=1,3) /
     S 0.79619427E-02, 0.36873141E+00, 0.00000000E+00/
      DATA (GB( 8,12,JC),JC=1,3) /
     S 0.79619427E-02, 0.37964009E+00, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    287.50
C
      DATA (GA( 9,11,JC),JC=1,3) /
     S 0.26192233E-01, 0.32568668E+00, 0.00000000E+00/
      DATA (GB( 9,11,JC),JC=1,3) /
     S 0.26192233E-01, 0.37385106E+00, 0.10000000E+01/
      DATA (GA( 9,12,JC),JC=1,3) /
     S 0.40764469E-02, 0.40137572E+00, 0.00000000E+00/
      DATA (GB( 9,12,JC),JC=1,3) /
     S 0.40764469E-02, 0.40608889E+00, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    300.00
C
      DATA (GA(10,11,JC),JC=1,3) /
     S 0.12168738E-01, 0.35455148E+00, 0.00000000E+00/
      DATA (GB(10,11,JC),JC=1,3) /
     S 0.12168738E-01, 0.37329084E+00, 0.10000000E+01/
      DATA (GA(10,12,JC),JC=1,3) /
     S 0.23914061E-02, 0.43615987E+00, 0.00000000E+00/
      DATA (GB(10,12,JC),JC=1,3) /
     S 0.23914061E-02, 0.43919208E+00, 0.10000000E+01/
C
C----- INTERVAL = 6 ----- T =    312.50
C
      DATA (GA(10,11,JC),JC=1,3) /
     S 0.12168738E-01, 0.35455148E+00, 0.00000000E+00/
      DATA (GB(10,11,JC),JC=1,3) /
     S 0.12168738E-01, 0.37329084E+00, 0.10000000E+01/
      DATA (GA(10,12,JC),JC=1,3) /
     S 0.23914061E-02, 0.43615987E+00, 0.00000000E+00/
      DATA (GB(10,12,JC),JC=1,3) /
     S 0.23914061E-02, 0.43919208E+00, 0.10000000E+01/
C
C-- CO2 -- INT.2 -- 500-800 CM-1 --- FROM ABS225 ------------
C
C-- FIU = 0.8 + MAX(0.35,(7-IU)*0.9)  , X/T,  9
C
C----- INTERVAL = 2 ----- T =  187.5
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 1,13,JC),JC=1,3) /
     S 0.87668459E-01, 0.13845511E+01, 0.00000000E+00/
      DATA (GB( 1,13,JC),JC=1,3) /
     S 0.87668459E-01, 0.23203798E+01, 0.10000000E+01/
      DATA (GA( 1,14,JC),JC=1,3) /
     S 0.74878820E-01, 0.11718758E+01, 0.00000000E+00/
      DATA (GB( 1,14,JC),JC=1,3) /
     S 0.74878820E-01, 0.20206726E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  200.0
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 2,13,JC),JC=1,3) /
     S 0.83754276E-01, 0.13187042E+01, 0.00000000E+00/
      DATA (GB( 2,13,JC),JC=1,3) /
     S 0.83754276E-01, 0.22288925E+01, 0.10000000E+01/
      DATA (GA( 2,14,JC),JC=1,3) /
     S 0.71650966E-01, 0.11216131E+01, 0.00000000E+00/
      DATA (GB( 2,14,JC),JC=1,3) /
     S 0.71650966E-01, 0.19441824E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  212.5
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 3,13,JC),JC=1,3) /
     S 0.80460283E-01, 0.12644396E+01, 0.00000000E+00/
      DATA (GB( 3,13,JC),JC=1,3) /
     S 0.80460283E-01, 0.21515593E+01, 0.10000000E+01/
      DATA (GA( 3,14,JC),JC=1,3) /
     S 0.68979615E-01, 0.10809473E+01, 0.00000000E+00/
      DATA (GB( 3,14,JC),JC=1,3) /
     S 0.68979615E-01, 0.18807257E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  225.0
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 4,13,JC),JC=1,3) /
     S 0.77659686E-01, 0.12191543E+01, 0.00000000E+00/
      DATA (GB( 4,13,JC),JC=1,3) /
     S 0.77659686E-01, 0.20855896E+01, 0.10000000E+01/
      DATA (GA( 4,14,JC),JC=1,3) /
     S 0.66745345E-01, 0.10476396E+01, 0.00000000E+00/
      DATA (GB( 4,14,JC),JC=1,3) /
     S 0.66745345E-01, 0.18275618E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  237.5
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 5,13,JC),JC=1,3) /
     S 0.75257056E-01, 0.11809511E+01, 0.00000000E+00/
      DATA (GB( 5,13,JC),JC=1,3) /
     S 0.75257056E-01, 0.20288489E+01, 0.10000000E+01/
      DATA (GA( 5,14,JC),JC=1,3) /
     S 0.64857571E-01, 0.10200373E+01, 0.00000000E+00/
      DATA (GB( 5,14,JC),JC=1,3) /
     S 0.64857571E-01, 0.17825910E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  250.0
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 6,13,JC),JC=1,3) /
     S 0.73179175E-01, 0.11484154E+01, 0.00000000E+00/
      DATA (GB( 6,13,JC),JC=1,3) /
     S 0.73179175E-01, 0.19796791E+01, 0.10000000E+01/
      DATA (GA( 6,14,JC),JC=1,3) /
     S 0.63248495E-01, 0.99692726E+00, 0.00000000E+00/
      DATA (GB( 6,14,JC),JC=1,3) /
     S 0.63248495E-01, 0.17442308E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  262.5
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 7,13,JC),JC=1,3) /
     S 0.71369063E-01, 0.11204723E+01, 0.00000000E+00/
      DATA (GB( 7,13,JC),JC=1,3) /
     S 0.71369063E-01, 0.19367778E+01, 0.10000000E+01/
      DATA (GA( 7,14,JC),JC=1,3) /
     S 0.61866970E-01, 0.97740923E+00, 0.00000000E+00/
      DATA (GB( 7,14,JC),JC=1,3) /
     S 0.61866970E-01, 0.17112809E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  275.0
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 8,13,JC),JC=1,3) /
     S 0.69781812E-01, 0.10962918E+01, 0.00000000E+00/
      DATA (GB( 8,13,JC),JC=1,3) /
     S 0.69781812E-01, 0.18991112E+01, 0.10000000E+01/
      DATA (GA( 8,14,JC),JC=1,3) /
     S 0.60673632E-01, 0.96080188E+00, 0.00000000E+00/
      DATA (GB( 8,14,JC),JC=1,3) /
     S 0.60673632E-01, 0.16828137E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  287.5
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA( 9,13,JC),JC=1,3) /
     S 0.68381606E-01, 0.10752229E+01, 0.00000000E+00/
      DATA (GB( 9,13,JC),JC=1,3) /
     S 0.68381606E-01, 0.18658501E+01, 0.10000000E+01/
      DATA (GA( 9,14,JC),JC=1,3) /
     S 0.59637277E-01, 0.94657562E+00, 0.00000000E+00/
      DATA (GB( 9,14,JC),JC=1,3) /
     S 0.59637277E-01, 0.16580908E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  300.0
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA(10,13,JC),JC=1,3) /
     S 0.67139539E-01, 0.10567474E+01, 0.00000000E+00/
      DATA (GB(10,13,JC),JC=1,3) /
     S 0.67139539E-01, 0.18363226E+01, 0.10000000E+01/
      DATA (GA(10,14,JC),JC=1,3) /
     S 0.58732178E-01, 0.93430511E+00, 0.00000000E+00/
      DATA (GB(10,14,JC),JC=1,3) /
     S 0.58732178E-01, 0.16365014E+01, 0.10000000E+01/
C
C----- INTERVAL = 2 ----- T =  312.5
C
C-- INDICES FOR PADE APPROXIMATION   1 30 38 45
      DATA (GA(11,13,JC),JC=1,3) /
     S 0.66032012E-01, 0.10404465E+01, 0.00000000E+00/
      DATA (GB(11,13,JC),JC=1,3) /
     S 0.66032012E-01, 0.18099779E+01, 0.10000000E+01/
      DATA (GA(11,14,JC),JC=1,3) /
     S 0.57936092E-01, 0.92363528E+00, 0.00000000E+00/
      DATA (GB(11,14,JC),JC=1,3) /
     S 0.57936092E-01, 0.16175164E+01, 0.10000000E+01/
C
C
C-- CARBON DIOXIDE LINES IN THE WINDOW REGION (800-1250 CM-1)
C
C-- G = 0.0
C
C----- INTERVAL = 4 ----- T =  187.5
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 1,15,JC),JC=1,3) /
     S 0.13230067E+02, 0.22042132E+02, 0.00000000E+00/
      DATA (GB( 1,15,JC),JC=1,3) /
     S 0.13230067E+02, 0.22051750E+02, 0.10000000E+01/
      DATA (GA( 1,16,JC),JC=1,3) /
     S 0.13183816E+02, 0.22169501E+02, 0.00000000E+00/
      DATA (GB( 1,16,JC),JC=1,3) /
     S 0.13183816E+02, 0.22178972E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  200.0
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 2,15,JC),JC=1,3) /
     S 0.13213564E+02, 0.22107298E+02, 0.00000000E+00/
      DATA (GB( 2,15,JC),JC=1,3) /
     S 0.13213564E+02, 0.22116850E+02, 0.10000000E+01/
      DATA (GA( 2,16,JC),JC=1,3) /
     S 0.13189991E+02, 0.22270075E+02, 0.00000000E+00/
      DATA (GB( 2,16,JC),JC=1,3) /
     S 0.13189991E+02, 0.22279484E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  212.5
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 3,15,JC),JC=1,3) /
     S 0.13209140E+02, 0.22180915E+02, 0.00000000E+00/
      DATA (GB( 3,15,JC),JC=1,3) /
     S 0.13209140E+02, 0.22190410E+02, 0.10000000E+01/
      DATA (GA( 3,16,JC),JC=1,3) /
     S 0.13209485E+02, 0.22379193E+02, 0.00000000E+00/
      DATA (GB( 3,16,JC),JC=1,3) /
     S 0.13209485E+02, 0.22388551E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  225.0
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 4,15,JC),JC=1,3) /
     S 0.13213894E+02, 0.22259478E+02, 0.00000000E+00/
      DATA (GB( 4,15,JC),JC=1,3) /
     S 0.13213894E+02, 0.22268925E+02, 0.10000000E+01/
      DATA (GA( 4,16,JC),JC=1,3) /
     S 0.13238789E+02, 0.22492992E+02, 0.00000000E+00/
      DATA (GB( 4,16,JC),JC=1,3) /
     S 0.13238789E+02, 0.22502309E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  237.5
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 5,15,JC),JC=1,3) /
     S 0.13225963E+02, 0.22341039E+02, 0.00000000E+00/
      DATA (GB( 5,15,JC),JC=1,3) /
     S 0.13225963E+02, 0.22350445E+02, 0.10000000E+01/
      DATA (GA( 5,16,JC),JC=1,3) /
     S 0.13275017E+02, 0.22608508E+02, 0.00000000E+00/
      DATA (GB( 5,16,JC),JC=1,3) /
     S 0.13275017E+02, 0.22617792E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  250.0
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 6,15,JC),JC=1,3) /
     S 0.13243806E+02, 0.22424247E+02, 0.00000000E+00/
      DATA (GB( 6,15,JC),JC=1,3) /
     S 0.13243806E+02, 0.22433617E+02, 0.10000000E+01/
      DATA (GA( 6,16,JC),JC=1,3) /
     S 0.13316096E+02, 0.22723843E+02, 0.00000000E+00/
      DATA (GB( 6,16,JC),JC=1,3) /
     S 0.13316096E+02, 0.22733099E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  262.5
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 7,15,JC),JC=1,3) /
     S 0.13266104E+02, 0.22508089E+02, 0.00000000E+00/
      DATA (GB( 7,15,JC),JC=1,3) /
     S 0.13266104E+02, 0.22517429E+02, 0.10000000E+01/
      DATA (GA( 7,16,JC),JC=1,3) /
     S 0.13360555E+02, 0.22837837E+02, 0.00000000E+00/
      DATA (GB( 7,16,JC),JC=1,3) /
     S 0.13360555E+02, 0.22847071E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  275.0
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 8,15,JC),JC=1,3) /
     S 0.13291782E+02, 0.22591771E+02, 0.00000000E+00/
      DATA (GB( 8,15,JC),JC=1,3) /
     S 0.13291782E+02, 0.22601086E+02, 0.10000000E+01/
      DATA (GA( 8,16,JC),JC=1,3) /
     S 0.13407324E+02, 0.22949751E+02, 0.00000000E+00/
      DATA (GB( 8,16,JC),JC=1,3) /
     S 0.13407324E+02, 0.22958967E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  287.5
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA( 9,15,JC),JC=1,3) /
     S 0.13319961E+02, 0.22674661E+02, 0.00000000E+00/
      DATA (GB( 9,15,JC),JC=1,3) /
     S 0.13319961E+02, 0.22683956E+02, 0.10000000E+01/
      DATA (GA( 9,16,JC),JC=1,3) /
     S 0.13455544E+02, 0.23059032E+02, 0.00000000E+00/
      DATA (GB( 9,16,JC),JC=1,3) /
     S 0.13455544E+02, 0.23068234E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  300.0
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA(10,15,JC),JC=1,3) /
     S 0.13349927E+02, 0.22756246E+02, 0.00000000E+00/
      DATA (GB(10,15,JC),JC=1,3) /
     S 0.13349927E+02, 0.22765522E+02, 0.10000000E+01/
      DATA (GA(10,16,JC),JC=1,3) /
     S 0.13504450E+02, 0.23165146E+02, 0.00000000E+00/
      DATA (GB(10,16,JC),JC=1,3) /
     S 0.13504450E+02, 0.23174336E+02, 0.10000000E+01/
C
C----- INTERVAL = 4 ----- T =  312.5
C
C-- INDICES FOR PADE APPROXIMATION     1   15   29   45
      DATA (GA(11,15,JC),JC=1,3) /
     S 0.13381108E+02, 0.22836093E+02, 0.00000000E+00/
      DATA (GB(11,15,JC),JC=1,3) /
     S 0.13381108E+02, 0.22845354E+02, 0.10000000E+01/
      DATA (GA(11,16,JC),JC=1,3) /
     S 0.13553282E+02, 0.23267456E+02, 0.00000000E+00/
      DATA (GB(11,16,JC),JC=1,3) /
     S 0.13553282E+02, 0.23276638E+02, 0.10000000E+01/
C
C
C-- WATER VAPOR --- 650 - 800 CM-1
C
C---- INTERVAL = 2B ---- T = 187.5
C
      DATA (GA( 1,21,JC),JC=1,3) /
     S 0.19550761E+02,-0.26333148E+00, 0.00000000E+00/
      DATA (GB( 1,21,JC),JC=1,3) /
     S 0.19550761E+02, 0.49595891E+01, 0.10000000E+01/
      DATA (GA( 1,22,JC),JC=1,3) /
     S 0.20402482E+02,-0.24922589E+00, 0.00000000E+00/
      DATA (GB( 1,22,JC),JC=1,3) /
     S 0.20402482E+02, 0.51104394E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 200.0
C
      DATA (GA( 2,21,JC),JC=1,3) /
     S 0.19830199E+02,-0.25754698E+00, 0.00000000E+00/
      DATA (GB( 2,21,JC),JC=1,3) /
     S 0.19830199E+02, 0.50093036E+01, 0.10000000E+01/
      DATA (GA( 2,22,JC),JC=1,3) /
     S 0.20696147E+02,-0.24494693E+00, 0.00000000E+00/
      DATA (GB( 2,22,JC),JC=1,3) /
     S 0.20696147E+02, 0.51601103E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 212.5
C
      DATA (GA( 3,21,JC),JC=1,3) /
     S 0.20094853E+02,-0.25394587E+00, 0.00000000E+00/
      DATA (GB( 3,21,JC),JC=1,3) /
     S 0.20094853E+02, 0.50564022E+01, 0.10000000E+01/
      DATA (GA( 3,22,JC),JC=1,3) /
     S 0.20957105E+02,-0.24158774E+00, 0.00000000E+00/
      DATA (GB( 3,22,JC),JC=1,3) /
     S 0.20957105E+02, 0.52035338E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 225.0
C
      DATA (GA( 4,21,JC),JC=1,3) /
     S 0.20321192E+02,-0.25012572E+00, 0.00000000E+00/
      DATA (GB( 4,21,JC),JC=1,3) /
     S 0.20321192E+02, 0.50954623E+01, 0.10000000E+01/
      DATA (GA( 4,22,JC),JC=1,3) /
     S 0.21189573E+02,-0.23892446E+00, 0.00000000E+00/
      DATA (GB( 4,22,JC),JC=1,3) /
     S 0.21189573E+02, 0.52416609E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 237.5
C
      DATA (GA( 5,21,JC),JC=1,3) /
     S 0.20525678E+02,-0.24697148E+00, 0.00000000E+00/
      DATA (GB( 5,21,JC),JC=1,3) /
     S 0.20525678E+02, 0.51303033E+01, 0.10000000E+01/
      DATA (GA( 5,22,JC),JC=1,3) /
     S 0.21397190E+02,-0.23679344E+00, 0.00000000E+00/
      DATA (GB( 5,22,JC),JC=1,3) /
     S 0.21397190E+02, 0.52752763E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 250.0
C
      DATA (GA( 6,21,JC),JC=1,3) /
     S 0.20710965E+02,-0.24434489E+00, 0.00000000E+00/
      DATA (GB( 6,21,JC),JC=1,3) /
     S 0.20710965E+02, 0.51615130E+01, 0.10000000E+01/
      DATA (GA( 6,22,JC),JC=1,3) /
     S 0.21583088E+02,-0.23507367E+00, 0.00000000E+00/
      DATA (GB( 6,22,JC),JC=1,3) /
     S 0.21583088E+02, 0.53050307E+01, 0.10000000E+01/

C
C---- INTERVAL = 2B ---- T = 262.5
C
      DATA (GA( 7,21,JC),JC=1,3) /
     S 0.20879342E+02,-0.24214043E+00, 0.00000000E+00/
      DATA (GB( 7,21,JC),JC=1,3) /
     S 0.20879342E+02, 0.51895822E+01, 0.10000000E+01/
      DATA (GA( 7,22,JC),JC=1,3) /
     S 0.21767391E+02,-0.23500593E+00, 0.00000000E+00/
      DATA (GB( 7,22,JC),JC=1,3) /
     S 0.21767391E+02, 0.53353369E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 275.0
C
      DATA (GA( 8,21,JC),JC=1,3) /
     S 0.21032783E+02,-0.24027675E+00, 0.00000000E+00/
      DATA (GB( 8,21,JC),JC=1,3) /
     S 0.21032783E+02, 0.52149228E+01, 0.10000000E+01/
      DATA (GA( 8,22,JC),JC=1,3) /
     S 0.21917785E+02,-0.23387069E+00, 0.00000000E+00/
      DATA (GB( 8,22,JC),JC=1,3) /
     S 0.21917785E+02, 0.53589399E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 287.5
C
      DATA (GA( 9,21,JC),JC=1,3) /
     S 0.21172996E+02,-0.23869043E+00, 0.00000000E+00/
      DATA (GB( 9,21,JC),JC=1,3) /
     S 0.21172996E+02, 0.52378823E+01, 0.10000000E+01/
      DATA (GA( 9,22,JC),JC=1,3) /
     S 0.22053477E+02,-0.23293447E+00, 0.00000000E+00/
      DATA (GB( 9,22,JC),JC=1,3) /
     S 0.22053477E+02, 0.53800578E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 300.0
C
      DATA (GA(10,21,JC),JC=1,3) /
     S 0.21301462E+02,-0.23733163E+00, 0.00000000E+00/
      DATA (GB(10,21,JC),JC=1,3) /
     S 0.21301462E+02, 0.52587551E+01, 0.10000000E+01/
      DATA (GA(10,22,JC),JC=1,3) /
     S 0.22176209E+02,-0.23215726E+00, 0.00000000E+00/
      DATA (GB(10,22,JC),JC=1,3) /
     S 0.22176209E+02, 0.53990137E+01, 0.10000000E+01/
C
C---- INTERVAL = 2B ---- T = 312.5
C
      DATA (GA(11,21,JC),JC=1,3) /
     S 0.21419467E+02,-0.23616087E+00, 0.00000000E+00/
      DATA (GB(11,21,JC),JC=1,3) /
     S 0.21419467E+02, 0.52777920E+01, 0.10000000E+01/
      DATA (GA(11,22,JC),JC=1,3) /
     S 0.22287486E+02,-0.23150796E+00, 0.00000000E+00/
      DATA (GB(11,22,JC),JC=1,3) /
     S 0.22287486E+02, 0.54160819E+01, 0.10000000E+01/
C
C
C-- WATER VAPOR --- 800 - 970 CM-1
C
C---- INTERVAL = 3A ---- T = 187.5
C
      DATA (GA( 1,19,JC),JC=1,3) /
     S 0.76275605E+02, 0.87071385E+01, 0.00000000E+00/
      DATA (GB( 1,19,JC),JC=1,3) /
     S 0.76275605E+02, 0.12222527E+02, 0.10000000E+01/
      DATA (GA( 1,20,JC),JC=1,3) /
     S 0.76707978E+02, 0.89850885E+01, 0.00000000E+00/
      DATA (GB( 1,20,JC),JC=1,3) /
     S 0.76707978E+02, 0.12434732E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 200
C
      DATA (GA( 2,19,JC),JC=1,3) /
     S 0.76444692E+02, 0.88235285E+01, 0.00000000E+00/
      DATA (GB( 2,19,JC),JC=1,3) /
     S 0.76444692E+02, 0.12310814E+02, 0.10000000E+01/
      DATA (GA( 2,20,JC),JC=1,3) /
     S 0.76898353E+02, 0.91013883E+01, 0.00000000E+00/
      DATA (GB( 2,20,JC),JC=1,3) /
     S 0.76898353E+02, 0.12523769E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 212.5
C
      DATA (GA( 3,19,JC),JC=1,3) /
     S 0.76601454E+02, 0.89263097E+01, 0.00000000E+00/
      DATA (GB( 3,19,JC),JC=1,3) /
     S 0.76601454E+02, 0.12389074E+02, 0.10000000E+01/
      DATA (GA( 3,20,JC),JC=1,3) /
     S 0.77725293E+02, 0.91803661E+01, 0.00000000E+00/
      DATA (GB( 3,20,JC),JC=1,3) /
     S 0.77725293E+02, 0.12614198E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 225
C
      DATA (GA( 4,19,JC),JC=1,3) /
     S 0.76746614E+02, 0.90176179E+01, 0.00000000E+00/
      DATA (GB( 4,19,JC),JC=1,3) /
     S 0.76746614E+02, 0.12458826E+02, 0.10000000E+01/
      DATA (GA( 4,20,JC),JC=1,3) /
     S 0.77890904E+02, 0.92706174E+01, 0.00000000E+00/
      DATA (GB( 4,20,JC),JC=1,3) /
     S 0.77890904E+02, 0.12683738E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 237.5
C
      DATA (GA( 5,19,JC),JC=1,3) /
     S 0.76880972E+02, 0.90991713E+01, 0.00000000E+00/
      DATA (GB( 5,19,JC),JC=1,3) /
     S 0.76880972E+02, 0.12521305E+02, 0.10000000E+01/
      DATA (GA( 5,20,JC),JC=1,3) /
     S 0.78042609E+02, 0.93508020E+01, 0.00000000E+00/
      DATA (GB( 5,20,JC),JC=1,3) /
     S 0.78042609E+02, 0.12745679E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 250
C
      DATA (GA( 6,19,JC),JC=1,3) /
     S 0.77655425E+02, 0.91491477E+01, 0.00000000E+00/
      DATA (GB( 6,19,JC),JC=1,3) /
     S 0.77655425E+02, 0.12589427E+02, 0.10000000E+01/
      DATA (GA( 6,20,JC),JC=1,3) /
     S 0.78181610E+02, 0.94223568E+01, 0.00000000E+00/
      DATA (GB( 6,20,JC),JC=1,3) /
     S 0.78181610E+02, 0.12801076E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 262.5
C
      DATA (GA( 7,19,JC),JC=1,3) /
     S 0.77774452E+02, 0.92150200E+01, 0.00000000E+00/
      DATA (GB( 7,19,JC),JC=1,3) /
     S 0.77774452E+02, 0.12640126E+02, 0.10000000E+01/
      DATA (GA( 7,20,JC),JC=1,3) /
     S 0.78309039E+02, 0.94864655E+01, 0.00000000E+00/
      DATA (GB( 7,20,JC),JC=1,3) /
     S 0.78309039E+02, 0.12850804E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 275
C
      DATA (GA( 8,19,JC),JC=1,3) /
     S 0.77884708E+02, 0.92746449E+01, 0.00000000E+00/
      DATA (GB( 8,19,JC),JC=1,3) /
     S 0.77884708E+02, 0.12686105E+02, 0.10000000E+01/
      DATA (GA( 8,20,JC),JC=1,3) /
     S 0.78425953E+02, 0.95441121E+01, 0.00000000E+00/
      DATA (GB( 8,20,JC),JC=1,3) /
     S 0.78425953E+02, 0.12895595E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 287.5
C
      DATA (GA( 9,19,JC),JC=1,3) /
     S 0.77986957E+02, 0.93288167E+01, 0.00000000E+00/
      DATA (GB( 9,19,JC),JC=1,3) /
     S 0.77986957E+02, 0.12727952E+02, 0.10000000E+01/
      DATA (GA( 9,20,JC),JC=1,3) /
     S 0.78533324E+02, 0.95961213E+01, 0.00000000E+00/
      DATA (GB( 9,20,JC),JC=1,3) /
     S 0.78533324E+02, 0.12936067E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 300
C
      DATA (GA(10,19,JC),JC=1,3) /
     S 0.78081895E+02, 0.93782046E+01, 0.00000000E+00/
      DATA (GB(10,19,JC),JC=1,3) /
     S 0.78081895E+02, 0.12766161E+02, 0.10000000E+01/
      DATA (GA(10,20,JC),JC=1,3) /
     S 0.78632031E+02, 0.96431901E+01, 0.00000000E+00/
      DATA (GB(10,20,JC),JC=1,3) /
     S 0.78632031E+02, 0.12972742E+02, 0.10000000E+01/
C
C---- INTERVAL = 3A ---- T = 312.5
C
      DATA (GA(11,19,JC),JC=1,3) /
     S 0.78170173E+02, 0.94233755E+01, 0.00000000E+00/
      DATA (GB(11,19,JC),JC=1,3) /
     S 0.78170173E+02, 0.12801157E+02, 0.10000000E+01/
      DATA (GA(11,20,JC),JC=1,3) /
     S 0.79479954E+02, 0.96591215E+01, 0.00000000E+00/
      DATA (GB(11,20,JC),JC=1,3) /
     S 0.79479954E+02, 0.13018486E+02, 0.10000000E+01/
C
C
C-- WATER VAPOR --- 1250 - 1450 CM-1
C
C---- INTERVAL = 6A ---- T = 187.5
C
      DATA (GA( 1,17,JC),JC=1,3) /
     S 0.89351314E+00,-0.11716478E+00, 0.00000000E+00/
      DATA (GB( 1,17,JC),JC=1,3) /
     S 0.89351314E+00, 0.23371168E+01, 0.10000000E+01/
      DATA (GA( 1,18,JC),JC=1,3) /
     S 0.78489275E+00,-0.93578500E-01, 0.00000000E+00/
      DATA (GB( 1,18,JC),JC=1,3) /
     S 0.78489275E+00, 0.21397518E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 200
C
      DATA (GA( 2,17,JC),JC=1,3) /
     S 0.82462699E+00,-0.10225793E+00, 0.00000000E+00/
      DATA (GB( 2,17,JC),JC=1,3) /
     S 0.82462699E+00, 0.22153415E+01, 0.10000000E+01/
      DATA (GA( 2,18,JC),JC=1,3) /
     S 0.71552080E+00,-0.78132444E-01, 0.00000000E+00/
      DATA (GB( 2,18,JC),JC=1,3) /
     S 0.71552080E+00, 0.20032846E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 212.5
C
      DATA (GA( 3,17,JC),JC=1,3) /
     S 0.76338653E+00,-0.88787922E-01, 0.00000000E+00/
      DATA (GB( 3,17,JC),JC=1,3) /
     S 0.76338653E+00, 0.20996613E+01, 0.10000000E+01/
      DATA (GA( 3,18,JC),JC=1,3) /
     S 0.65500758E+00,-0.64411300E-01, 0.00000000E+00/
      DATA (GB( 3,18,JC),JC=1,3) /
     S 0.65500758E+00, 0.18774865E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 225
C
      DATA (GA( 4,17,JC),JC=1,3) /
     S 0.70887760E+00,-0.76600481E-01, 0.00000000E+00/
      DATA (GB( 4,17,JC),JC=1,3) /
     S 0.70887760E+00, 0.19909030E+01, 0.10000000E+01/
      DATA (GA( 4,18,JC),JC=1,3) /
     S 0.60215560E+00,-0.52213461E-01, 0.00000000E+00/
      DATA (GB( 4,18,JC),JC=1,3) /
     S 0.60215560E+00, 0.17624913E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 237.5
C
      DATA (GA( 5,17,JC),JC=1,3) /
     S 0.66065205E+00,-0.65662686E-01, 0.00000000E+00/
      DATA (GB( 5,17,JC),JC=1,3) /
     S 0.66065205E+00, 0.18904910E+01, 0.10000000E+01/
      DATA (GA( 5,18,JC),JC=1,3) /
     S 0.55598748E+00,-0.41381040E-01, 0.00000000E+00/
      DATA (GB( 5,18,JC),JC=1,3) /
     S 0.55598748E+00, 0.16581940E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 250
C
      DATA (GA( 6,17,JC),JC=1,3) /
     S 0.61783356E+00,-0.55813085E-01, 0.00000000E+00/
      DATA (GB( 6,17,JC),JC=1,3) /
     S 0.61783356E+00, 0.17980026E+01, 0.10000000E+01/
      DATA (GA( 6,18,JC),JC=1,3) /
     S 0.51447244E+00,-0.31409439E-01, 0.00000000E+00/
      DATA (GB( 6,18,JC),JC=1,3) /
     S 0.51447244E+00, 0.15605436E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 262.5
C
      DATA (GA( 7,17,JC),JC=1,3) /
     S 0.57976804E+00,-0.46940089E-01, 0.00000000E+00/
      DATA (GB( 7,17,JC),JC=1,3) /
     S 0.57976804E+00, 0.17131839E+01, 0.10000000E+01/
      DATA (GA( 7,18,JC),JC=1,3) /
     S 0.47868911E+00,-0.22726019E-01, 0.00000000E+00/
      DATA (GB( 7,18,JC),JC=1,3) /
     S 0.47868911E+00, 0.14745410E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 275
C
      DATA (GA( 8,17,JC),JC=1,3) /
     S 0.54587141E+00,-0.38940929E-01, 0.00000000E+00/
      DATA (GB( 8,17,JC),JC=1,3) /
     S 0.54587141E+00, 0.16356179E+01, 0.10000000E+01/
      DATA (GA( 8,18,JC),JC=1,3) /
     S 0.44731443E+00,-0.15014979E-01, 0.00000000E+00/
      DATA (GB( 8,18,JC),JC=1,3) /
     S 0.44731443E+00, 0.13974344E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 287.5
C
      DATA (GA( 9,17,JC),JC=1,3) /
     S 0.51448094E+00,-0.31366881E-01, 0.00000000E+00/
      DATA (GB( 9,17,JC),JC=1,3) /
     S 0.51448094E+00, 0.15612510E+01, 0.10000000E+01/
      DATA (GA( 9,18,JC),JC=1,3) /
     S 0.41974527E+00,-0.81628316E-02, 0.00000000E+00/
      DATA (GB( 9,18,JC),JC=1,3) /
     S 0.41974527E+00, 0.13283720E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 300
C
      DATA (GA(10,17,JC),JC=1,3) /
     S 0.48699661E+00,-0.24698543E-01, 0.00000000E+00/
      DATA (GB(10,17,JC),JC=1,3) /
     S 0.48699661E+00, 0.14952467E+01, 0.10000000E+01/
      DATA (GA(10,18,JC),JC=1,3) /
     S 0.39576456E+00,-0.21710464E-02, 0.00000000E+00/
      DATA (GB(10,18,JC),JC=1,3) /
     S 0.39576456E+00, 0.12674953E+01, 0.10000000E+01/
C
C---- INTERVAL = 6A ---- T = 312.5
C
      DATA (GA(11,17,JC),JC=1,3) /
     S 0.46280034E+00,-0.18801305E-01, 0.00000000E+00/
      DATA (GB(11,17,JC),JC=1,3) /
     S 0.46280034E+00, 0.14364190E+01, 0.10000000E+01/
      DATA (GA(11,18,JC),JC=1,3) /
     S 0.37428169E+00, 0.32669120E-02, 0.00000000E+00/
      DATA (GB(11,18,JC),JC=1,3) /
     S 0.37428169E+00, 0.12119726E+01, 0.10000000E+01/
C
C-----  (CONTINUUM ABSORPTION)
C
      DATA (ETY( 1, JC),JC=1,18)/1178.11, 996.24, 285.28, 283.63, 
     S     98.88, 97.84, 39.02, 38.56, 13.01, 12.93, 7.04, 7.02, 
     S     6.46, 6.48, 26.52, 26.95, 12.92, 12.20/
      DATA (ETY( 2, JC),JC=1,18)/1108.77, 924.99, 284.92, 283.32,
     S     98.58, 97.56, 38.85, 38.40, 12.97, 12.89, 7.03, 7.01,
     S     6.47, 6.49, 26.80, 27.25, 12.19, 11.45/
      DATA (ETY( 3, JC),JC=1,18)/1044.99, 864.44, 284.60, 283.03, 
     S     98.32, 97.33, 38.71, 38.27, 12.94, 12.86, 7.02, 7.00,
     S     6.48, 6.50, 27.05, 27.51, 11.52, 10.77/
      DATA (ETY( 4, JC),JC=1,18)/ 987.37, 813.88, 284.32, 282.76,
     S     98.09, 97.13, 38.59, 38.15, 12.92, 12.83, 7.02, 6.99,
     S     6.49, 6.50, 27.28, 27.74, 10.92, 10.16/
      DATA (ETY( 5, JC),JC=1,18)/ 935.91, 772.35, 284.08, 282.52,
     S     97.90, 96.95, 38.48, 38.05, 12.89, 12.82, 7.01, 6.99,
     S     6.49, 6.51, 27.48, 27.95, 10.37, 9.61/
      DATA (ETY( 6, JC),JC=1,18)/ 890.38, 738.66, 283.87, 282.33,
     S     97.72, 96.80, 38.38, 37.96, 12.87, 12.80, 7.01, 6.98,
     S     6.50, 6.52, 27.67, 28.15, 9.87, 9.10/
      DATA (ETY( 7, JC),JC=1,18)/ 850.38, 711.54, 283.69, 282.15,
     S     97.57, 96.67, 38.30, 37.89, 12.85, 12.78, 6.99, 6.97,
     S     6.50, 6.53, 27.84, 28.32, 9.42, 8.65/
      DATA (ETY( 8, JC),JC=1,18)/ 815.50, 689.77, 283.52, 282.00,
     S     97.43, 96.55, 38.22, 37.82, 12.83, 12.76, 6.99, 6.97,
     S     6.51, 6.53, 28.00, 28.49, 9.00, 8.23/
      DATA (ETY( 9, JC),JC=1,18)/ 785.25, 672.23, 283.27, 281.87,
     S     97.30, 96.44, 38.15, 37.75, 12.82, 12.74, 6.99, 6.96,
     S     6.51, 6.53, 28.15, 28.64, 8.62, 7.85/
      DATA (ETY(10, JC),JC=1,18)/ 759.14, 658.02, 283.23, 281.75,
     S     97.19, 96.35, 38.09, 37.70, 12.81, 12.73, 6.98, 6.96,
     S     6.52, 6.54, 28.28, 28.78, 8.27, 7.50/
      DATA (ETY(11, JC),JC=1,18)/ 736.69, 646.40, 283.09, 281.64,
     S     97.09, 96.27, 38.03, 37.65, 12.79, 12.72, 6.98, 6.95,
     S     6.52, 6.54, 28.41, 28.90, 7.94, 7.19/
C
      DATA (PTY( 1, JC),JC=1,18)/76.577, 68.422, 6.982, 6.864,
     S     0.586, 0.565, 0.043, 0.042, 0.017, 0.017, 0.017, 0.017, 
     S     0.038, 0.039, 0.684, 0.709, 0.852, 0.800/
      DATA (PTY( 2, JC),JC=1,18)/73.332, 70.449, 6.955, 6.842, 
     S     0.580, 0.560, 0.043, 0.041, 0.017, 0.017, 0.017, 0.017, 
     S     0.039, 0.039, 0.700, 0.725, 0.800, 0.748/
      DATA (PTY( 3, JC),JC=1,18)/62.687, 67.895, 6.933, 6.823, 
     S     0.575, 0.555, 0.042, 0.041, 0.017, 0.017, 0.017, 0.017, 
     S     0.039, 0.040, 0.714, 0.740, 0.753, 0.701/
      DATA (PTY( 4, JC),JC=1,18)/60.501, 65.641, 6.913, 6.807, 
     S     0.571, 0.552, 0.042, 0.041, 0.017, 0.017, 0.017, 0.017, 
     S     0.039, 0.040, 0.727, 0.753, 0.711, 0.660/
      DATA (PTY( 5, JC),JC=1,18)/58.705, 63.661, 6.896, 6.793, 
     S     0.567, 0.548, 0.042, 0.040, 0.017, 0.017, 0.017, 0.017, 
     S     0.040, 0.040, 0.739, 0.765, 0.674, 0.623/
      DATA (PTY( 6, JC),JC=1,18)/57.245, 61.930, 6.881, 6.781, 
     S     0.563, 0.545, 0.041, 0.040, 0.017, 0.017, 0.017, 0.017, 
     S     0.040, 0.041, 0.749, 0.776, 0.641, 0.590/
      DATA (PTY( 7, JC),JC=1,18)/56.066, 60.424, 6.868, 6.771, 
     S     0.560, 0.543, 0.041, 0.040, 0.017, 0.017, 0.017, 0.017, 
     S     0.040, 0.041, 0.759, 0.786, 0.611, 0.561/
      DATA (PTY( 8, JC),JC=1,18)/65.313, 55.114, 6.856, 6.762, 
     S     0.558, 0.540, 0.041, 0.039, 0.017, 0.017, 0.017, 0.017, 
     S     0.040, 0.041, 0.768, 0.795, 0.584, 0.534/
      DATA (PTY( 9, JC),JC=1,18)/59.119, 54.342, 6.845, 6.754, 
     S     0.555, 0.538, 0.041, 0.039, 0.017, 0.017, 0.017, 0.017, 
     S     0.040, 0.041, 0.776, 0.804, 0.559, 0.510/
      DATA (PTY(10, JC),JC=1,18)/57.994, 53.713, 6.836, 6.747, 
     S     0.553, 0.537, 0.040, 0.039, 0.017, 0.017, 0.017, 0.017, 
     S     0.041, 0.041, 0.784, 0.812, 0.537, 0.488/
      DATA (PTY(11, JC),JC=1,18)/57.025, 53.194, 6.827, 6.741,
     S     0.551, 0.535, 0.040, 0.039, 0.017, 0.017, 0.017, 0.017, 
     S     0.041, 0.042, 0.791, 0.819, 0.516, 0.468/
C
C              -----------
C
C     THE COMMON THAT CONTAINS  PLANCK FUNCTION WEIGHTED  FLUX 
C     TRANSMISSIVITIES PRECALCULATED BY THE LINE-BY-LINE MODEL
C     GENLN2 4.0 ( D. P. EDWARDS 1995 )      
C     THE COEFFICIENTS OF TEMPERATURE DEPENDENCE ARE FITTED FROM
C     LBL CALCULATED TRANSMISSION FOR 200, 250 AND 300 K.
C     O3 TB IS ALSO IN.
C
C     ------------------------------------------------------------
C
C
C     ------------------------------------------------------------
      DATA (PLG(JP),JP=1,31)/
     S -8.,-7.5,-7.,-6.5,-6.,-5.5,-5.,-4.5,-4.,-3.5,-3.,-2.5,-2.,-1.5,
     S -1.,-0.5, 0., 0.5, 1., 1.5, 2., 2.5, 3., 3.5,4., 4.5, 5., 5.5, 
     S  6., 6.5, 7./  
C
      DATA (ULG(JU),JU=1,40)/
     S -13.,-12.5,-12.,-11.5,-11.,-10.5,-10.,-9.5,-9.,-8.5,-8.,-7.5,-7.,
     S -6.5,-6.,-5.5,-5.,-4.5,-4.,-3.5,-3.,-2.5,-2.,-1.5,-1.,-0.5, 0.,
     S  0.5, 1., 1.5, 2., 2.5, 3., 3.5, 4., 4.5, 5., 5.5, 6.,6.5/  
C
      DATA (ULGO(JU),JU=1,38)/
     S -17.5,-17.,-16.5,-16.,-15.5,-15.,-14.5,-14.,-13.5,-13.,
     S -12.5,-12.,-11.5,-11.,-10.5,-10.,-9.5,-9.,-8.5,-8.,
     S -7.5,-7.,-6.5,-6.,-5.5,-5.,-4.5,-4.,-3.5,-3.,-2.5,-2.,
     S -1.5,-1.,-0.5, 0.,0.5,1.0/
C
C----- -1,0E3*(ln(1-TB))    ( CO2 15 micron band )
      DATA (TB(I,1,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9017, 8647, 8315, 8020,
     S  7758, 7517, 7281, 7045, 6805, 6564, 6329, 6104, 5890, 5689,
     S  5496, 5309, 5127, 4950, 4777, 4607, 4440, 4276, 4115, 3956,
     S  3801, 3649, 3501, 3358, 3220, 3087, 2958, 2832, 2708, 2586/
      DATA (TB(I,2,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9017, 8647, 8315, 8020,
     S  7758, 7516, 7281, 7045, 6804, 6564, 6329, 6104, 5890, 5689,
     S  5496, 5309, 5127, 4949, 4776, 4606, 4439, 4274, 4112, 3953,
     S  3796, 3643, 3493, 3349, 3209, 3074, 2943, 2814, 2687, 2562/
      DATA (TB(I,3,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9017, 8647, 8315, 8020,
     S  7758, 7516, 7281, 7045, 6804, 6564, 6329, 6103, 5890, 5688,
     S  5495, 5308, 5126, 4948, 4774, 4604, 4436, 4271, 4108, 3947,
     S  3789, 3634, 3482, 3336, 3194, 3056, 2922, 2791, 2661, 2533/
      DATA (TB(I,4,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9017, 8647, 8314, 8020,
     S  7758, 7516, 7281, 7044, 6804, 6564, 6329, 6103, 5889, 5688,
     S  5494, 5307, 5125, 4946, 4772, 4601, 4432, 4266, 4101, 3938,
     S  3778, 3621, 3467, 3318, 3173, 3033, 2896, 2761, 2628, 2495/
      DATA (TB(I,5,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9017, 8647, 8314, 8020,
     S  7758, 7516, 7281, 7044, 6804, 6563, 6328, 6102, 5889, 5687,
     S  5493, 5305, 5122, 4943, 4768, 4596, 4426, 4257, 4091, 3926,
     S  3763, 3603, 3446, 3294, 3147, 3003, 2862, 2723, 2585, 2448/
      DATA (TB(I,6,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9017, 8647, 8314, 8020,
     S  7758, 7516, 7281, 7044, 6803, 6563, 6327, 6101, 5887, 5685,
     S  5491, 5302, 5119, 4938, 4762, 4588, 4416, 4245, 4075, 3907,
     S  3742, 3579, 3419, 3263, 3112, 2964, 2819, 2675, 2532, 2389/
      DATA (TB(I,7,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9016, 8646, 8314, 8020,
     S  7757, 7515, 7280, 7043, 6802, 6562, 6326, 6100, 5885, 5682,
     S  5487, 5298, 5113, 4931, 4752, 4576, 4401, 4227, 4054, 3882,
     S  3713, 3546, 3383, 3224, 3068, 2916, 2766, 2616, 2468, 2319/
      DATA (TB(I,8,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9016, 8646, 8314, 8019,
     S  7757, 7514, 7279, 7042, 6801, 6560, 6323, 6097, 5881, 5677,
     S  5481, 5290, 5103, 4919, 4737, 4557, 4379, 4201, 4024, 3849,
     S  3675, 3505, 3337, 3174, 3013, 2856, 2700, 2545, 2390, 2235/
      DATA (TB(I,9,1),I=1,40)/
     S 11718,11236,10761,10297, 9848, 9419, 9016, 8646, 8313, 8018,
     S  7756, 7513, 7277, 7039, 6798, 6557, 6320, 6092, 5876, 5670,
     S  5472, 5279, 5089, 4901, 4715, 4531, 4348, 4166, 3985, 3805,
     S  3627, 3452, 3280, 3111, 2946, 2782, 2620, 2459, 2298, 2138/
      DATA (TB(I,10,1),I=1,40)/
     S 11718,11235,10761,10297, 9848, 9418, 9015, 8645, 8312, 8017,
     S  7754, 7511, 7274, 7036, 6794, 6551, 6314, 6084, 5866, 5658,
     S  5457, 5260, 5066, 4874, 4684, 4495, 4306, 4119, 3933, 3749,
     S  3566, 3387, 3210, 3036, 2864, 2695, 2526, 2359, 2193, 2029/
      DATA (TB(I,11,1),I=1,40)/
     S 11718,11235,10761,10297, 9847, 9418, 9015, 8644, 8310, 8015,
     S  7751, 7507, 7270, 7030, 6787, 6543, 6304, 6072, 5851, 5640,
     S  5435, 5233, 5034, 4836, 4640, 4445, 4252, 4060, 3869, 3679,
     S  3492, 3307, 3125, 2945, 2768, 2592, 2418, 2246, 2077, 1911/
      DATA (TB(I,12,1),I=1,40)/
     S 11718,11235,10760,10296, 9847, 9417, 9013, 8642, 8308, 8011,
     S  7746, 7501, 7262, 7021, 6776, 6530, 6288, 6054, 5828, 5612,
     S  5401, 5194, 4988, 4784, 4582, 4382, 4183, 3985, 3789, 3594,
     S  3402, 3212, 3024, 2839, 2655, 2475, 2296, 2121, 1950, 1785/
      DATA (TB(I,13,1),I=1,40)/
     S 11718,11235,10760,10296, 9846, 9415, 9011, 8639, 8303, 8005,
     S  7738, 7491, 7250, 7007, 6759, 6510, 6264, 6024, 5794, 5571,
     S  5354, 5139, 4927, 4716, 4508, 4302, 4097, 3894, 3693, 3493,
     S  3296, 3101, 2907, 2717, 2529, 2344, 2163, 1987, 1817, 1655/
      DATA (TB(I,14,1),I=1,40)/
     S 11718,11235,10759,10294, 9844, 9413, 9007, 8633, 8296, 7995,
     S  7725, 7475, 7231, 6984, 6732, 6478, 6227, 5981, 5744, 5513,
     S  5288, 5066, 4847, 4631, 4416, 4204, 3994, 3786, 3580, 3376,
     S  3174, 2973, 2775, 2580, 2389, 2203, 2022, 1847, 1682, 1526/
      DATA (TB(I,15,1),I=1,40)/
     S 11718,11234,10758,10293, 9841, 9409, 9001, 8625, 8284, 7980,
     S  7706, 7450, 7201, 6949, 6691, 6432, 6173, 5920, 5674, 5436,
     S  5203, 4974, 4748, 4526, 4306, 4088, 3873, 3661, 3450, 3242,
     S  3035, 2831, 2629, 2432, 2239, 2053, 1875, 1705, 1547, 1400/
      DATA (TB(I,16,1),I=1,40)/
     S 11718,11233,10757,10290, 9837, 9402, 8992, 8612, 8266, 7956,
     S  7675, 7413, 7156, 6897, 6632, 6365, 6099, 5837, 5583, 5336,
     S  5096, 4861, 4629, 4401, 4176, 3954, 3735, 3519, 3305, 3093,
     S  2882, 2675, 2472, 2274, 2083, 1900, 1727, 1565, 1416, 1281/
      DATA (TB(I,17,1),I=1,40)/
     S 11717,11232,10754,10286, 9831, 9393, 8978, 8592, 8239, 7920,
     S  7630, 7357, 7091, 6823, 6550, 6275, 6000, 5731, 5468, 5215,
     S  4968, 4727, 4491, 4258, 4030, 3805, 3582, 3363, 3145, 2930,
     S  2718, 2509, 2306, 2111, 1924, 1747, 1583, 1431, 1294, 1171/
      DATA (TB(I,18,1),I=1,40)/
     S 11717,11231,10751,10280, 9821, 9379, 8957, 8563, 8200, 7869,
     S  7565, 7281, 7003, 6725, 6444, 6161, 5878, 5601, 5333, 5073,
     S  4821, 4576, 4336, 4100, 3869, 3641, 3416, 3194, 2974, 2757,
     S  2544, 2337, 2137, 1946, 1766, 1599, 1445, 1305, 1181, 1071/
      DATA (TB(I,19,1),I=1,40)/
     S 11716,11228,10746,10272, 9808, 9359, 8929, 8524, 8147, 7800,
     S  7480, 7181, 6891, 6603, 6314, 6024, 5736, 5453, 5179, 4914,
     S  4658, 4410, 4167, 3930, 3696, 3466, 3239, 3015, 2794, 2577,
     S  2366, 2163, 1968, 1785, 1614, 1458, 1317, 1191, 1080,  981/
      DATA (TB(I,20,1),I=1,40)/
     S 11715,11225,10740,10261, 9792, 9335, 8895, 8476, 8082, 7716,
     S  7377, 7060, 6757, 6460, 6164, 5869, 5577, 5290, 5012, 4744,
     S  4485, 4234, 3990, 3751, 3516, 3284, 3055, 2830, 2609, 2395,
     S  2188, 1990, 1803, 1630, 1471, 1328, 1201, 1089,  989,  899/
      DATA (TB(I,21,1),I=1,40)/
     S 11714,11221,10733,10250, 9774, 9308, 8856, 8422, 8010, 7623,
     S  7262, 6926, 6608, 6302, 6000, 5702, 5408, 5119, 4838, 4567,
     S  4306, 4053, 3808, 3567, 3330, 3096, 2867, 2642, 2424, 2214,
     S  2013, 1823, 1647, 1486, 1341, 1212, 1098,  997,  907,  823/
      DATA (TB(I,22,1),I=1,40)/
     S 11712,11217,10726,10238, 9756, 9282, 8819, 8370, 7939, 7530,
     S  7147, 6790, 6455, 6137, 5830, 5530, 5235, 4946, 4664, 4391,
     S  4128, 3873, 3624, 3381, 3142, 2907, 2678, 2456, 2242, 2038,
     S  1846, 1666, 1502, 1355, 1224, 1109, 1007,  915,  831,  752/
      DATA (TB(I,23,1),I=1,40)/
     S 11710,11213,10719,10228, 9741, 9259, 8786, 8324, 7877, 7449,
     S  7043, 6663, 6308, 5976, 5661, 5358, 5063, 4775, 4494, 4220,
     S  3954, 3695, 3443, 3196, 2955, 2720, 2493, 2275, 2067, 1871,
     S  1689, 1522, 1372, 1239, 1122, 1019,  926,  840,  761,  687/
      DATA (TB(I,24,1),I=1,40)/
     S 11708,11211,10714,10220, 9729, 9242, 8762, 8290, 7829, 7383,
     S  6957, 6554, 6177, 5827, 5500, 5191, 4896, 4609, 4329, 4055,
     S  3786, 3522, 3265, 3014, 2772, 2538, 2315, 2102, 1902, 1716,
     S  1546, 1393, 1258, 1139, 1034,  939,  852,  772,  697,  629/
      DATA (TB(I,25,1),I=1,40)/
     S 11707,11209,10711,10215, 9721, 9230, 8744, 8265, 7794, 7335,
     S  6892, 6469, 6070, 5699, 5355, 5036, 4736, 4449, 4170, 3895,
     S  3623, 3355, 3093, 2839, 2595, 2363, 2144, 1938, 1748, 1574,
     S  1418, 1280, 1159, 1053,  956,  868,  786,  710,  640,  576/
      DATA (TB(I,26,1),I=1,40)/
     S 11707,11208,10709,10212, 9716, 9223, 8733, 8248, 7770, 7301,
     S  6845, 6406, 5989, 5597, 5234, 4900, 4590, 4297, 4015, 3739,
     S  3465, 3194, 2928, 2672, 2427, 2198, 1984, 1787, 1608, 1448,
     S  1307, 1183, 1075,  977,  887,  803,  726,  654,  589,  531/
      DATA (TB(I,27,1),I=1,40)/
     S 11707,11208,10709,10211, 9714, 9218, 8726, 8238, 7755, 7280,
     S  6815, 6364, 5932, 5522, 5140, 4787, 4460, 4156, 3866, 3586,
     S  3309, 3037, 2771, 2514, 2270, 2044, 1836, 1649, 1482, 1337,
     S  1211, 1101, 1001,  910,  825,  745,  672,  605,  546,  493/
      DATA (TB(I,28,1),I=1,40)/
     S 11707,11208,10709,10210, 9713, 9216, 8722, 8232, 7746, 7266,
     S  6795, 6336, 5893, 5470, 5071, 4699, 4353, 4031, 3727, 3438,
     S  3157, 2884, 2619, 2364, 2124, 1903, 1703, 1527, 1374, 1242,
     S  1129, 1029,  936,  850,  769,  693,  625,  564,  510,  461/
      DATA (TB(I,29,1),I=1,40)/
     S 11708,11209,10710,10211, 9713, 9216, 8721, 8229, 7741, 7258,
     S  6783, 6318, 5867, 5434, 5021, 4632, 4267, 3925, 3603, 3299,
     S  3010, 2735, 2472, 2222, 1989, 1777, 1588, 1423, 1283, 1163,
     S  1059,  964,  877,  795,  718,  648,  585,  530,  481,  435/
      DATA (TB(I,30,1),I=1,40)/
     S 11710,11211,10711,10212, 9714, 9217, 8721, 8228, 7738, 7253,
     S  6775, 6306, 5848, 5407, 4983, 4581, 4199, 3838, 3496, 3174,
     S  2873, 2592, 2330, 2087, 1864, 1665, 1490, 1340, 1211, 1099,
     S   998,  907,  823,  745,  673,  609,  553,  503,  457,  412/
      DATA (TB(I,31,1),I=1,40)/
     S 11713,11214,10714,10215, 9717, 9219, 8723, 8228, 7737, 7250,
     S  6769, 6296, 5833, 5384, 4952, 4538, 4143, 3766, 3407, 3067,
     S  2751, 2461, 2198, 1962, 1752, 1569, 1412, 1277, 1157, 1048,
     S   948,  857,  774,  700,  635,  578,  528,  482,  438,  393/
C----- -1,0E3*(ln(1-TDB)) 
      DATA (TB(I,1,2),I=1,40)/
     S 11682,11199,10724,10261, 9812, 9383, 8980, 8610, 8278, 7984,
     S  7722, 7480, 7245, 7009, 6769, 6529, 6295, 6070, 5857, 5655,
     S  5463, 5276, 5094, 4917, 4744, 4575, 4408, 4244, 4083, 3924,
     S  3769, 3617, 3470, 3328, 3190, 3058, 2929, 2803, 2679, 2557/
      DATA (TB(I,2,2),I=1,40)/
     S 11682,11199,10724,10261, 9812, 9383, 8980, 8610, 8278, 7984,
     S  7722, 7480, 7245, 7009, 6769, 6529, 6294, 6069, 5856, 5655,
     S  5462, 5276, 5094, 4916, 4743, 4573, 4407, 4242, 4080, 3921,
     S  3764, 3611, 3463, 3318, 3179, 3044, 2913, 2785, 2658, 2533/
      DATA (TB(I,3,2),I=1,40)/
     S 11682,11199,10724,10261, 9812, 9383, 8980, 8610, 8278, 7984,
     S  7722, 7480, 7245, 7009, 6769, 6529, 6294, 6069, 5856, 5655,
     S  5462, 5275, 5093, 4915, 4742, 4571, 4404, 4239, 4076, 3915,
     S  3757, 3602, 3452, 3305, 3164, 3026, 2893, 2761, 2632, 2504/
      DATA (TB(I,4,2),I=1,40)/
     S 11682,11199,10724,10261, 9812, 9383, 8980, 8610, 8278, 7984,
     S  7721, 7480, 7245, 7008, 6769, 6529, 6294, 6069, 5856, 5654,
     S  5461, 5274, 5092, 4913, 4739, 4568, 4400, 4233, 4069, 3906,
     S  3746, 3590, 3436, 3288, 3143, 3003, 2866, 2731, 2598, 2466/
      DATA (TB(I,5,2),I=1,40)/
     S 11682,11199,10724,10261, 9812, 9383, 8980, 8610, 8278, 7983,
     S  7721, 7480, 7245, 7008, 6768, 6528, 6293, 6068, 5855, 5653,
     S  5460, 5272, 5089, 4910, 4735, 4563, 4393, 4225, 4059, 3894,
     S  3731, 3572, 3415, 3264, 3116, 2973, 2832, 2693, 2555, 2418/
      DATA (TB(I,6,2),I=1,40)/
     S 11682,11199,10724,10261, 9812, 9383, 8980, 8610, 8278, 7983,
     S  7721, 7479, 7244, 7008, 6768, 6528, 6293, 6067, 5853, 5651,
     S  5457, 5269, 5086, 4906, 4729, 4555, 4383, 4213, 4043, 3875,
     S  3710, 3547, 3388, 3233, 3081, 2934, 2789, 2645, 2502, 2359/
      DATA (TB(I,7,2),I=1,40)/
     S 11682,11199,10724,10261, 9812, 9383, 8980, 8610, 8277, 7983,
     S  7721, 7479, 7244, 7007, 6767, 6527, 6291, 6065, 5851, 5648,
     S  5454, 5265, 5080, 4898, 4719, 4543, 4368, 4194, 4022, 3850,
     S  3681, 3515, 3352, 3193, 3037, 2885, 2735, 2586, 2437, 2288/
      DATA (TB(I,8,2),I=1,40)/
     S 11682,11199,10724,10261, 9812, 9382, 8980, 8610, 8277, 7982,
     S  7720, 7478, 7243, 7006, 6765, 6525, 6289, 6062, 5848, 5644,
     S  5448, 5257, 5070, 4886, 4705, 4525, 4346, 4168, 3992, 3817,
     S  3643, 3473, 3306, 3142, 2982, 2825, 2669, 2514, 2359, 2204/
      DATA (TB(I,9,2),I=1,40)/
     S 11682,11199,10724,10260, 9811, 9382, 8979, 8609, 8276, 7982,
     S  7719, 7476, 7241, 7004, 6763, 6522, 6285, 6058, 5842, 5636,
     S  5439, 5245, 5055, 4868, 4682, 4498, 4315, 4133, 3952, 3772,
     S  3595, 3420, 3248, 3080, 2914, 2751, 2589, 2428, 2267, 2107/
      DATA (TB(I,10,2),I=1,40)/
     S 11682,11199,10724,10260, 9811, 9382, 8979, 8608, 8275, 7980,
     S  7717, 7474, 7238, 7000, 6758, 6517, 6279, 6050, 5832, 5625,
     S  5424, 5227, 5033, 4841, 4651, 4462, 4274, 4087, 3901, 3716,
     S  3534, 3355, 3178, 3004, 2833, 2663, 2495, 2328, 2162, 1998/
      DATA (TB(I,11,2),I=1,40)/
     S 11682,11199,10724,10260, 9811, 9381, 8978, 8607, 8274, 7978,
     S  7714, 7470, 7233, 6994, 6752, 6508, 6269, 6038, 5818, 5606,
     S  5401, 5200, 5001, 4803, 4607, 4413, 4219, 4027, 3836, 3646,
     S  3459, 3275, 3093, 2913, 2736, 2560, 2386, 2214, 2045, 1879/
      DATA (TB(I,12,2),I=1,40)/
     S 11682,11199,10724,10260, 9810, 9380, 8977, 8605, 8271, 7974,
     S  7709, 7464, 7226, 6985, 6741, 6495, 6253, 6019, 5794, 5578,
     S  5368, 5160, 4955, 4751, 4549, 4349, 4150, 3952, 3756, 3561,
     S  3369, 3179, 2992, 2807, 2623, 2442, 2264, 2089, 1918, 1753/
      DATA (TB(I,13,2),I=1,40)/
     S 11682,11199,10723,10259, 9809, 9379, 8974, 8602, 8266, 7968,
     S  7701, 7454, 7214, 6971, 6723, 6475, 6229, 5990, 5760, 5537,
     S  5320, 5105, 4893, 4683, 4475, 4269, 4064, 3861, 3659, 3460,
     S  3263, 3068, 2875, 2684, 2496, 2311, 2131, 1955, 1785, 1623/
      DATA (TB(I,14,2),I=1,40)/
     S 11682,11198,10723,10258, 9807, 9376, 8971, 8597, 8259, 7958,
     S  7689, 7439, 7194, 6948, 6696, 6443, 6192, 5947, 5710, 5479,
     S  5254, 5033, 4814, 4597, 4383, 4171, 3961, 3752, 3546, 3342,
     S  3140, 2940, 2743, 2548, 2356, 2170, 1989, 1815, 1649, 1493/
      DATA (TB(I,15,2),I=1,40)/
     S 11681,11198,10722,10256, 9805, 9372, 8965, 8588, 8248, 7943,
     S  7669, 7414, 7165, 6913, 6656, 6396, 6138, 5886, 5640, 5401,
     S  5169, 4940, 4714, 4492, 4272, 4054, 3840, 3627, 3417, 3208,
     S  3002, 2798, 2596, 2399, 2206, 2020, 1842, 1672, 1514, 1367/
      DATA (TB(I,16,2),I=1,40)/
     S 11681,11197,10720,10253, 9800, 9366, 8955, 8575, 8230, 7919,
     S  7638, 7376, 7120, 6861, 6597, 6330, 6064, 5802, 5548, 5302,
     S  5062, 4826, 4595, 4367, 4142, 3921, 3702, 3485, 3271, 3059,
     S  2849, 2642, 2438, 2241, 2050, 1867, 1694, 1532, 1383, 1248/
      DATA (TB(I,17,2),I=1,40)/
     S 11681,11196,10718,10249, 9794, 9356, 8941, 8555, 8202, 7883,
     S  7593, 7321, 7055, 6787, 6515, 6240, 5965, 5696, 5434, 5180,
     S  4934, 4693, 4457, 4224, 3996, 3771, 3549, 3329, 3111, 2896,
     S  2684, 2476, 2273, 2078, 1891, 1714, 1549, 1398, 1261, 1138/
      DATA (TB(I,18,2),I=1,40)/
     S 11680,11194,10714,10243, 9784, 9342, 8921, 8526, 8163, 7832,
     S  7529, 7244, 6967, 6689, 6408, 6125, 5843, 5566, 5298, 5038,
     S  4787, 4541, 4301, 4066, 3835, 3607, 3382, 3160, 2940, 2723,
     S  2510, 2303, 2103, 1913, 1733, 1565, 1411, 1272, 1148, 1038/
      DATA (TB(I,19,2),I=1,40)/
     S 11680,11191,10709,10235, 9772, 9322, 8893, 8487, 8110, 7763,
     S  7444, 7144, 6854, 6567, 6278, 5988, 5700, 5418, 5144, 4879,
     S  4624, 4375, 4133, 3895, 3662, 3432, 3205, 2981, 2760, 2543,
     S  2332, 2129, 1934, 1751, 1581, 1424, 1283, 1158, 1047,  949/
      DATA (TB(I,20,2),I=1,40)/
     S 11679,11188,10703,10225, 9755, 9298, 8858, 8439, 8045, 7679,
     S  7340, 7024, 6720, 6424, 6128, 5833, 5541, 5255, 4977, 4709,
     S  4450, 4199, 3955, 3716, 3481, 3249, 3021, 2795, 2575, 2360,
     S  2153, 1956, 1769, 1596, 1437, 1294, 1168, 1055,  956,  866/
      DATA (TB(I,21,2),I=1,40)/
     S 11677,11185,10696,10213, 9737, 9271, 8819, 8385, 7973, 7586,
     S  7226, 6890, 6572, 6265, 5964, 5666, 5372, 5084, 4803, 4532,
     S  4271, 4019, 3773, 3532, 3295, 3062, 2832, 2608, 2390, 2179,
     S  1979, 1789, 1613, 1452, 1307, 1178, 1065,  964,  874,  790/
      DATA (TB(I,22,2),I=1,40)/
     S 11675,11180,10689,10201, 9719, 9245, 8782, 8333, 7902, 7494,
     S  7110, 6753, 6419, 6101, 5794, 5494, 5199, 4910, 4629, 4356,
     S  4093, 3838, 3590, 3346, 3107, 2873, 2644, 2422, 2208, 2004,
     S  1811, 1632, 1468, 1321, 1190, 1075,  974,  882,  798,  720/
      DATA (TB(I,23,2),I=1,40)/
     S 11673,11176,10682,10191, 9704, 9222, 8749, 8287, 7840, 7412,
     S  7006, 6626, 6271, 5939, 5624, 5321, 5027, 4739, 4458, 4185,
     S  3918, 3660, 3407, 3161, 2920, 2686, 2459, 2240, 2033, 1837,
     S  1654, 1488, 1338, 1205, 1089,  985,  893,  807,  728,  655/
      DATA (TB(I,24,2),I=1,40)/
     S 11671,11173,10677,10183, 9692, 9205, 8725, 8253, 7792, 7346,
     S  6920, 6517, 6141, 5790, 5463, 5155, 4859, 4573, 4293, 4019,
     S  3750, 3487, 3229, 2979, 2737, 2503, 2280, 2067, 1867, 1681,
     S  1511, 1359, 1223, 1105, 1000,  906,  819,  739,  664,  596/
      DATA (TB(I,25,2),I=1,40)/
     S 11670,11172,10674,10178, 9684, 9193, 8707, 8228, 7757, 7298,
     S  6855, 6432, 6033, 5662, 5319, 5000, 4700, 4413, 4134, 3859,
     S  3587, 3319, 3057, 2803, 2560, 2328, 2109, 1903, 1713, 1539,
     S  1383, 1246, 1125, 1018,  922,  834,  752,  677,  607,  544/
      DATA (TB(I,26,2),I=1,40)/
     S 11670,11171,10672,10175, 9679, 9186, 8696, 8211, 7733, 7264,
     S  6808, 6369, 5952, 5560, 5197, 4863, 4553, 4260, 3979, 3702,
     S  3429, 3158, 2893, 2636, 2392, 2162, 1948, 1751, 1573, 1413,
     S  1272, 1149, 1040,  943,  853,  769,  692,  621,  556,  499/
      DATA (TB(I,27,2),I=1,40)/
     S 11670,11170,10672,10173, 9677, 9181, 8689, 8200, 7718, 7242,
     S  6778, 6327, 5895, 5485, 5103, 4750, 4424, 4119, 3830, 3549,
     S  3273, 3001, 2735, 2478, 2234, 2008, 1800, 1613, 1447, 1302,
     S  1176, 1066,  967,  876,  790,  711,  638,  572,  513,  461/
      DATA (TB(I,28,2),I=1,40)/
     S 11670,11171,10672,10173, 9676, 9179, 8685, 8195, 7709, 7229,
     S  6758, 6299, 5856, 5433, 5034, 4662, 4316, 3994, 3691, 3401,
     S  3121, 2848, 2583, 2328, 2088, 1867, 1667, 1491, 1338, 1207,
     S  1094,  994,  901,  815,  734,  659,  591,  530,  477,  430/
      DATA (TB(I,29,2),I=1,40)/
     S 11671,11172,10673,10174, 9676, 9179, 8684, 8192, 7704, 7221,
     S  6746, 6281, 5830, 5397, 4984, 4595, 4230, 3888, 3566, 3262,
     S  2973, 2698, 2435, 2186, 1953, 1741, 1552, 1388, 1247, 1128,
     S  1024,  929,  842,  760,  684,  614,  551,  497,  448,  403/
      DATA (TB(I,30,2),I=1,40)/
     S 11673,11174,10674,10175, 9677, 9180, 8684, 8191, 7701, 7216,
     S  6738, 6269, 5811, 5369, 4946, 4543, 4162, 3800, 3458, 3137,
     S  2836, 2555, 2294, 2050, 1828, 1629, 1454, 1304, 1175, 1063,
     S   963,  872,  788,  710,  639,  575,  519,  470,  425,  381/
      DATA (TB(I,31,2),I=1,40)/
     S 11676,11177,10677,10178, 9680, 9182, 8686, 8191, 7700, 7213,
     S  6732, 6259, 5796, 5347, 4914, 4501, 4106, 3729, 3370, 3030,
     S  2714, 2424, 2161, 1925, 1715, 1533, 1376, 1241, 1121, 1012,
     S   912,  821,  739,  666,  600,  544,  494,  449,  405,  361/
C
C----- 1.,0E4*ALP(I,J,1) FOR TB 
      DATA (ALP(I, 1,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  72.,  74.,  76.,  79.,  81.,  84.,  86.,  88.,  90.,  91.,
     S  92.,  93.,  93.,  92.,  92.,  92.,  91.,  90.,  89.,  88./
      DATA (ALP(I, 2,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  72.,  74.,  76.,  79.,  81.,  83.,  86.,  88.,  89.,  91.,
     S  92.,  92.,  92.,  91.,  91.,  90.,  89.,  89.,  88.,  86./
      DATA (ALP(I, 3,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  72.,  74.,  76.,  79.,  81.,  83.,  85.,  87.,  89.,  90.,
     S  91.,  91.,  90.,  90.,  89.,  88.,  88.,  87.,  85.,  83./
      DATA (ALP(I, 4,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  72.,  74.,  76.,  78.,  81.,  83.,  85.,  87.,  88.,  89.,
     S  89.,  89.,  89.,  88.,  87.,  86.,  85.,  84.,  82.,  80./
      DATA (ALP(I, 5,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  72.,  74.,  76.,  78.,  80.,  82.,  84.,  86.,  87.,  87.,
     S  88.,  87.,  87.,  86.,  85.,  83.,  82.,  81.,  79.,  77./
      DATA (ALP(I, 6,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  68.,  70.,
     S  71.,  73.,  75.,  77.,  79.,  81.,  83.,  84.,  85.,  85.,
     S  85.,  85.,  84.,  83.,  82.,  80.,  79.,  77.,  75.,  72./
      DATA (ALP(I, 7,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  68.,  69.,
     S  71.,  73.,  75.,  76.,  78.,  80.,  81.,  82.,  83.,  83.,
     S  83.,  82.,  81.,  80.,  78.,  77.,  75.,  73.,  71.,  68./
      DATA (ALP(I, 8,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  53.,  59.,  63.,  65.,  67.,  67.,  68.,  69.,
     S  70.,  72.,  74.,  75.,  77.,  78.,  79.,  80.,  80.,  80.,
     S  80.,  79.,  77.,  76.,  74.,  73.,  71.,  69.,  66.,  63./
      DATA (ALP(I, 9,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  53.,  59.,  63.,  65.,  66.,  67.,  67.,  68.,
     S  69.,  71.,  72.,  73.,  74.,  75.,  76.,  76.,  77.,  76.,
     S  76.,  75.,  73.,  72.,  70.,  68.,  66.,  64.,  61.,  58./
      DATA (ALP(I,10,1),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  31.,
     S  39.,  46.,  53.,  58.,  62.,  65.,  66.,  66.,  66.,  67.,
     S  68.,  69.,  70.,  71.,  71.,  72.,  73.,  73.,  73.,  72.,
     S  72.,  71.,  69.,  67.,  65.,  63.,  61.,  59.,  56.,  53./
      DATA (ALP(I,11,1),I=1,40)/
     S   1.,   1.,   2.,   4.,   6.,   9.,  13.,  18.,  24.,  31.,
     S  39.,  46.,  52.,  58.,  61.,  64.,  65.,  65.,  65.,  65.,
     S  65.,  66.,  67.,  67.,  68.,  68.,  69.,  69.,  68.,  68.,
     S  67.,  66.,  64.,  62.,  60.,  58.,  56.,  54.,  51.,  49./
      DATA (ALP(I,12,1),I=1,40)/
     S   1.,   1.,   2.,   4.,   6.,   9.,  13.,  18.,  24.,  31.,
     S  38.,  45.,  52.,  57.,  60.,  62.,  63.,  63.,  62.,  62.,
     S  62.,  63.,  63.,  63.,  64.,  64.,  64.,  64.,  64.,  63.,
     S  62.,  61.,  59.,  57.,  55.,  53.,  51.,  49.,  47.,  44./
      DATA (ALP(I,13,1),I=1,40)/
     S   1.,   1.,   2.,   4.,   6.,   9.,  13.,  17.,  23.,  30.,
     S  38.,  44.,  50.,  55.,  58.,  60.,  61.,  60.,  59.,  59.,
     S  59.,  59.,  59.,  59.,  59.,  59.,  59.,  59.,  59.,  58.,
     S  57.,  56.,  54.,  52.,  50.,  48.,  47.,  45.,  43.,  41./
      DATA (ALP(I,14,1),I=1,40)/
     S   1.,   1.,   2.,   4.,   6.,   9.,  12.,  17.,  23.,  29.,
     S  36.,  43.,  49.,  53.,  56.,  57.,  57.,  56.,  55.,  54.,
     S  54.,  54.,  54.,  54.,  54.,  54.,  55.,  54.,  54.,  53.,
     S  52.,  51.,  50.,  48.,  46.,  44.,  43.,  41.,  39.,  38./
      DATA (ALP(I,15,1),I=1,40)/
     S   0.,   1.,   2.,   4.,   6.,   8.,  12.,  16.,  22.,  28.,
     S  35.,  41.,  46.,  50.,  52.,  53.,  53.,  52.,  51.,  50.,
     S  49.,  49.,  49.,  49.,  49.,  50.,  50.,  50.,  49.,  49.,
     S  48.,  47.,  45.,  44.,  42.,  41.,  39.,  38.,  37.,  36./
      DATA (ALP(I,16,1),I=1,40)/
     S   0.,   1.,   2.,   3.,   5.,   8.,  11.,  15.,  20.,  26.,
     S  32.,  37.,  42.,  45.,  47.,  48.,  47.,  46.,  45.,  44.,
     S  44.,  44.,  44.,  44.,  45.,  45.,  45.,  45.,  45.,  44.,
     S  44.,  42.,  41.,  40.,  39.,  37.,  36.,  36.,  35.,  34./
      DATA (ALP(I,17,1),I=1,40)/
     S   0.,   1.,   2.,   3.,   5.,   7.,  10.,  13.,  18.,  23.,
     S  28.,  33.,  37.,  40.,  41.,  42.,  42.,  41.,  40.,  39.,
     S  39.,  39.,  40.,  40.,  41.,  41.,  41.,  41.,  41.,  41.,
     S  40.,  39.,  38.,  37.,  36.,  35.,  34.,  34.,  34.,  34./
      DATA (ALP(I,18,1),I=1,40)/
     S   0.,   1.,   1.,   2.,   4.,   6.,   8.,  11.,  15.,  19.,
     S  24.,  28.,  31.,  33.,  35.,  36.,  36.,  36.,  35.,  35.,
     S  35.,  35.,  36.,  37.,  37.,  38.,  38.,  38.,  38.,  37.,
     S  37.,  36.,  35.,  34.,  34.,  33.,  33.,  33.,  33.,  33./
      DATA (ALP(I,19,1),I=1,40)/
     S   0.,   1.,   1.,   2.,   3.,   4.,   6.,   9.,  12.,  15.,
     S  19.,  22.,  25.,  27.,  29.,  30.,  31.,  31.,  31.,  31.,
     S  32.,  32.,  33.,  34.,  34.,  35.,  35.,  35.,  35.,  35.,
     S  34.,  34.,  33.,  33.,  32.,  32.,  32.,  32.,  33.,  33./
      DATA (ALP(I,20,1),I=1,40)/
     S   0.,   0.,   1.,   1.,   2.,   3.,   4.,   6.,   8.,  11.,
     S  13.,  16.,  19.,  21.,  23.,  25.,  27.,  28.,  28.,  29.,
     S  29.,  30.,  30.,  31.,  32.,  32.,  33.,  33.,  33.,  32.,
     S  32.,  32.,  32.,  31.,  31.,  32.,  32.,  32.,  33.,  33./
      DATA (ALP(I,21,1),I=1,40)/
     S   0.,   0.,   0.,   1.,   1.,   2.,   3.,   4.,   5.,   7.,
     S   9.,  11.,  14.,  16.,  19.,  21.,  23.,  25.,  26.,  27.,
     S  27.,  28.,  29.,  29.,  30.,  30.,  31.,  31.,  31.,  31.,
     S  31.,  31.,  31.,  31.,  31.,  31.,  32.,  32.,  32.,  32./
      DATA (ALP(I,22,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   3.,   4.,
     S   6.,   8.,  10.,  12.,  15.,  18.,  21.,  23.,  25.,  26.,
     S  26.,  27.,  27.,  28.,  28.,  29.,  29.,  29.,  29.,  30.,
     S  30.,  30.,  30.,  30.,  31.,  32.,  32.,  32.,  32.,  31./
      DATA (ALP(I,23,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   2.,
     S   4.,   5.,   7.,   9.,  12.,  15.,  18.,  21.,  23.,  25.,
     S  26.,  26.,  26.,  27.,  27.,  27.,  27.,  28.,  28.,  28.,
     S  29.,  29.,  30.,  31.,  31.,  32.,  32.,  31.,  31.,  30./
      DATA (ALP(I,24,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,
     S   2.,   4.,   5.,   7.,  10.,  13.,  16.,  19.,  21.,  23.,
     S  24.,  25.,  25.,  26.,  26.,  26.,  26.,  27.,  27.,  28.,
     S  28.,  29.,  30.,  31.,  31.,  32.,  31.,  31.,  30.,  29./
      DATA (ALP(I,25,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,
     S   2.,   3.,   4.,   6.,   8.,  10.,  13.,  16.,  19.,  21.,
     S  23.,  24.,  24.,  25.,  25.,  25.,  25.,  25.,  26.,  27.,
     S  28.,  29.,  30.,  31.,  31.,  31.,  31.,  30.,  29.,  28./
      DATA (ALP(I,26,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,
     S   2.,   2.,   4.,   5.,   7.,   9.,  11.,  14.,  16.,  19.,
     S  21.,  22.,  23.,  24.,  24.,  24.,  24.,  25.,  26.,  27.,
     S  28.,  29.,  30.,  31.,  31.,  30.,  30.,  29.,  28.,  27./
      DATA (ALP(I,27,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,
     S   2.,   2.,   3.,   5.,   6.,   8.,  10.,  12.,  14.,  16.,
     S  18.,  20.,  21.,  22.,  23.,  23.,  24.,  24.,  25.,  27.,
     S  28.,  29.,  30.,  30.,  30.,  30.,  29.,  28.,  27.,  26./
      DATA (ALP(I,28,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,
     S   1.,   2.,   3.,   4.,   6.,   7.,   8.,  10.,  11.,  13.,
     S  15.,  17.,  19.,  20.,  21.,  22.,  23.,  24.,  26.,  27.,
     S  28.,  29.,  29.,  29.,  29.,  28.,  28.,  27.,  26.,  25./
      DATA (ALP(I,29,1),I=1,40)/
     S  -1.,  -1.,  -1.,  -1.,   0.,   0.,   0.,   0.,   0.,   1.,
     S   1.,   2.,   3.,   4.,   5.,   6.,   7.,   8.,   9.,  10.,
     S  12.,  14.,  16.,  18.,  20.,  21.,  23.,  25.,  26.,  27.,
     S  28.,  28.,  28.,  28.,  28.,  27.,  26.,  25.,  25.,  25./
      DATA (ALP(I,30,1),I=1,40)/
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   0.,   1.,   2.,   2.,   3.,   4.,   5.,   6.,   6.,   7.,
     S   9.,  10.,  13.,  15.,  18.,  20.,  23.,  26.,  27.,  28.,
     S  28.,  28.,  27.,  27.,  26.,  25.,  25.,  24.,  25.,  25./
      DATA (ALP(I,31,1),I=1,40)/
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S   0.,   0.,   1.,   1.,   2.,   2.,   3.,   4.,   4.,   5.,
     S   6.,   8.,  10.,  13.,  17.,  20.,  24.,  27.,  28.,  29.,
     S  28.,  28.,  26.,  25.,  24.,  24.,  23.,  24.,  24.,  25./
C----- 1.,0E6*BET(I,J,1) FOR TB 
      DATA (BET(I, 1,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  19.,  20.,  21.,  24.,  26.,  28.,  29.,  30.,
     S  29.,  29.,  27.,  26.,  26.,  25.,  26.,  26.,  25.,  23./
      DATA (BET(I, 2,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  19.,  20.,  21.,  24.,  26.,  28.,  29.,  29.,
     S  29.,  28.,  27.,  26.,  25.,  25.,  25.,  25.,  24.,  23./
      DATA (BET(I, 3,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  19.,  20.,  21.,  24.,  26.,  28.,  29.,  29.,
     S  29.,  28.,  27.,  26.,  25.,  25.,  25.,  24.,  24.,  22./
      DATA (BET(I, 4,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  19.,  20.,  21.,  23.,  26.,  28.,  29.,  29.,
     S  29.,  28.,  27.,  25.,  24.,  24.,  24.,  24.,  23.,  21./
      DATA (BET(I, 5,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  19.,  20.,  21.,  23.,  26.,  27.,  28.,  29.,
     S  28.,  27.,  26.,  25.,  24.,  23.,  23.,  23.,  22.,  20./
      DATA (BET(I, 6,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  19.,  20.,  21.,  23.,  25.,  27.,  28.,  28.,
     S  28.,  27.,  25.,  24.,  23.,  22.,  22.,  22.,  21.,  19./
      DATA (BET(I, 7,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  19.,  20.,  21.,  23.,  25.,  27.,  27.,  27.,
     S  27.,  26.,  24.,  23.,  22.,  21.,  21.,  20.,  19.,  17./
      DATA (BET(I, 8,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  19.,  20.,  21.,  23.,  25.,  26.,  26.,  26.,
     S  26.,  24.,  23.,  21.,  20.,  20.,  19.,  19.,  17.,  16./
      DATA (BET(I, 9,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  18.,  19.,  21.,  22.,  24.,  25.,  25.,  25.,
     S  24.,  23.,  21.,  20.,  19.,  18.,  18.,  17.,  16.,  14./
      DATA (BET(I,10,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,   9.,  10.,  12.,  14.,
     S  16.,  18.,  18.,  19.,  20.,  21.,  23.,  24.,  24.,  23.,
     S  23.,  21.,  20.,  18.,  17.,  17.,  16.,  15.,  14.,  12./
      DATA (BET(I,11,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,  10.,  11.,  12.,  14.,
     S  16.,  17.,  18.,  18.,  19.,  20.,  21.,  22.,  22.,  22.,
     S  21.,  20.,  18.,  17.,  16.,  15.,  14.,  13.,  12.,  10./
      DATA (BET(I,12,1),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,  10.,  11.,  12.,  14.,
     S  16.,  17.,  17.,  17.,  18.,  19.,  20.,  20.,  20.,  20.,
     S  19.,  18.,  16.,  15.,  14.,  13.,  12.,  11.,  10.,   9./
      DATA (BET(I,13,1),I=1,40)/
     S   0.,   0.,   0.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,  10.,  11.,  12.,  14.,
     S  15.,  16.,  16.,  16.,  17.,  17.,  18.,  18.,  18.,  18.,
     S  17.,  16.,  14.,  13.,  12.,  11.,  11.,  10.,   9.,   7./
      DATA (BET(I,14,1),I=1,40)/
     S   0.,   0.,   0.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   2.,
     S   3.,   5.,   7.,   8.,   9.,   9.,  10.,  10.,  12.,  13.,
     S  14.,  15.,  15.,  15.,  15.,  16.,  16.,  16.,  16.,  16.,
     S  15.,  14.,  13.,  11.,  10.,  10.,   9.,   8.,   7.,   6./
      DATA (BET(I,15,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   2.,
     S   4.,   6.,   7.,   9.,   9.,   9.,  10.,  10.,  11.,  12.,
     S  13.,  13.,  13.,  13.,  13.,  14.,  14.,  14.,  14.,  14.,
     S  13.,  12.,  11.,  10.,   9.,   8.,   7.,   7.,   6.,   4./
      DATA (BET(I,16,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   2.,
     S   4.,   6.,   7.,   9.,   9.,   9.,   9.,   9.,  10.,  11.,
     S  11.,  12.,  12.,  12.,  12.,  12.,  12.,  12.,  12.,  12.,
     S  11.,  10.,   9.,   8.,   7.,   7.,   6.,   5.,   4.,   3./
      DATA (BET(I,17,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   3.,
     S   4.,   6.,   8.,   9.,   9.,   9.,   8.,   8.,   9.,   9.,
     S  10.,  10.,  10.,  10.,  10.,  10.,  10.,  11.,  10.,  10.,
     S   9.,   9.,   8.,   7.,   6.,   6.,   5.,   4.,   3.,   2./
      DATA (BET(I,18,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   3.,
     S   5.,   6.,   7.,   8.,   8.,   8.,   8.,   7.,   7.,   8.,
     S   8.,   9.,   9.,   9.,   9.,   9.,   9.,   9.,   9.,   8.,
     S   8.,   7.,   7.,   6.,   5.,   4.,   4.,   3.,   2.,   0./
      DATA (BET(I,19,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   2.,   3.,
     S   5.,   6.,   7.,   8.,   8.,   7.,   7.,   6.,   6.,   6.,
     S   7.,   7.,   7.,   8.,   8.,   8.,   8.,   8.,   8.,   7.,
     S   7.,   6.,   5.,   5.,   4.,   3.,   2.,   1.,   0.,  -1./
      DATA (BET(I,20,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   2.,   3.,
     S   4.,   5.,   6.,   7.,   7.,   6.,   6.,   5.,   5.,   5.,
     S   5.,   6.,   6.,   7.,   7.,   7.,   7.,   7.,   6.,   6.,
     S   6.,   5.,   5.,   4.,   3.,   2.,   1.,   0.,  -1.,  -2./
      DATA (BET(I,21,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   2.,   3.,
     S   4.,   5.,   5.,   6.,   6.,   6.,   5.,   5.,   4.,   4.,
     S   4.,   5.,   5.,   6.,   6.,   6.,   6.,   6.,   5.,   5.,
     S   5.,   4.,   4.,   3.,   2.,   1.,   0.,  -1.,  -2.,  -2./
      DATA (BET(I,22,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   2.,
     S   3.,   4.,   4.,   5.,   5.,   5.,   5.,   4.,   4.,   3.,
     S   4.,   4.,   5.,   5.,   5.,   5.,   5.,   5.,   5.,   4.,
     S   4.,   4.,   3.,   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3./
      DATA (BET(I,23,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,
     S   2.,   3.,   3.,   4.,   4.,   5.,   5.,   4.,   4.,   4.,
     S   3.,   3.,   4.,   4.,   5.,   5.,   5.,   4.,   4.,   4.,
     S   3.,   3.,   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4./
      DATA (BET(I,24,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,
     S   1.,   2.,   2.,   3.,   3.,   4.,   4.,   4.,   4.,   4.,
     S   4.,   3.,   3.,   3.,   4.,   4.,   4.,   4.,   4.,   3.,
     S   3.,   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4.,  -4./
      DATA (BET(I,25,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,
     S   1.,   1.,   1.,   2.,   2.,   3.,   4.,   4.,   5.,   4.,
     S   4.,   3.,   3.,   3.,   3.,   3.,   3.,   3.,   3.,   2.,
     S   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4.,  -4.,  -4./
      DATA (BET(I,26,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   1.,   1.,   2.,   2.,   3.,   4.,   4.,   5.,
     S   4.,   4.,   3.,   2.,   2.,   2.,   2.,   2.,   2.,   1.,
     S   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4.,  -4.,  -4.,  -3./
      DATA (BET(I,27,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   1.,   1.,   2.,   3.,   4.,   4.,
     S   4.,   4.,   4.,   3.,   2.,   2.,   1.,   1.,   1.,   0.,
     S   0.,  -1.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -3.,  -2./
      DATA (BET(I,28,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   1.,   2.,   2.,   3.,   4.,
     S   4.,   4.,   4.,   3.,   2.,   1.,   1.,   0.,  -1.,  -1.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -3.,  -4.,  -3.,  -2.,  -2./
      DATA (BET(I,29,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   2.,   3.,
     S   4.,   4.,   4.,   3.,   2.,   1.,   0.,  -1.,  -2.,  -3.,
     S  -3.,  -3.,  -3.,  -3.,  -3.,  -3.,  -3.,  -2.,  -1.,  -1./
      DATA (BET(I,30,1),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   2.,
     S   3.,   3.,   3.,   2.,   2.,   1.,  -1.,  -2.,  -3.,  -4.,
     S  -5.,  -5.,  -4.,  -4.,  -3.,  -3.,  -2.,  -1.,   0.,   0./
      DATA (BET(I,31,1),I=1,40)/
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,
     S   1.,   1.,   1.,   1.,   0.,  -1.,  -2.,  -3.,  -4.,  -5.,
     S  -6.,  -6.,  -5.,  -4.,  -3.,  -2.,  -1.,   0.,   1.,   1./
C----- 1.,0E4*ALP(I,J,2) FOR TDB 
      DATA (ALP(I, 1,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  25.,  32.,
     S  40.,  47.,  54.,  59.,  64.,  66.,  68.,  68.,  69.,  70.,
     S  72.,  74.,  77.,  79.,  81.,  84.,  86.,  88.,  90.,  91.,
     S  92.,  93.,  93.,  92.,  92.,  92.,  91.,  90.,  89.,  88./
      DATA (ALP(I, 2,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  25.,  32.,
     S  40.,  47.,  54.,  59.,  63.,  66.,  68.,  68.,  69.,  70.,
     S  72.,  74.,  77.,  79.,  81.,  84.,  86.,  88.,  90.,  91.,
     S  92.,  92.,  92.,  91.,  91.,  90.,  89.,  89.,  87.,  86./
      DATA (ALP(I, 3,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  25.,  32.,
     S  40.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  72.,  74.,  76.,  79.,  81.,  83.,  86.,  88.,  89.,  90.,
     S  91.,  91.,  90.,  90.,  89.,  88.,  87.,  86.,  85.,  83./
      DATA (ALP(I, 4,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  25.,  32.,
     S  40.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  72.,  74.,  76.,  79.,  81.,  83.,  85.,  87.,  88.,  89.,
     S  89.,  89.,  89.,  88.,  87.,  86.,  85.,  84.,  82.,  80./
      DATA (ALP(I, 5,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  25.,  32.,
     S  40.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  72.,  74.,  76.,  78.,  80.,  82.,  84.,  86.,  87.,  88.,
     S  88.,  87.,  87.,  86.,  85.,  83.,  82.,  81.,  79.,  76./
      DATA (ALP(I, 6,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  25.,  32.,
     S  40.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  69.,  70.,
     S  71.,  73.,  76.,  78.,  80.,  81.,  83.,  84.,  85.,  86.,
     S  85.,  85.,  84.,  83.,  82.,  80.,  79.,  77.,  75.,  72./
      DATA (ALP(I, 7,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  25.,  32.,
     S  39.,  47.,  54.,  59.,  63.,  66.,  67.,  68.,  68.,  69.,
     S  71.,  73.,  75.,  77.,  78.,  80.,  81.,  82.,  83.,  83.,
     S  83.,  82.,  81.,  80.,  78.,  77.,  75.,  73.,  71.,  68./
      DATA (ALP(I, 8,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  25.,  32.,
     S  39.,  47.,  53.,  59.,  63.,  66.,  67.,  67.,  68.,  69.,
     S  70.,  72.,  74.,  75.,  77.,  78.,  79.,  80.,  80.,  80.,
     S  80.,  79.,  77.,  76.,  74.,  73.,  71.,  68.,  66.,  63./
      DATA (ALP(I, 9,2),I=1,40)/
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  32.,
     S  39.,  47.,  53.,  59.,  63.,  65.,  66.,  67.,  67.,  68.,
     S  69.,  71.,  72.,  73.,  74.,  75.,  76.,  77.,  77.,  76.,
     S  76.,  75.,  73.,  72.,  70.,  68.,  66.,  64.,  61.,  58./
      DATA (ALP(I,10,2),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  31.,
     S  39.,  46.,  53.,  58.,  62.,  65.,  66.,  66.,  66.,  67.,
     S  68.,  69.,  70.,  71.,  72.,  72.,  73.,  73.,  73.,  72.,
     S  72.,  71.,  69.,  67.,  65.,  63.,  61.,  58.,  56.,  53./
      DATA (ALP(I,11,2),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  31.,
     S  39.,  46.,  53.,  58.,  62.,  64.,  65.,  65.,  65.,  65.,
     S  66.,  66.,  67.,  67.,  68.,  68.,  69.,  69.,  69.,  68.,
     S  67.,  66.,  64.,  62.,  60.,  58.,  56.,  53.,  51.,  48./
      DATA (ALP(I,12,2),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  31.,
     S  38.,  45.,  52.,  57.,  60.,  62.,  63.,  63.,  63.,  62.,
     S  63.,  63.,  63.,  63.,  64.,  64.,  64.,  64.,  64.,  63.,
     S  62.,  61.,  59.,  57.,  55.,  53.,  51.,  49.,  47.,  44./
      DATA (ALP(I,13,2),I=1,40)/
     S   1.,   1.,   3.,   4.,   6.,   9.,  13.,  18.,  24.,  30.,
     S  38.,  45.,  51.,  55.,  59.,  60.,  61.,  60.,  59.,  59.,
     S  59.,  59.,  59.,  59.,  59.,  59.,  59.,  59.,  59.,  58.,
     S  57.,  56.,  54.,  52.,  50.,  48.,  46.,  45.,  43.,  41./
      DATA (ALP(I,14,2),I=1,40)/
     S   1.,   1.,   2.,   4.,   6.,   9.,  12.,  17.,  23.,  29.,
     S  36.,  43.,  49.,  53.,  56.,  57.,  57.,  56.,  55.,  55.,
     S  54.,  54.,  54.,  54.,  54.,  55.,  55.,  54.,  54.,  53.,
     S  52.,  51.,  50.,  48.,  46.,  44.,  42.,  41.,  39.,  38./
      DATA (ALP(I,15,2),I=1,40)/
     S   1.,   1.,   2.,   4.,   6.,   8.,  12.,  16.,  22.,  28.,
     S  35.,  41.,  46.,  50.,  52.,  53.,  53.,  52.,  51.,  50.,
     S  49.,  49.,  49.,  49.,  49.,  50.,  50.,  50.,  49.,  49.,
     S  48.,  47.,  45.,  44.,  42.,  40.,  39.,  38.,  37.,  36./
      DATA (ALP(I,16,2),I=1,40)/
     S   1.,   1.,   2.,   3.,   5.,   8.,  11.,  15.,  20.,  26.,
     S  32.,  37.,  42.,  45.,  47.,  48.,  48.,  47.,  45.,  45.,
     S  44.,  44.,  44.,  45.,  45.,  45.,  45.,  45.,  45.,  44.,
     S  44.,  42.,  41.,  40.,  39.,  37.,  36.,  35.,  35.,  34./
      DATA (ALP(I,17,2),I=1,40)/
     S   0.,   1.,   2.,   3.,   5.,   7.,  10.,  14.,  18.,  23.,
     S  28.,  33.,  37.,  40.,  41.,  42.,  42.,  41.,  40.,  40.,
     S  39.,  40.,  40.,  40.,  41.,  41.,  41.,  41.,  41.,  41.,
     S  40.,  39.,  38.,  37.,  36.,  35.,  34.,  34.,  34.,  34./
      DATA (ALP(I,18,2),I=1,40)/
     S   0.,   1.,   2.,   3.,   4.,   6.,   8.,  11.,  15.,  19.,
     S  24.,  28.,  31.,  33.,  35.,  36.,  36.,  36.,  35.,  35.,
     S  35.,  35.,  36.,  37.,  37.,  38.,  38.,  38.,  38.,  37.,
     S  37.,  36.,  35.,  34.,  34.,  33.,  33.,  33.,  33.,  33./
      DATA (ALP(I,19,2),I=1,40)/
     S   0.,   1.,   1.,   2.,   3.,   4.,   6.,   9.,  12.,  15.,
     S  19.,  22.,  25.,  27.,  29.,  30.,  31.,  31.,  31.,  31.,
     S  32.,  32.,  33.,  34.,  34.,  35.,  35.,  35.,  35.,  35.,
     S  34.,  34.,  33.,  33.,  32.,  32.,  32.,  32.,  33.,  33./
      DATA (ALP(I,20,2),I=1,40)/
     S   0.,   0.,   1.,   1.,   2.,   3.,   4.,   6.,   8.,  11.,
     S  13.,  16.,  19.,  21.,  24.,  25.,  27.,  28.,  28.,  29.,
     S  29.,  30.,  31.,  31.,  32.,  32.,  33.,  33.,  33.,  33.,
     S  32.,  32.,  32.,  31.,  31.,  32.,  32.,  32.,  32.,  32./
      DATA (ALP(I,21,2),I=1,40)/
     S   0.,   0.,   0.,   1.,   1.,   2.,   3.,   4.,   5.,   7.,
     S   9.,  11.,  14.,  16.,  19.,  21.,  24.,  25.,  26.,  27.,
     S  28.,  28.,  29.,  29.,  30.,  30.,  31.,  31.,  31.,  31.,
     S  31.,  31.,  31.,  31.,  31.,  31.,  32.,  32.,  32.,  32./
      DATA (ALP(I,22,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   3.,   4.,
     S   6.,   8.,  10.,  12.,  15.,  18.,  21.,  23.,  25.,  26.,
     S  26.,  27.,  27.,  28.,  28.,  29.,  29.,  29.,  29.,  30.,
     S  30.,  30.,  30.,  30.,  31.,  31.,  32.,  32.,  32.,  31./
      DATA (ALP(I,23,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   3.,
     S   4.,   5.,   7.,   9.,  12.,  15.,  18.,  21.,  23.,  25.,
     S  26.,  26.,  26.,  27.,  27.,  27.,  28.,  28.,  28.,  28.,
     S  29.,  29.,  30.,  30.,  31.,  32.,  32.,  31.,  31.,  30./
      DATA (ALP(I,24,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,
     S   2.,   4.,   5.,   7.,  10.,  13.,  16.,  19.,  21.,  23.,
     S  25.,  25.,  25.,  26.,  26.,  26.,  26.,  27.,  27.,  28.,
     S  28.,  29.,  30.,  31.,  31.,  31.,  31.,  31.,  30.,  29./
      DATA (ALP(I,25,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,
     S   2.,   3.,   4.,   6.,   8.,  11.,  13.,  16.,  19.,  21.,
     S  23.,  24.,  25.,  25.,  25.,  25.,  25.,  26.,  26.,  27.,
     S  28.,  29.,  30.,  31.,  31.,  31.,  31.,  30.,  29.,  28./
      DATA (ALP(I,26,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,
     S   2.,   3.,   4.,   5.,   7.,   9.,  11.,  14.,  16.,  19.,
     S  21.,  22.,  23.,  24.,  24.,  24.,  24.,  25.,  26.,  27.,
     S  28.,  29.,  30.,  31.,  31.,  30.,  30.,  29.,  28.,  26./
      DATA (ALP(I,27,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,
     S   2.,   2.,   3.,   5.,   6.,   8.,  10.,  12.,  14.,  16.,
     S  18.,  20.,  21.,  22.,  23.,  23.,  24.,  24.,  25.,  27.,
     S  28.,  29.,  30.,  30.,  30.,  29.,  29.,  28.,  26.,  25./
      DATA (ALP(I,28,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,
     S   1.,   2.,   3.,   4.,   6.,   7.,   8.,  10.,  11.,  13.,
     S  15.,  17.,  19.,  20.,  21.,  22.,  23.,  24.,  26.,  27.,
     S  28.,  29.,  29.,  29.,  29.,  28.,  27.,  26.,  25.,  25./
      DATA (ALP(I,29,2),I=1,40)/
     S  -1.,  -1.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,
     S   1.,   2.,   3.,   4.,   5.,   6.,   7.,   8.,   9.,  10.,
     S  12.,  14.,  16.,  18.,  20.,  21.,  23.,  25.,  26.,  27.,
     S  28.,  28.,  28.,  28.,  28.,  27.,  26.,  25.,  25.,  25./
      DATA (ALP(I,30,2),I=1,40)/
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   1.,   2.,   3.,   3.,   4.,   5.,   6.,   6.,   7.,
     S   9.,  10.,  13.,  15.,  18.,  21.,  23.,  26.,  27.,  28.,
     S  28.,  28.,  27.,  27.,  26.,  25.,  24.,  24.,  24.,  25./
      DATA (ALP(I,31,2),I=1,40)/
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S   0.,   0.,   1.,   1.,   2.,   2.,   3.,   4.,   4.,   5.,
     S   6.,   8.,  10.,  13.,  17.,  20.,  24.,  27.,  28.,  29.,
     S  28.,  28.,  26.,  25.,  24.,  23.,  23.,  23.,  24.,  24./
C----- 1.,0E6*BET(I,J,2) FOR TDB 
      DATA (BET(I, 1,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  15.,
     S  17.,  18.,  19.,  20.,  21.,  24.,  26.,  28.,  29.,  29.,
     S  29.,  29.,  27.,  26.,  26.,  25.,  26.,  26.,  25.,  23./
      DATA (BET(I, 2,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  15.,
     S  17.,  18.,  19.,  20.,  21.,  24.,  26.,  28.,  29.,  29.,
     S  29.,  28.,  27.,  26.,  25.,  25.,  25.,  25.,  24.,  23./
      DATA (BET(I, 3,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  15.,
     S  17.,  18.,  19.,  20.,  21.,  24.,  26.,  28.,  29.,  29.,
     S  29.,  28.,  27.,  26.,  25.,  25.,  25.,  24.,  24.,  22./
      DATA (BET(I, 4,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  15.,
     S  17.,  18.,  19.,  20.,  21.,  24.,  26.,  28.,  29.,  29.,
     S  29.,  28.,  26.,  25.,  24.,  24.,  24.,  24.,  23.,  21./
      DATA (BET(I, 5,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  15.,
     S  17.,  18.,  19.,  20.,  21.,  24.,  26.,  27.,  28.,  29.,
     S  28.,  27.,  26.,  25.,  24.,  23.,  23.,  23.,  22.,  20./
      DATA (BET(I, 6,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  15.,
     S  17.,  18.,  19.,  20.,  21.,  23.,  26.,  27.,  28.,  28.,
     S  28.,  26.,  25.,  24.,  23.,  22.,  22.,  22.,  20.,  19./
      DATA (BET(I, 7,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  15.,
     S  17.,  18.,  19.,  20.,  21.,  23.,  25.,  27.,  27.,  27.,
     S  27.,  25.,  24.,  23.,  22.,  21.,  21.,  20.,  19.,  17./
      DATA (BET(I, 8,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   8.,   9.,   9.,  10.,  12.,  15.,
     S  17.,  18.,  19.,  20.,  21.,  23.,  25.,  26.,  26.,  26.,
     S  26.,  24.,  23.,  21.,  20.,  20.,  19.,  19.,  17.,  15./
      DATA (BET(I, 9,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,   9.,  11.,  12.,  15.,
     S  17.,  18.,  19.,  19.,  21.,  22.,  24.,  25.,  25.,  25.,
     S  24.,  23.,  21.,  20.,  19.,  18.,  18.,  17.,  16.,  14./
      DATA (BET(I,10,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,  10.,  11.,  12.,  15.,
     S  16.,  18.,  18.,  19.,  20.,  22.,  23.,  24.,  24.,  23.,
     S  22.,  21.,  20.,  18.,  17.,  17.,  16.,  15.,  14.,  12./
      DATA (BET(I,11,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,  10.,  11.,  12.,  14.,
     S  16.,  17.,  18.,  18.,  19.,  20.,  21.,  22.,  22.,  22.,
     S  21.,  19.,  18.,  17.,  16.,  15.,  14.,  13.,  12.,  10./
      DATA (BET(I,12,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,  10.,  11.,  12.,  14.,
     S  16.,  17.,  17.,  17.,  18.,  19.,  20.,  20.,  20.,  20.,
     S  19.,  18.,  16.,  15.,  14.,  13.,  12.,  11.,  10.,   9./
      DATA (BET(I,13,2),I=1,40)/
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,   1.,
     S   3.,   5.,   7.,   8.,   9.,   9.,  10.,  11.,  12.,  14.,
     S  15.,  16.,  16.,  16.,  17.,  17.,  18.,  18.,  18.,  18.,
     S  17.,  16.,  14.,  13.,  12.,  11.,  11.,  10.,   9.,   7./
      DATA (BET(I,14,2),I=1,40)/
     S   0.,   0.,   0.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   2.,
     S   3.,   5.,   7.,   8.,   9.,  10.,  10.,  11.,  12.,  13.,
     S  14.,  15.,  15.,  15.,  15.,  16.,  16.,  16.,  16.,  16.,
     S  15.,  14.,  13.,  11.,  10.,  10.,   9.,   8.,   7.,   6./
      DATA (BET(I,15,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   2.,
     S   4.,   6.,   7.,   9.,   9.,  10.,  10.,  10.,  11.,  12.,
     S  13.,  13.,  13.,  13.,  13.,  14.,  14.,  14.,  14.,  14.,
     S  13.,  12.,  11.,  10.,   9.,   8.,   7.,   7.,   6.,   4./
      DATA (BET(I,16,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   2.,
     S   4.,   6.,   8.,   9.,   9.,   9.,   9.,   9.,  10.,  11.,
     S  11.,  12.,  12.,  12.,  12.,  12.,  12.,  12.,  12.,  12.,
     S  11.,  10.,   9.,   8.,   7.,   7.,   6.,   5.,   4.,   3./
      DATA (BET(I,17,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   3.,
     S   4.,   6.,   8.,   9.,   9.,   9.,   9.,   8.,   9.,   9.,
     S  10.,  10.,  10.,  10.,  10.,  10.,  10.,  11.,  10.,  10.,
     S   9.,   9.,   8.,   7.,   6.,   6.,   5.,   4.,   3.,   2./
      DATA (BET(I,18,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   3.,
     S   5.,   6.,   7.,   8.,   8.,   8.,   8.,   7.,   7.,   8.,
     S   8.,   9.,   9.,   9.,   9.,   9.,   9.,   9.,   9.,   8.,
     S   8.,   7.,   7.,   6.,   5.,   4.,   4.,   3.,   1.,   0./
      DATA (BET(I,19,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   2.,   3.,
     S   5.,   6.,   7.,   8.,   8.,   7.,   7.,   6.,   6.,   6.,
     S   7.,   7.,   7.,   8.,   8.,   8.,   8.,   8.,   7.,   7.,
     S   7.,   6.,   5.,   5.,   4.,   3.,   2.,   1.,   0.,  -1./
      DATA (BET(I,20,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   2.,   3.,
     S   4.,   5.,   6.,   7.,   7.,   6.,   6.,   5.,   5.,   5.,
     S   5.,   6.,   6.,   7.,   7.,   7.,   7.,   7.,   6.,   6.,
     S   6.,   5.,   5.,   4.,   3.,   2.,   1.,   0.,  -1.,  -2./
      DATA (BET(I,21,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   2.,   3.,
     S   4.,   5.,   5.,   6.,   6.,   6.,   5.,   5.,   4.,   4.,
     S   4.,   5.,   5.,   6.,   6.,   6.,   6.,   6.,   5.,   5.,
     S   5.,   4.,   4.,   3.,   2.,   1.,   0.,  -1.,  -2.,  -3./
      DATA (BET(I,22,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   2.,
     S   3.,   4.,   4.,   5.,   5.,   5.,   5.,   4.,   4.,   3.,
     S   4.,   4.,   5.,   5.,   5.,   5.,   5.,   5.,   5.,   4.,
     S   4.,   3.,   3.,   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3./
      DATA (BET(I,23,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,
     S   2.,   3.,   3.,   4.,   4.,   5.,   5.,   4.,   4.,   4.,
     S   3.,   3.,   4.,   4.,   5.,   5.,   5.,   4.,   4.,   4.,
     S   3.,   3.,   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4./
      DATA (BET(I,24,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,
     S   1.,   2.,   2.,   3.,   3.,   4.,   4.,   5.,   4.,   4.,
     S   4.,   3.,   3.,   3.,   4.,   4.,   4.,   4.,   4.,   3.,
     S   2.,   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4.,  -4./
      DATA (BET(I,25,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,
     S   1.,   1.,   1.,   2.,   2.,   3.,   4.,   4.,   5.,   4.,
     S   4.,   3.,   3.,   3.,   3.,   3.,   3.,   3.,   3.,   2.,
     S   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4.,  -4.,  -4./
      DATA (BET(I,26,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   1.,   1.,   2.,   2.,   3.,   4.,   4.,   5.,
     S   4.,   4.,   3.,   2.,   2.,   2.,   2.,   2.,   2.,   1.,
     S   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4.,  -4.,  -4.,  -3./
      DATA (BET(I,27,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   1.,   1.,   2.,   3.,   4.,   4.,
     S   4.,   4.,   4.,   3.,   2.,   2.,   1.,   1.,   1.,   0.,
     S   0.,  -1.,  -2.,  -2.,  -3.,  -4.,  -4.,  -4.,  -3.,  -3./
      DATA (BET(I,28,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   1.,   2.,   2.,   3.,   4.,
     S   4.,   4.,   4.,   3.,   2.,   1.,   1.,   0.,  -1.,  -1.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -3.,  -3.,  -2./
      DATA (BET(I,29,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   2.,   3.,
     S   4.,   4.,   4.,   3.,   2.,   1.,   0.,  -1.,  -2.,  -3.,
     S  -3.,  -3.,  -3.,  -3.,  -3.,  -3.,  -3.,  -2.,  -1.,  -1./
      DATA (BET(I,30,2),I=1,40)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,   2.,
     S   3.,   3.,   3.,   2.,   2.,   1.,  -1.,  -2.,  -3.,  -4.,
     S  -5.,  -5.,  -4.,  -4.,  -3.,  -3.,  -2.,  -1.,   0.,   0./
      DATA (BET(I,31,2),I=1,40)/
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,
     S   1.,   1.,   1.,   1.,   0.,  -1.,  -2.,  -3.,  -4.,  -5.,
     S  -6.,  -6.,  -6.,  -4.,  -3.,  -2.,  -1.,   0.,   1.,   0./
C
C-----  -10E+2*LN(1.-TB)    (O3 9.6 micron band)
      DATA (TBO3(I, 1),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.8, 840.8, 791.9, 743.8, 696.8, 651.1,
     S 606.8, 564.8, 525.5, 489.4, 457.0, 428.0, 401.7, 377.3, 353.9,
     S 331.2, 308.8, 286.8, 265.4, 244.7, 225.2, 207.1, 190.5, 175.6,
     S 162.2, 150.4/
      DATA (TBO3(I, 2),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.8, 840.8, 791.9, 743.8, 696.8, 651.1,
     S 606.8, 564.8, 525.5, 489.4, 457.0, 428.0, 401.7, 377.3, 353.9,
     S 331.2, 308.8, 286.8, 265.4, 244.7, 225.2, 207.0, 190.5, 175.5,
     S 162.2, 150.3/
      DATA (TBO3(I, 3),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.8, 840.8, 791.9, 743.8, 696.8, 651.1,
     S 606.8, 564.8, 525.5, 489.4, 457.0, 428.0, 401.7, 377.3, 353.9,
     S 331.2, 308.8, 286.8, 265.4, 244.7, 225.2, 207.0, 190.5, 175.5,
     S 162.2, 150.3/
      DATA (TBO3(I, 4),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.8, 840.8, 791.9, 743.8, 696.8, 651.1,
     S 606.8, 564.8, 525.5, 489.4, 457.0, 428.0, 401.7, 377.3, 353.9,
     S 331.2, 308.8, 286.8, 265.4, 244.7, 225.1, 207.0, 190.4, 175.4,
     S 162.1, 150.2/
      DATA (TBO3(I, 5),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.8, 840.8, 791.9, 743.8, 696.9, 651.1,
     S 606.8, 564.8, 525.5, 489.4, 457.0, 428.0, 401.7, 377.2, 353.9,
     S 331.2, 308.8, 286.8, 265.3, 244.6, 225.1, 206.9, 190.3, 175.3,
     S 161.9, 150.0/
      DATA (TBO3(I, 6),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.7, 840.8, 791.9, 743.8, 696.8, 651.1,
     S 606.8, 564.8, 525.5, 489.4, 457.0, 427.9, 401.7, 377.2, 353.9,
     S 331.1, 308.7, 286.7, 265.3, 244.6, 225.0, 206.8, 190.2, 175.2,
     S 161.7, 149.7/
      DATA (TBO3(I, 7),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.8, 840.7, 792.0, 743.8, 696.8, 651.1,
     S 606.8, 564.8, 525.5, 489.4, 457.0, 427.9, 401.6, 377.2, 353.8,
     S 331.1, 308.7, 286.7, 265.2, 244.5, 224.9, 206.6, 190.0, 174.9,
     S 161.3, 149.2/
      DATA (TBO3(I, 8),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.8, 840.8, 791.9, 743.8, 696.8, 651.1,
     S 606.8, 564.8, 525.4, 489.4, 456.9, 427.9, 401.6, 377.1, 353.8,
     S 331.0, 308.6, 286.6, 265.0, 244.3, 224.6, 206.4, 189.6, 174.4,
     S 160.7, 148.4/
      DATA (TBO3(I, 9),I=1,38)/
     S1487.4,1437.4,1387.4,1337.4,1287.4,1237.4,1187.4,1137.4,1087.4,
     S1037.4, 987.4, 937.9, 889.8, 840.8, 792.0, 743.8, 696.9, 651.1,
     S 606.8, 564.8, 525.4, 489.4, 456.9, 427.8, 401.5, 377.0, 353.7,
     S 330.9, 308.4, 286.4, 264.8, 244.0, 224.3, 205.9, 189.0, 173.7,
     S 159.8, 147.2/
      DATA (TBO3(I,10),I=1,38)/
     S1487.4,1437.4,1387.4,1337.4,1287.4,1237.4,1187.4,1137.4,1087.4,
     S1037.4, 987.4, 937.9, 889.6, 840.7, 792.0, 743.7, 696.8, 651.1,
     S 606.8, 564.7, 525.4, 489.3, 456.8, 427.7, 401.4, 376.9, 353.5,
     S 330.6, 308.2, 286.0, 264.4, 243.5, 223.7, 205.2, 188.1, 172.5,
     S 158.3, 145.4/
      DATA (TBO3(I,11),I=1,38)/
     S1487.6,1437.6,1387.6,1337.6,1287.6,1237.6,1187.6,1137.6,1087.6,
     S1037.6, 987.6, 937.9, 889.7, 840.8, 792.0, 743.8, 696.8, 651.1,
     S 606.8, 564.7, 525.3, 489.2, 456.7, 427.6, 401.2, 376.7, 353.2,
     S 330.3, 307.7, 285.5, 263.8, 242.8, 222.8, 204.0, 186.7, 170.8,
     S 156.2, 142.7/
      DATA (TBO3(I,12),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.9, 889.6, 840.8, 791.9, 743.8, 696.8, 651.0,
     S 606.7, 564.6, 525.2, 489.1, 456.5, 427.3, 400.9, 376.3, 352.7,
     S 329.7, 307.0, 284.7, 262.8, 241.5, 221.3, 202.2, 184.5, 168.2,
     S 153.0, 139.0/
      DATA (TBO3(I,13),I=1,38)/
     S1487.0,1437.0,1387.0,1337.0,1287.0,1237.0,1187.0,1137.0,1087.0,
     S1037.0, 987.0, 938.1, 889.6, 840.8, 791.8, 743.9, 696.8, 651.0,
     S 606.7, 564.5, 525.0, 488.9, 456.2, 426.9, 400.4, 375.7, 352.0,
     S 328.8, 305.9, 283.4, 261.2, 239.6, 219.0, 199.5, 181.3, 164.4,
     S 148.7, 134.1/
      DATA (TBO3(I,14),I=1,38)/
     S1487.4,1437.4,1387.4,1337.4,1287.4,1237.4,1187.4,1137.4,1087.4,
     S1037.4, 987.4, 938.5, 889.7, 840.6, 791.9, 743.8, 696.7, 650.9,
     S 606.5, 564.3, 524.8, 488.5, 455.7, 426.3, 399.6, 374.6, 350.7,
     S 327.3, 304.2, 281.2, 258.7, 236.7, 215.6, 195.6, 176.8, 159.2,
     S 142.9, 127.8/
      DATA (TBO3(I,15),I=1,38)/
     S1487.8,1437.8,1387.8,1337.8,1287.8,1237.8,1187.8,1137.8,1087.8,
     S1037.8, 987.8, 938.4, 890.1, 840.7, 791.9, 743.8, 696.7, 650.8,
     S 606.3, 564.0, 524.4, 487.9, 454.9, 425.3, 398.3, 373.0, 348.8,
     S 325.0, 301.4, 278.0, 254.9, 232.4, 210.7, 190.1, 170.6, 152.5,
     S 135.7, 120.0/
      DATA (TBO3(I,16),I=1,38)/
     S1487.8,1437.8,1387.8,1337.8,1287.8,1237.8,1187.8,1137.8,1087.8,
     S1037.8, 987.8, 938.9, 890.2, 840.7, 791.9, 743.8, 696.5, 650.6,
     S 606.0, 563.5, 523.6, 486.9, 453.6, 423.6, 396.2, 370.5, 345.8,
     S 321.4, 297.3, 273.3, 249.5, 226.3, 204.0, 182.7, 162.8, 144.2,
     S 126.9, 111.0/
      DATA (TBO3(I,17),I=1,38)/
     S1488.2,1438.2,1388.2,1338.2,1288.2,1238.2,1188.2,1138.2,1088.2,
     S1038.2, 988.2, 939.4, 890.4, 841.1, 791.8, 743.7, 696.4, 650.2,
     S 605.6, 562.8, 522.6, 485.5, 451.7, 421.1, 393.0, 366.7, 341.2,
     S 316.2, 291.3, 266.5, 242.1, 218.2, 195.3, 173.6, 153.2, 134.3,
     S 116.9, 101.0/
      DATA (TBO3(I,18),I=1,38)/
     S1489.0,1439.0,1389.0,1339.0,1289.0,1239.0,1189.0,1139.0,1089.0,
     S1039.0, 989.0, 938.9, 890.7, 840.6, 791.2, 743.4, 696.2, 649.8,
     S 604.8, 561.7, 521.0, 483.2, 448.7, 417.2, 388.3, 361.1, 334.7,
     S 308.8, 283.1, 257.5, 232.4, 208.0, 184.7, 162.6, 142.1, 123.1,
     S 105.8,  90.2/
      DATA (TBO3(I,19),I=1,38)/
     S1489.0,1439.0,1389.0,1339.0,1289.0,1239.0,1189.0,1139.0,1089.0,
     S1039.0, 989.0, 938.9, 890.0, 839.6, 791.3, 743.3, 695.8, 649.1,
     S 603.7, 560.1, 518.7, 480.1, 444.4, 411.8, 381.7, 353.3, 325.9,
     S 299.0, 272.5, 246.3, 220.6, 195.9, 172.3, 150.2, 129.8, 111.1,
     S  94.2,  79.3/
      DATA (TBO3(I,20),I=1,38)/
     S1489.0,1439.0,1389.0,1339.0,1289.0,1239.0,1189.0,1139.0,1089.0,
     S1039.0, 989.0, 938.6, 888.9, 839.6, 791.3, 743.2, 695.5, 648.2,
     S 602.4, 557.9, 515.6, 475.7, 438.7, 404.6, 373.0, 343.3, 314.7,
     S 286.9, 259.6, 233.0, 207.1, 182.3, 158.8, 136.9, 116.8,  98.7,
     S  82.7,  68.7/
      DATA (TBO3(I,21),I=1,38)/
     S1487.2,1437.2,1387.2,1337.2,1287.2,1237.2,1187.2,1137.2,1087.2,
     S1037.2, 987.2, 937.8, 888.6, 840.0, 791.6, 742.9, 694.5, 647.2,
     S 600.5, 555.3, 511.7, 470.4, 431.8, 395.9, 362.5, 331.3, 301.6,
     S 272.9, 245.2, 218.3, 192.4, 167.7, 144.6, 123.2, 103.9,  86.7,
     S  71.7,  58.9/
      DATA (TBO3(I,22),I=1,38)/
     S1486.6,1436.6,1386.6,1336.6,1286.6,1236.6,1186.6,1136.6,1086.6,
     S1036.6, 986.6, 937.6, 889.5, 840.2, 791.5, 742.1, 694.1, 645.8,
     S 598.5, 552.3, 507.5, 464.7, 424.2, 386.3, 351.0, 318.2, 287.3,
     S 257.9, 229.8, 202.9, 177.2, 153.0, 130.5, 109.9,  91.6,  75.5,
     S  61.8,  50.3/
      DATA (TBO3(I,23),I=1,38)/
     S1485.9,1435.9,1385.9,1335.9,1285.9,1235.9,1185.9,1135.9,1085.9,
     S1035.9, 985.9, 937.8, 888.5, 840.6, 790.8, 742.4, 693.2, 644.4,
     S 596.7, 549.5, 503.6, 459.2, 416.8, 376.9, 339.6, 305.0, 272.8,
     S 242.6, 214.3, 187.5, 162.2, 138.7, 117.1,  97.6,  80.4,  65.7,
     S  53.3,  43.0/
      DATA (TBO3(I,24),I=1,38)/
     S1484.1,1434.1,1384.1,1334.1,1284.1,1234.1,1184.1,1134.1,1084.1,
     S1034.1, 984.1, 937.9, 887.8, 839.6, 791.1, 741.9, 692.4, 643.2,
     S 594.9, 547.1, 500.2, 454.5, 410.6, 368.7, 329.4, 292.9, 259.2,
     S 228.1, 199.4, 172.7, 148.1, 125.4, 104.9,  86.7,  70.9,  57.5,
     S  46.3,  37.1/
      DATA (TBO3(I,25),I=1,38)/
     S1482.1,1432.1,1382.1,1332.1,1282.1,1232.1,1182.1,1132.1,1082.1,
     S1032.1, 982.1, 937.4, 888.7, 840.2, 790.9, 741.8, 692.0, 642.4,
     S 593.7, 545.3, 497.6, 451.0, 405.7, 362.3, 321.1, 282.7, 247.3,
     S 215.0, 185.7, 159.1, 135.1, 113.4,  94.2,  77.4,  63.0,  50.9,
     S  40.8,  32.2/
      DATA (TBO3(I,26),I=1,38)/
     S1483.8,1433.8,1383.8,1333.8,1283.8,1233.8,1183.8,1133.8,1083.8,
     S1033.8, 983.8, 936.5, 890.0, 840.4, 791.2, 741.8, 691.7, 642.0,
     S 592.8, 544.0, 495.9, 448.5, 402.3, 357.6, 314.8, 274.7, 237.6,
     S 204.0, 173.8, 147.1, 123.6, 103.0,  85.2,  69.9,  56.9,  46.0,
     S  36.6,  28.5/
      DATA (TBO3(I,27),I=1,38)/
     S1482.3,1432.3,1382.3,1332.3,1282.3,1232.3,1182.3,1132.3,1082.3,
     S1032.3, 982.3, 935.2, 889.5, 841.6, 792.1, 741.2, 691.5, 641.6,
     S 592.3, 543.2, 494.7, 446.8, 399.9, 354.2, 310.4, 268.9, 230.3,
     S 195.2, 164.1, 137.0, 113.9,  94.3,  77.8,  63.9,  52.2,  42.2,
     S  33.4,  25.5/
      DATA (TBO3(I,28),I=1,38)/
     S1482.8,1432.8,1382.8,1332.8,1282.8,1232.8,1182.8,1132.8,1082.8,
     S1032.8, 982.8, 935.7, 889.2, 842.7, 791.9, 742.1, 691.9, 641.4,
     S 591.7, 542.7, 493.9, 445.7, 398.4, 352.1, 307.5, 264.9, 225.1,
     S 188.9, 156.8, 129.2, 106.3,  87.5,  72.2,  59.6,  49.0,  39.7,
     S  31.2,  23.5/
      DATA (TBO3(I,29),I=1,38)/
     S1483.4,1433.4,1383.4,1333.4,1283.4,1233.4,1183.4,1133.4,1083.4,
     S1033.4, 983.4, 935.7, 889.2, 843.3, 791.2, 742.1, 691.3, 641.3,
     S 591.6, 542.3, 493.5, 445.0, 397.3, 350.7, 305.5, 262.3, 221.6,
     S 184.4, 151.5, 123.5, 100.5,  82.4,  68.0,  56.5,  46.6,  37.7,
     S  29.4,  21.7/
      DATA (TBO3(I,30),I=1,38)/
     S1484.9,1434.9,1384.9,1334.9,1284.9,1234.9,1184.9,1134.9,1084.9,
     S1034.9, 984.9, 938.5, 889.3, 842.1, 791.7, 741.8, 691.4, 641.4,
     S 591.9, 542.4, 493.4, 444.7, 396.9, 350.1, 304.5, 260.8, 219.7,
     S 181.9, 148.3, 119.9,  96.9,  79.2,  65.6,  54.7,  45.3,  36.6,
     S  28.4,  20.8/
      DATA (TBO3(I,31),I=1,38)/
     S1486.6,1436.6,1386.6,1336.6,1286.6,1236.6,1186.6,1136.6,1086.6,
     S1036.6, 986.6, 938.5, 890.1, 840.9, 792.3, 741.0, 690.5, 641.6,
     S 591.7, 542.2, 493.1, 444.4, 396.4, 349.5, 303.7, 259.8, 218.3,
     S 180.1, 146.2, 117.4,  94.4,  76.9,  63.7,  53.2,  43.9,  35.4,
     S  27.2,  19.6/
C----- 1.,0E4*AO3(I,J) FOR TB 
      DATA (AO3(I, 1),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  11.,
     S  15.,  21.,  27.,  33.,  39.,  45.,  49.,  53.,  57.,  59.,
     S  60.,  60.,  60.,  58.,  56.,  54.,  52.,  49./
      DATA (AO3(I, 2),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  11.,
     S  15.,  21.,  27.,  33.,  39.,  45.,  49.,  53.,  57.,  59.,
     S  60.,  60.,  60.,  58.,  56.,  54.,  52.,  49./
      DATA (AO3(I, 3),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  11.,
     S  15.,  21.,  27.,  33.,  39.,  45.,  49.,  53.,  57.,  59.,
     S  60.,  60.,  60.,  58.,  56.,  54.,  52.,  49./
      DATA (AO3(I, 4),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  11.,
     S  15.,  21.,  27.,  33.,  39.,  45.,  49.,  53.,  57.,  59.,
     S  60.,  60.,  60.,  58.,  56.,  54.,  52.,  49./
      DATA (AO3(I, 5),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  11.,
     S  15.,  21.,  27.,  33.,  39.,  45.,  49.,  53.,  57.,  59.,
     S  60.,  60.,  59.,  58.,  56.,  54.,  52.,  49./
      DATA (AO3(I, 6),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  11.,
     S  15.,  21.,  27.,  33.,  39.,  45.,  49.,  53.,  57.,  59.,
     S  60.,  60.,  59.,  58.,  56.,  54.,  51.,  49./
      DATA (AO3(I, 7),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  11.,
     S  15.,  21.,  27.,  33.,  39.,  45.,  49.,  53.,  56.,  59.,
     S  60.,  60.,  59.,  58.,  56.,  53.,  51.,  48./
      DATA (AO3(I, 8),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  10.,
     S  15.,  21.,  27.,  33.,  39.,  45.,  49.,  53.,  56.,  59.,
     S  60.,  60.,  59.,  57.,  55.,  53.,  50.,  47./
      DATA (AO3(I, 9),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  10.,
     S  15.,  21.,  27.,  33.,  39.,  44.,  49.,  53.,  56.,  58.,
     S  59.,  59.,  59.,  57.,  55.,  52.,  49.,  46./
      DATA (AO3(I,10),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  10.,
     S  15.,  21.,  27.,  33.,  39.,  44.,  49.,  53.,  56.,  58.,
     S  59.,  59.,  58.,  56.,  54.,  51.,  48.,  44./
      DATA (AO3(I,11),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  10.,
     S  15.,  21.,  27.,  33.,  39.,  44.,  49.,  52.,  55.,  57.,
     S  58.,  58.,  57.,  55.,  52.,  49.,  46.,  42./
      DATA (AO3(I,12),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   7.,  10.,
     S  15.,  21.,  27.,  33.,  39.,  44.,  48.,  52.,  55.,  57.,
     S  57.,  57.,  55.,  53.,  50.,  47.,  44.,  40./
      DATA (AO3(I,13),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   6.,  10.,
     S  15.,  20.,  26.,  33.,  38.,  43.,  47.,  51.,  54.,  55.,
     S  56.,  55.,  53.,  51.,  48.,  44.,  41.,  37./
      DATA (AO3(I,14),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   6.,  10.,
     S  15.,  20.,  26.,  32.,  37.,  42.,  46.,  50.,  52.,  53.,
     S  53.,  52.,  50.,  48.,  44.,  41.,  37.,  33./
      DATA (AO3(I,15),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   4.,   6.,  10.,
     S  14.,  20.,  25.,  31.,  36.,  41.,  45.,  48.,  49.,  50.,
     S  50.,  49.,  47.,  44.,  41.,  37.,  34.,  30./
      DATA (AO3(I,16),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   3.,   6.,   9.,
     S  14.,  19.,  24.,  30.,  34.,  39.,  42.,  45.,  46.,  47.,
     S  46.,  44.,  42.,  39.,  36.,  33.,  30.,  27./
      DATA (AO3(I,17),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   3.,   5.,   9.,
     S  13.,  17.,  22.,  27.,  32.,  36.,  38.,  40.,  42.,  42.,
     S  41.,  40.,  37.,  35.,  32.,  30.,  27.,  24./
      DATA (AO3(I,18),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   3.,   5.,   8.,
     S  11.,  16.,  20.,  25.,  28.,  32.,  34.,  36.,  36.,  36.,
     S  36.,  35.,  33.,  31.,  29.,  26.,  24.,  22./
      DATA (AO3(I,19),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   2.,   4.,   6.,
     S  10.,  13.,  17.,  21.,  24.,  27.,  29.,  30.,  31.,  31.,
     S  31.,  30.,  29.,  27.,  26.,  24.,  22.,  20./
      DATA (AO3(I,20),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   2.,   3.,   5.,
     S   7.,  10.,  13.,  17.,  19.,  22.,  23.,  25.,  25.,  26.,
     S  26.,  26.,  25.,  24.,  23.,  22.,  20.,  19./
      DATA (AO3(I,21),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   2.,   4.,
     S   5.,   7.,  10.,  12.,  15.,  17.,  18.,  20.,  21.,  22.,
     S  23.,  23.,  23.,  22.,  22.,  21.,  19.,  18./
      DATA (AO3(I,22),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   2.,   2.,
     S   4.,   5.,   7.,   9.,  11.,  13.,  14.,  16.,  17.,  19.,
     S  20.,  21.,  21.,  21.,  21.,  20.,  19.,  18./
      DATA (AO3(I,23),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   2.,
     S   2.,   3.,   4.,   6.,   8.,   9.,  11.,  13.,  15.,  17.,
     S  18.,  19.,  20.,  20.,  20.,  19.,  19.,  18./
      DATA (AO3(I,24),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,
     S   1.,   2.,   3.,   4.,   6.,   7.,   9.,  11.,  13.,  15.,
     S  16.,  18.,  19.,  20.,  20.,  19.,  19.,  17./
      DATA (AO3(I,25),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,
     S   1.,   2.,   2.,   3.,   4.,   6.,   7.,   9.,  11.,  13.,
     S  15.,  17.,  18.,  19.,  20.,  19.,  18.,  17./
      DATA (AO3(I,26),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   1.,   1.,   2.,   2.,   3.,   5.,   6.,   8.,  10.,  12.,
     S  14.,  16.,  18.,  19.,  19.,  19.,  18.,  17./
      DATA (AO3(I,27),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   1.,   1.,   1.,   2.,   3.,   4.,   5.,   7.,   9.,  11.,
     S  14.,  16.,  17.,  19.,  19.,  19.,  18.,  17./
      DATA (AO3(I,28),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   1.,   1.,   2.,   3.,   4.,   5.,   7.,   9.,  11.,
     S  13.,  15.,  17.,  18.,  19.,  19.,  18.,  17./
      DATA (AO3(I,29),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   1.,   1.,   2.,   2.,   3.,   5.,   6.,   8.,  11.,
     S  13.,  15.,  17.,  18.,  19.,  19.,  18.,  16./
      DATA (AO3(I,30),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   1.,   1.,   2.,   2.,   3.,   4.,   6.,   8.,  11.,
     S  13.,  16.,  18.,  19.,  19.,  19.,  18.,  16./
      DATA (AO3(I,31),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   1.,   1.,   1.,   2.,   2.,   3.,   4.,   6.,   8.,  11.,
     S  13.,  16.,  18.,  18.,  19.,  18.,  17.,  16./
C----- 1.,0E6*BO3(I,J) FOR TB 
      DATA (BO3(I, 1),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -3.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   5.,   4.,   2.,  -1.,  -3.,  -5.,  -7.,  -8./
      DATA (BO3(I, 2),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -3.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   5.,   4.,   2.,  -1.,  -3.,  -5.,  -7.,  -8./
      DATA (BO3(I, 3),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -3.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   5.,   4.,   2.,   0.,  -3.,  -5.,  -7.,  -8./
      DATA (BO3(I, 4),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -3.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   5.,   4.,   2.,   0.,  -3.,  -5.,  -7.,  -8./
      DATA (BO3(I, 5),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -3.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   5.,   4.,   2.,   0.,  -3.,  -5.,  -7.,  -8./
      DATA (BO3(I, 6),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -3.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   6.,   4.,   2.,   0.,  -3.,  -5.,  -7.,  -8./
      DATA (BO3(I, 7),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -3.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   6.,   4.,   2.,   0.,  -3.,  -5.,  -6.,  -8./
      DATA (BO3(I, 8),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -2.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   6.,   4.,   2.,   0.,  -3.,  -5.,  -6.,  -8./
      DATA (BO3(I, 9),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -2.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   6.,   4.,   2.,   0.,  -2.,  -4.,  -6.,  -8./
      DATA (BO3(I,10),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -2.,  -3.,
     S  -4.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   6.,   4.,   2.,   0.,  -2.,  -4.,  -6.,  -7./
      DATA (BO3(I,11),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -2.,  -3.,
     S  -3.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   6.,   4.,   2.,   0.,  -2.,  -4.,  -5.,  -7./
      DATA (BO3(I,12),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,  -2.,  -3.,
     S  -3.,  -4.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   6.,
     S   6.,   4.,   2.,   0.,  -2.,  -4.,  -5.,  -7./
      DATA (BO3(I,13),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -2.,  -3.,
     S  -3.,  -3.,  -3.,  -2.,   0.,   2.,   4.,   5.,   6.,   7.,
     S   6.,   5.,   3.,   1.,  -1.,  -3.,  -5.,  -6./
      DATA (BO3(I,14),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,  -2.,  -3.,
     S  -3.,  -3.,  -3.,  -1.,   0.,   2.,   4.,   6.,   6.,   7.,
     S   6.,   5.,   3.,   1.,  -1.,  -3.,  -5.,  -6./
      DATA (BO3(I,15),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,  -2.,  -3.,
     S  -3.,  -3.,  -3.,  -1.,   1.,   3.,   4.,   6.,   7.,   7.,
     S   6.,   5.,   3.,   1.,  -1.,  -3.,  -5.,  -6./
      DATA (BO3(I,16),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,  -2.,  -3.,
     S  -3.,  -3.,  -2.,  -1.,   1.,   3.,   4.,   6.,   7.,   7.,
     S   6.,   5.,   3.,   1.,  -1.,  -3.,  -4.,  -5./
      DATA (BO3(I,17),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -2.,  -1.,  -2.,
     S  -2.,  -2.,  -2.,   0.,   1.,   3.,   5.,   6.,   7.,   7.,
     S   6.,   4.,   2.,   0.,  -1.,  -3.,  -4.,  -5./
      DATA (BO3(I,18),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,  -1.,  -2.,
     S  -2.,  -2.,  -1.,   0.,   2.,   3.,   5.,   6.,   6.,   6.,
     S   5.,   4.,   2.,   0.,  -2.,  -3.,  -4.,  -5./
      DATA (BO3(I,19),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,   1.,   2.,   3.,   4.,   5.,   5.,   5.,
     S   4.,   3.,   1.,   0.,  -2.,  -3.,  -4.,  -5./
      DATA (BO3(I,20),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,  -1.,
     S  -1.,  -1.,   0.,   1.,   2.,   3.,   4.,   4.,   5.,   4.,
     S   4.,   2.,   1.,   0.,  -2.,  -3.,  -4.,  -4./
      DATA (BO3(I,21),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,
     S   0.,   0.,   0.,   1.,   2.,   2.,   3.,   4.,   4.,   3.,
     S   3.,   2.,   1.,  -1.,  -2.,  -3.,  -4.,  -4./
      DATA (BO3(I,22),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,   0.,
     S   0.,   0.,   0.,   1.,   1.,   2.,   2.,   3.,   3.,   3.,
     S   2.,   1.,   0.,  -1.,  -2.,  -3.,  -4.,  -4./
      DATA (BO3(I,23),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   1.,   1.,   1.,   2.,   2.,   2.,
     S   2.,   1.,   0.,  -1.,  -2.,  -3.,  -3.,  -4./
      DATA (BO3(I,24),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,   1.,   1.,
     S   1.,   1.,   0.,  -1.,  -1.,  -2.,  -3.,  -4./
      DATA (BO3(I,25),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,  -1.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   1.,
     S   1.,   1.,   0.,   0.,  -1.,  -2.,  -3.,  -4./
      DATA (BO3(I,26),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,  -1.,  -2.,  -3.,  -4./
      DATA (BO3(I,27),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,   0.,
     S   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   0.,   0.,   0.,  -1.,  -1.,  -2.,  -3.,  -4./
      DATA (BO3(I,28),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,   0.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -2.,  -3.,  -4./
      DATA (BO3(I,29),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,  -1.,  -1.,
     S  -1.,  -1.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -2.,  -2.,  -3.,  -4./
      DATA (BO3(I,30),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -2.,  -2.,  -3.,  -4./
      DATA (BO3(I,31),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   1.,   0.,   0.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -2.,
     S  -2.,  -2.,  -1.,  -2.,  -2.,  -2.,  -3.,  -4./
C
C-----  -10E+3*LN(1.-TB)   ( O3 14 micron band )
      DATA (TO3B(I, 1),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.6, 886.4, 837.8, 789.9, 742.9, 697.1, 652.9, 610.9,
     S 571.6, 535.5, 502.9, 473.6, 447.3, 423.3, 401.0, 379.9, 359.8,
     S 340.6, 321.9/
      DATA (TO3B(I, 2),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.6, 886.4, 837.8, 789.9, 742.9, 697.1, 652.9, 610.9,
     S 571.6, 535.5, 502.9, 473.6, 447.3, 423.3, 401.0, 379.9, 359.8,
     S 340.6, 321.9/
      DATA (TO3B(I, 3),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.6, 886.4, 837.8, 789.9, 742.9, 697.1, 652.9, 610.9,
     S 571.6, 535.5, 502.9, 473.6, 447.3, 423.3, 401.0, 379.9, 359.8,
     S 340.5, 321.9/
      DATA (TO3B(I, 4),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.6, 886.4, 837.8, 789.9, 742.9, 697.1, 652.9, 610.9,
     S 571.6, 535.5, 502.9, 473.6, 447.3, 423.3, 401.0, 379.9, 359.8,
     S 340.5, 321.9/
      DATA (TO3B(I, 5),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.5, 886.4, 837.8, 789.9, 742.9, 697.1, 652.9, 610.9,
     S 571.6, 535.5, 502.9, 473.6, 447.3, 423.3, 400.9, 379.8, 359.7,
     S 340.5, 321.8/
      DATA (TO3B(I, 6),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.5, 886.4, 837.8, 789.9, 742.9, 697.1, 652.9, 610.9,
     S 571.6, 535.5, 502.8, 473.6, 447.3, 423.2, 400.9, 379.8, 359.7,
     S 340.4, 321.7/
      DATA (TO3B(I, 7),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.5, 886.4, 837.8, 789.9, 742.9, 697.0, 652.9, 610.9,
     S 571.6, 535.5, 502.8, 473.5, 447.2, 423.2, 400.8, 379.7, 359.6,
     S 340.2, 321.5/
      DATA (TO3B(I, 8),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.5, 886.4, 837.8, 789.9, 742.8, 697.0, 652.9, 610.9,
     S 571.6, 535.4, 502.8, 473.5, 447.1, 423.1, 400.7, 379.6, 359.4,
     S 340.0, 321.3/
      DATA (TO3B(I, 9),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.5, 886.4, 837.8, 789.9, 742.8, 697.0, 652.8, 610.8,
     S 571.5, 535.4, 502.7, 473.4, 447.0, 423.0, 400.5, 379.3, 359.1,
     S 339.7, 320.8/
      DATA (TO3B(I,10),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.5, 886.4, 837.8, 789.9, 742.8, 697.0, 652.8, 610.8,
     S 571.4, 535.3, 502.6, 473.2, 446.8, 422.7, 400.2, 379.0, 358.7,
     S 339.1, 320.1/
      DATA (TO3B(I,11),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.5, 886.4, 837.8, 789.8, 742.8, 696.9, 652.8, 610.7,
     S 571.3, 535.1, 502.4, 473.0, 446.5, 422.3, 399.7, 378.4, 357.9,
     S 338.2, 318.9/
      DATA (TO3B(I,12),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.6,
     S 984.9, 935.5, 886.4, 837.8, 789.8, 742.7, 696.9, 652.6, 610.6,
     S 571.2, 534.9, 502.1, 472.6, 446.0, 421.7, 399.0, 377.4, 356.7,
     S 336.7, 317.1/
      DATA (TO3B(I,13),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.4,
     S 984.9, 935.4, 886.4, 837.7, 789.7, 742.6, 696.8, 652.5, 610.3,
     S 570.9, 534.5, 501.5, 471.9, 445.2, 420.7, 397.7, 375.9, 354.8,
     S 334.4, 314.3/
      DATA (TO3B(I,14),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.4,
     S 984.8, 935.4, 886.3, 837.6, 789.6, 742.5, 696.6, 652.2, 610.0,
     S 570.4, 533.9, 500.7, 470.9, 443.9, 419.0, 395.7, 373.4, 351.9,
     S 330.8, 310.1/
      DATA (TO3B(I,15),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1234.5,1184.0,1134.2,1084.3,1034.4,
     S 984.8, 935.3, 886.2, 837.5, 789.5, 742.3, 696.3, 651.8, 609.4,
     S 569.6, 532.8, 499.4, 469.2, 441.8, 416.5, 392.6, 369.7, 347.5,
     S 325.6, 304.0/
      DATA (TO3B(I,16),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1233.1,1184.0,1133.7,1083.9,1034.4,
     S 984.7, 935.2, 886.0, 837.3, 789.2, 741.9, 695.8, 651.2, 608.5,
     S 568.4, 531.3, 497.3, 466.6, 438.6, 412.6, 388.0, 364.3, 341.1,
     S 318.3, 295.7/
      DATA (TO3B(I,17),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1233.1,1184.0,1133.7,1083.9,1034.2,
     S 984.5, 935.0, 885.8, 837.0, 788.8, 741.4, 695.1, 650.2, 607.2,
     S 566.6, 528.9, 494.3, 462.7, 433.8, 406.9, 381.3, 356.6, 332.4,
     S 308.5, 284.9/
      DATA (TO3B(I,18),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1386.3,1334.0,1282.9,1233.1,1184.0,1133.7,1083.9,1034.0,
     S 984.3, 934.8, 885.5, 836.6, 788.2, 740.6, 694.0, 648.7, 605.2,
     S 564.0, 525.4, 489.9, 457.2, 427.2, 399.1, 372.4, 346.5, 321.2,
     S 296.3, 271.8/
      DATA (TO3B(I,19),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.7,1083.6,1033.8,
     S 984.1, 934.5, 885.1, 836.0, 787.5, 739.6, 692.6, 646.8, 602.6,
     S 560.5, 520.8, 484.0, 450.0, 418.5, 389.0, 361.0, 333.9, 307.6,
     S 281.9, 256.8/
      DATA (TO3B(I,20),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.2,1083.3,1033.5,
     S 983.8, 934.0, 884.6, 835.4, 786.6, 738.3, 690.9, 644.4, 599.4,
     S 556.2, 515.2, 476.8, 441.1, 407.9, 376.9, 347.6, 319.4, 292.2,
     S 265.9, 240.3/
      DATA (TO3B(I,21),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.2,1083.0,1033.3,
     S 983.4, 933.7, 884.1, 834.7, 785.6, 737.0, 689.0, 641.9, 596.0,
     S 551.6, 509.1, 468.9, 431.3, 396.3, 363.6, 332.9, 303.7, 275.8,
     S 249.0, 223.2/
      DATA (TO3B(I,22),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.2,1083.0,1033.1,
     S 983.2, 933.4, 883.7, 834.1, 784.8, 735.8, 687.4, 639.6, 592.8,
     S 547.2, 503.2, 461.2, 421.6, 384.6, 350.1, 317.9, 287.7, 259.1,
     S 231.9, 206.1/
      DATA (TO3B(I,23),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1132.7,1083.0,1033.1,
     S 983.1, 933.2, 883.4, 833.7, 784.2, 734.9, 686.1, 637.7, 590.1,
     S 543.5, 498.1, 454.5, 413.0, 373.9, 337.5, 303.6, 272.2, 242.8,
     S 215.3, 189.6/
      DATA (TO3B(I,24),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1132.7,1083.0,1032.9,
     S 983.1, 933.1, 883.3, 833.5, 783.8, 734.3, 685.1, 636.3, 588.1,
     S 540.6, 494.2, 449.2, 406.0, 364.9, 326.5, 290.9, 258.0, 227.7,
     S 199.9, 174.2/
      DATA (TO3B(I,25),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1132.7,1083.0,1032.9,
     S 983.0, 933.1, 883.2, 833.3, 783.5, 733.9, 684.5, 635.3, 586.6,
     S 538.6, 491.3, 445.2, 400.6, 358.0, 317.7, 280.2, 245.7, 214.3,
     S 185.9, 160.3/
      DATA (TO3B(I,26),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1132.7,1083.0,1032.9,
     S 983.0, 933.0, 883.1, 833.2, 783.4, 733.6, 684.0, 634.7, 585.7,
     S 537.1, 489.3, 442.4, 396.8, 352.8, 311.0, 271.8, 235.8, 203.1,
     S 173.9, 148.1/
      DATA (TO3B(I,27),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.2,1083.0,1033.1,
     S 983.1, 933.0, 883.1, 833.2, 783.3, 733.5, 683.8, 634.3, 585.1,
     S 536.2, 488.0, 440.5, 394.1, 349.2, 306.2, 265.7, 228.2, 194.3,
     S 164.2, 138.0/
      DATA (TO3B(I,28),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.2,1083.0,1033.1,
     S 983.1, 933.1, 883.1, 833.2, 783.3, 733.4, 683.7, 634.1, 584.7,
     S 535.7, 487.1, 439.3, 392.4, 346.8, 302.9, 261.4, 222.8, 187.7,
     S 156.7, 130.0/
      DATA (TO3B(I,29),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.2,1083.0,1033.1,
     S 983.2, 933.2, 883.2, 833.3, 783.3, 733.5, 683.7, 634.0, 584.5,
     S 535.4, 486.6, 438.5, 391.3, 345.2, 300.8, 258.5, 219.0, 183.1,
     S 151.2, 124.0/
      DATA (TO3B(I,30),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.2,1083.3,1033.3,
     S 983.3, 933.3, 883.3, 833.4, 783.4, 733.6, 683.7, 634.0, 584.5,
     S 535.2, 486.4, 438.1, 390.6, 344.3, 299.4, 256.6, 216.5, 179.9,
     S 147.5, 119.8/
      DATA (TO3B(I,31),I=1,38)/
     S1663.6,1663.6,1663.6,1663.6,1663.6,1663.6,1594.2,1524.9,1484.4,
     S1433.3,1380.2,1334.0,1282.9,1233.1,1183.2,1133.7,1083.3,1033.5,
     S 983.5, 933.5, 883.5, 833.6, 783.6, 733.8, 683.9, 634.2, 584.6,
     S 535.3, 486.3, 438.0, 390.3, 343.8, 298.6, 255.5, 215.0, 177.8,
     S 144.9, 116.9/
C----- 1.0E4*O3A(I,J) FOR TB 
      DATA (O3A(I, 1),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  23.,  28.,
     S  34.,  40.,  45.,  49.,  52.,  55.,  57.,  58./
      DATA (O3A(I, 2),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  23.,  28.,
     S  34.,  40.,  44.,  49.,  52.,  55.,  57.,  58./
      DATA (O3A(I, 3),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  23.,  28.,
     S  34.,  40.,  44.,  49.,  52.,  55.,  57.,  58./
      DATA (O3A(I, 4),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  23.,  28.,
     S  34.,  40.,  44.,  49.,  52.,  55.,  57.,  58./
      DATA (O3A(I, 5),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  23.,  28.,
     S  34.,  40.,  44.,  49.,  52.,  55.,  57.,  58./
      DATA (O3A(I, 6),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  23.,  28.,
     S  34.,  40.,  44.,  49.,  52.,  55.,  57.,  58./
      DATA (O3A(I, 7),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  23.,  28.,
     S  34.,  40.,  44.,  48.,  52.,  54.,  56.,  58./
      DATA (O3A(I, 8),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  22.,  28.,
     S  34.,  39.,  44.,  48.,  52.,  54.,  56.,  57./
      DATA (O3A(I, 9),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  22.,  28.,
     S  34.,  39.,  44.,  48.,  51.,  54.,  56.,  57./
      DATA (O3A(I,10),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  22.,  28.,
     S  34.,  39.,  44.,  48.,  51.,  54.,  55.,  56./
      DATA (O3A(I,11),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  22.,  28.,
     S  34.,  39.,  44.,  47.,  50.,  53.,  54.,  55./
      DATA (O3A(I,12),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  12.,  17.,  22.,  28.,
     S  33.,  38.,  43.,  47.,  50.,  52.,  53.,  53./
      DATA (O3A(I,13),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,   0.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   9.,  12.,  17.,  22.,  27.,
     S  33.,  38.,  42.,  45.,  48.,  50.,  51.,  51./
      DATA (O3A(I,14),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,   0.,
     S   1.,   1.,   3.,   4.,   6.,   8.,  12.,  16.,  21.,  26.,
     S  32.,  36.,  41.,  44.,  46.,  47.,  48.,  47./
      DATA (O3A(I,15),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,
     S   1.,   1.,   2.,   4.,   6.,   8.,  11.,  15.,  20.,  25.,
     S  30.,  35.,  38.,  41.,  43.,  44.,  44.,  43./
      DATA (O3A(I,16),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,
     S   0.,   1.,   2.,   3.,   5.,   7.,  10.,  14.,  19.,  23.,
     S  28.,  32.,  35.,  38.,  39.,  39.,  39.,  38./
      DATA (O3A(I,17),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,   0.,   0.,
     S   0.,   1.,   2.,   3.,   4.,   6.,   9.,  13.,  17.,  21.,
     S  25.,  28.,  31.,  33.,  34.,  34.,  34.,  33./
      DATA (O3A(I,18),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,
     S   0.,   0.,   1.,   2.,   3.,   5.,   8.,  10.,  14.,  17.,
     S  21.,  24.,  26.,  28.,  29.,  29.,  28.,  28./
      DATA (O3A(I,19),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S   0.,   0.,   0.,   1.,   2.,   4.,   6.,   8.,  11.,  14.,
     S  17.,  19.,  21.,  23.,  23.,  24.,  24.,  23./
      DATA (O3A(I,20),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,   0.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,   0.,   0.,   1.,   2.,   4.,   5.,   7.,  10.,
     S  12.,  14.,  16.,  18.,  19.,  19.,  20.,  20./
      DATA (O3A(I,21),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,   0.,   0.,  -3.,  -2.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,   0.,   0.,   1.,   2.,   3.,   5.,   6.,
     S   8.,  10.,  12.,  14.,  15.,  16.,  17.,  17./
      DATA (O3A(I,22),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,  -4.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,   0.,   0.,   1.,   2.,   3.,   4.,
     S   6.,   7.,   9.,  11.,  12.,  14.,  15.,  16./
      DATA (O3A(I,23),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,  -4.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,   1.,   1.,   2.,
     S   4.,   5.,   7.,   9.,  10.,  12.,  13.,  14./
      DATA (O3A(I,24),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,  -4.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -2.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,   1.,   1.,
     S   2.,   4.,   5.,   7.,   9.,  10.,  12.,  13./
      DATA (O3A(I,25),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,  -4.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,   1.,
     S   2.,   3.,   4.,   6.,   7.,   9.,  11.,  12./
      DATA (O3A(I,26),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,  -4.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,
     S   1.,   2.,   3.,   4.,   6.,   8.,  10.,  11./
      DATA (O3A(I,27),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,  -4.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,
     S   1.,   1.,   2.,   4.,   5.,   7.,   9.,  11./
      DATA (O3A(I,28),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,   0.,   0.,  -3.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,
     S   0.,   1.,   2.,   3.,   5.,   6.,   8.,  10./
      DATA (O3A(I,29),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,   0.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,
     S   0.,   1.,   2.,   3.,   4.,   6.,   7.,   9./
      DATA (O3A(I,30),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,   0.,   0.,  -1.,  -2.,  -1.,  -2.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,
     S   0.,   1.,   1.,   2.,   4.,   5.,   7.,   9./
      DATA (O3A(I,31),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  -6.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,
     S   0.,   0.,   1.,   2.,   3.,   5.,   6.,   8./
C----- 1.0E6*O3B(I,J) FOR TB 
      DATA (O3B(I, 1),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -1.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   4.,   5.,   6./
      DATA (O3B(I, 2),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -1.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   4.,   5.,   6./
      DATA (O3B(I, 3),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -1.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   4.,   5.,   6./
      DATA (O3B(I, 4),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -1.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   4.,   5.,   6./
      DATA (O3B(I, 5),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -2.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   4.,   5.,   6./
      DATA (O3B(I, 6),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -2.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   4.,   5.,   6./
      DATA (O3B(I, 7),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -2.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   4.,   5.,   6./
      DATA (O3B(I, 8),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -2.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   4.,   5.,   6./
      DATA (O3B(I, 9),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -2.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   5.,   5.,   6./
      DATA (O3B(I,10),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -2.,  -2.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   3.,   5.,   5.,   6./
      DATA (O3B(I,11),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,  -2.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   4.,   5.,   6.,   6./
      DATA (O3B(I,12),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -5.,  -5.,  -4.,
     S  -3.,  -1.,   1.,   2.,   4.,   5.,   6.,   6./
      DATA (O3B(I,13),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,  -2.,
     S  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -4.,  -4.,  -4.,
     S  -2.,  -1.,   1.,   3.,   4.,   5.,   6.,   6./
      DATA (O3B(I,14),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   3.,  -2.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -2.,  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -4.,  -3.,
     S  -2.,  -1.,   1.,   3.,   4.,   5.,   6.,   6./
      DATA (O3B(I,15),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,   3.,  -2.,   0.,  -1.,  -1.,  -1.,  -1.,
     S  -2.,  -2.,  -2.,  -2.,  -3.,  -3.,  -4.,  -4.,  -4.,  -3.,
     S  -2.,   0.,   2.,   3.,   4.,   5.,   6.,   6./
      DATA (O3B(I,16),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,  -3.,  -2.,  -2.,  -1.,  -1.,  -1.,  -1.,
     S  -2.,  -2.,  -2.,  -2.,  -2.,  -3.,  -3.,  -3.,  -3.,  -3.,
     S  -1.,   0.,   2.,   3.,   4.,   5.,   6.,   6./
      DATA (O3B(I,17),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   0.,  -4.,  -3.,   0.,  -2.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -2.,  -2.,  -2.,  -2.,  -3.,  -3.,  -3.,  -2.,
     S  -1.,   1.,   2.,   3.,   4.,   5.,   5.,   5./
      DATA (O3B(I,18),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S  12.,   0.,  -4.,  -3.,   2.,   0.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -2.,  -2.,  -2.,  -2.,  -2.,  -1.,
     S   0.,   1.,   2.,   3.,   4.,   4.,   4.,   4./
      DATA (O3B(I,19),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   0.,  -4.,  -3.,  -2.,   0.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S   0.,   1.,   2.,   3.,   3.,   3.,   3.,   3./
      DATA (O3B(I,20),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   0.,   0.,  -3.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,
     S   0.,   1.,   2.,   2.,   2.,   2.,   2.,   2./
      DATA (O3B(I,21),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   0.,   0.,   0.,   0.,  -1.,  -1.,  -1.,  -1.,   0.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,
     S   0.,   1.,   1.,   1.,   2.,   2.,   1.,   1./
      DATA (O3B(I,22),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   7.,   0.,   3.,   0.,   1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,   0.,
     S   0.,   0.,   0.,   1.,   1.,   1.,   1.,   0./
      DATA (O3B(I,23),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   7.,   0.,   3.,   0.,  -1.,  -1.,   0.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0./
      DATA (O3B(I,24),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   7.,   0.,   3.,   0.,  -1.,  -1.,  -1.,   0.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,   0.,   0.,   0.,   0.,   0.,   0./
      DATA (O3B(I,25),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   7.,   0.,   3.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,   0.,  -1./
      DATA (O3B(I,26),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S   0.,   7.,   0.,   3.,   0.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1./
      DATA (O3B(I,27),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   7.,   0.,   3.,   0.,   1.,  -1.,   0.,   0.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1./
      DATA (O3B(I,28),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   0.,   0.,   0.,   0.,   1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1./
      DATA (O3B(I,29),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   0.,   0.,  -3.,   0.,  -1.,  -1.,  -1.,   0.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -2.,  -2./
      DATA (O3B(I,30),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   0.,   0.,  -3.,   0.,  -1.,  -1.,  -1.,   0.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -2.,  -2.,  -2./
      DATA (O3B(I,31),I=1,38)/
     S   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     S -12.,   0.,  -4.,  -3.,  -2.,   0.,  -1.,  -1.,   0.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,  -1.,
     S  -1.,  -1.,  -1.,  -1.,  -1.,  -2.,  -2.,  -2./
c
      DO 1000 I=1,40
        DO 1000 J=1,31
          DO 1000 K=1,2
            ALP(I,J,K)=ALP(I,J,K)*1.D-04
            BET(I,J,K)=BET(I,J,K)*1.D-07
 1000 CONTINUE
      DO 1001 I=1,38
        DO 1001 J=1,31
          AO3(I,J)=AO3(I,J)*1.D-04
          BO3(I,J)=BO3(I,J)*1.D-06
          O3A(I,J)=O3A(I,J)*1.D-04
          O3B(I,J)=O3B(I,J)*1.D-06
 1001 CONTINUE
      RETURN
      END
C
*DECK SUSW
      SUBROUTINE SUSW
C
C**** *SUSW*   - INITIALIZE COMMON YOMSW
C
C     PURPOSE.
C     --------
C           INITIALIZE YOMSW, THE COMMON THAT CONTAINS COEFFICIENTS
C           NEEDED TO RUN THE SHORTWAVE RADIATION SUBROUTINES
C
C**   INTERFACE.
C     ----------
C        *CALL* *SUSW
C
C        EXPLICIT ARGUMENTS
C        --------------------
C        NONE
C
C        IMPLICIT ARGUMENTS
C        --------------------
C        COMMON YOMSW
C
C     METHOD.
C     -------
C        SEE DOCUMENTATION
C
C     EXTERNALS.
C     ----------
C
C     REFERENCE.
C     ----------
C        ECMWF RESEARCH DEPARTMENT DOCUMENTATION
C
C     AUTHOR.
C     -------
C        JEAN-JACQUES MORCRETTE *ECMWF*
C
C     MODIFICATIONS.
C     --------------
C        ORIGINAL : 88-12-15
C     ------------------------------------------------------------------
C
*CALL ZPARB 
c      INCLUDE 'zparb.upd'
C
C      ----------------------------------------------------------------
C
C*       1.    SET VALUES.
C              -----------
C
c      DATA SUN(1) / 0.441676 /
      DATA SUN(1) / 0.441130 /
      DATA (D(1,K),K = 1,3) / 0.00, 0.00, 0.00 /
      DATA ((APAD(1,I,J),I=1,3),J=1,7) /
     S 0.000000000E-00, 0.000000000E-00, 0.925887084E-04,
     S 0.000000000E-00, 0.000000000E-00, 0.129353723E-01,
     S 0.000000000E-00, 0.000000000E-00, 0.800821928E+00,
     S 0.000000000E-00, 0.000000000E-00, 0.242715973E+02,
     S 0.000000000E-00, 0.000000000E-00, 0.878331486E+02,
     S 0.000000000E-00, 0.000000000E-00, 0.191559725E+02,
     S 0.000000000E-00, 0.000000000E-00, 0.000000000E+00 /
C
      DATA ((BPAD(1,I,J),I=1,3),J=1,7) /
     S 0.000000000E-00, 0.000000000E-00, 0.925887084E-04,
     S 0.000000000E-00, 0.000000000E-00, 0.131812683E-01,
     S 0.000000000E-00, 0.000000000E-00, 0.812706117E+00,
     S 0.000000000E-00, 0.000000000E-00, 0.249863591E+02,
     S 0.000000000E-00, 0.000000000E-00, 0.931071925E+02,
     S 0.000000000E-00, 0.000000000E-00, 0.252233437E+02,
     S 0.000000000E-00, 0.000000000E-00, 0.100000000E+01 /
C
      DATA (CRAY(1,K),K=1,6) /
     S .428937E-01, .890743E+00,-.288555E+01,
     S .522744E+01,-.469173E+01, .161645E+01/
C
c      DATA SUN(2) / 0.558324 /
      DATA SUN(2) / 0.557633 /
      DATA (D(2,K),K=1,3) / 0.317735694, 0.775570952, 0.800000000 /
      DATA ((APAD(2,I,J),I=1,3),J=1,7) /
     S 0.822745535E-02, 0.145321703E-04, 0.410177786E+03,
     S 0.705825794E+01, 0.175741897E-01, 0.672595424E+02,
     S 0.348747605E+03, 0.259696276E+01, 0.000000000E-00,
     S 0.174921268E+04, 0.599852834E+02, 0.000000000E-00,
     S 0.100138721E+04, 0.203510317E+03, 0.000000000E-00,
     S 0.518496206E+02, 0.757222990E+02, 0.000000000E-00,
     S 0.000000000E+00, 0.000000000E+00, 0.000000000E+00 /
C
      DATA ((BPAD(2,I,J),I=1,3),J=1,7) /
     S 0.822745535E-02, 0.145321703E-04, 0.410177786E+03,
     S 0.719686994E+01, 0.176779370E-01, 0.731185438E+02,
     S 0.381961022E+03, 0.265802733E+01, 0.100000000E+01,
     S 0.219460901E+04, 0.634292876E+02, 0.000000000E+00,
     S 0.159321079E+04, 0.228829763E+03, 0.000000000E+00,
     S 0.138748279E+03, 0.992586506E+02, 0.000000000E+00,
     S 0.100000000E+01, 0.100000000E+01, 0.000000000E+00 /
C
      DATA (CRAY(2,K),K=1,6) /
     S .697200E-02, .173297E-01,-.850903E-01,
     S .248261E+00,-.302031E+00, .129662E+00/
C
      RETURN
      END
C
*DECK INICON
      SUBROUTINE INICON
C
C*** **INICON  PRESET CONSTANTS IN *COMCON*.
C
C      M.JARRAUD      E.C.M.W.F.     10/12/1982.
C
C     PURPOSE.
C     --------
C
C             PRESET CONSTANTS IN *COMCON*.
C
C**   INTERFACE.
C     ----------
C
C             **INICON IS CALLED FROM *SETDYN*.
C
C     EXTERNALS.
C     ----------
C
C             NONE.
C
*CALL ZPARB 
c      INCLUDE 'zparb.upd'
C
   10 CONTINUE
C
C     ------------------------------------------------------------
C
C*        1.       PRESET CONSTANTS.
C                  ------ ----------
C
  100 CONTINUE
C
      API=2.*ASIN(1.D0)
      A=6371000.
      OMEGA=.7292E-4
      G=9.80665
      CPD=1005.46
      CPV=1869.46
      RD=287.05
      RV=461.51
C
      RCPD=1./CPD
      VTMPC1=RV/RD-1.
      VTMPC2=CPV/CPD-1.
C
      RHOH2O=1000.
      ALV=2.5008E6
      ALS=2.8345E6
      ALF=ALS-ALV
C
      CLW=4186.84
      TMELT=273.16
C
      SOLC=1376.
      STBO=5.67E-8
C
      DAYL=86400.
      YEARL=365.2425
C
      C1ES=610.78
      C2ES=C1ES*RD/RV
      C3LES=17.269
      C3IES=21.875
      C4LES=35.86
      C4IES=7.66
      C5LES=C3LES*(TMELT-C4LES)
      C5IES=C3IES*(TMELT-C4IES)
C
C     ------------------------------------------------------------
C
      RETURN
      END
C***************************************************************
C
C   FUNCTION CVMGT
C
C***************************************************************
C
       REAL FUNCTION CVMGT(Z1, Z2, LO)
C
       IMPLICIT NONE
C
       REAL Z1, Z2
       LOGICAL LO
C
       IF (LO) THEN
          CVMGT = Z1
       ELSE
          CVMGT = Z2
       END IF
C
       RETURN
       END
C***************************************************************
C
C   FUNCTION CVMGI
C
C***************************************************************
C
       INTEGER FUNCTION CVMGI(I1, I2, LO)
C
       IMPLICIT NONE
C
       INTEGER I1, I2
       LOGICAL LO
C
       IF (LO) THEN
          CVMGI = I1
       ELSE
          CVMGI = I2
       END IF
C
       RETURN
       END
C
C***************************************************************
C
C   FUNCTION CVMGM
C
C***************************************************************
C
       REAL FUNCTION CVMGM(Z1, Z2, Z3)
C
       IMPLICIT NONE
C
       REAL Z1, Z2, Z3
C
       IF (Z3 .LT. 0) THEN
          CVMGM = Z1
       ELSE
          CVMGM = Z2
       END IF
C
       RETURN
       END
C
C**************************************************************
C
C   FUNCTION SIGMA2
C
C***************************************************************
C
       REAL FUNCTION SIGMA2(N, SX, INCX)
C
       IMPLICIT NONE
C
       INTEGER N, INCX, I
       REAL SX(N*INCX)
C
       SIGMA2 = 0.
       DO 1 I=1,N
          SIGMA2 = SIGMA2 + SX(I*INCX)
 1     CONTINUE
C
       END

*DECK SOLANG
      SUBROUTINE SOLANG (LDIUR,DOY,YCLOCK,ALAT,alon,
     :     AMU0,RDAYL,CDISSEM)  
C Pm.M. deF   27-1-98
C inputs
C LDIUR  ! Logical for diurnal average
C DOY    ! Julian day of year
C YCLOCK ! Don't know yet
C ALAT   ! Latitude in degrees
C ALON   ! Longitude in degrees
C outputs
C AMU0   ! Cosine of solar zenith angle
C RDAYL  ! Fractional day length
C CDISSEM  ! Suns relative distance as a fraction

C**** *SOLANG* - FOR SOLAR ZENITH ANGLE AND RELATIVE DAYLENGTH.        
C                                                                      
C     PURPOSE.                                                         
C     --------                                                         
C                                                                      
C          THIS ROUTINE GIVES DIFFERENT RESULTS DEPENDING ON A LOGICAL 
C     SWITCH. IF LDIUR IS TRUE ONE OBTAINS ACTUAL SOLAR ZENITH ANGLES  
C     AND VALUES OF ONE OR ZERO DEPENDING ON THE SIGN OF THE FORMER. IF
C     LDIUR IS FALSE ONE GETS THE SAME ANSWERS AT ALL POINTS, I.E. MEAN
C     VALUE OF THE DAYTIME SOLAR ZENITH ANGLE AND RELATIVE LENGTH OF   
C     THE DAY.                                                         
C                                                                      
C   LDIUR .... true: sun at time, false: diurnally averaged
C
C     ----------                               
C                                              
      REAL DOY,ALAT,RDAYL,AMU0,YCLOCK,ALON,XLON
C
      REAL XLAT,YTIME,YEARL,API,ZC1YT,ZS1YT,ZC2YT,ZS2YT,CDISSEM
      INTEGER JDAY

      REAL ZMU0(128),ZRDAYL(128)
      LOGICAL LDIUR,LO
      REAL ZCDIS(5),ZCEQT(5),ZCDEC(5)
      DATA ZCDIS/+1.000110,+0.034221,+0.001280,+0.000719,+0.000077/
      DATA CRAE/+0.1277E-02/
      DATA ZCDEC/+0.006918,-0.399912,+0.070257,-0.006758,+0.000907/
      DATA ZCEQT/+0.000075,+0.001868,-0.032077,-0.014615,-0.040849/


      YEARL=365.2425
      API=2.0*ASIN(1.0)
      XLAT=API*alat/180.0
      XLON=API*ALON/180.0
      JDAY=INT(DOY)
      YTIME = (FLOAT(JDAY-1)+YCLOCK/2./API)/YEARL*2.*API

C           
C                                                                    
C  
C dirrnal cycle part
C
C*    COMPUTATIONAL CONSTANTS.
C     ------------- ----------
C
      ZC1YT=COS(YTIME)
      ZS1YT=SIN(YTIME)
      ZC2YT=ZC1YT**2-ZS1YT**2
      ZS2YT=2.*ZS1YT*ZC1YT
      CDISSEM=ZCDIS(1)+ZCDIS(2)*ZC1YT+ZCDIS(3)*ZS1YT+ZCDIS(4)*ZC2YT
     *       +ZCDIS(5)*ZS2YT
C
      ZCRAE=CRAE*(CRAE+2.)
C     ------------------------------------------------------------------
C*         2.     SOLAR ANGLE AND OZONE/AEROSOL PARAMETERS COMPUTATIONS.
C                 ----- ----- --- ------------- ---------- -------------
C
 200  CONTINUE
C
C*         2.1     INTRODUCE THE LATITUDE DEPENDENCY.
      ZSIN = SIN(XLAT)
      ZSQCST = SQRT(1.0-ZSIN**2)
      ZDECLI=ZCDEC(1)+ZCDEC(2)*ZC1YT+ZCDEC(3)*ZS1YT+ZCDEC(4)*ZC2YT   
     *       +ZCDEC(5)*ZS2YT                                         
      ZEQTIM=ZCEQT(1)+ZCEQT(2)*ZC1YT+ZCEQT(3)*ZS1YT+ZCEQT(4)*ZC2YT   
     *       +ZCEQT(5)*ZS2YT                                       
      ZZEN1=SIN(ZDECLI)                                             
      ZZEN2=COS(ZDECLI)*COS(YCLOCK+ZEQTIM)                        
      ZZEN3=COS(ZDECLI)*SIN(YCLOCK+ZEQTIM)                      
C
      ZTIM1 = ZZEN1 * ZSIN
      ZTIM2 =-ZZEN2 * ZSQCST
      ZTIM3 = ZZEN3 * ZSQCST
C
C     ---------------------------------------------------------------
C                                                                    
C*         2.     COMPUTATIONS IF DIURNAL CYCLE "ON".                
C                 ------------ -- ------- ----- -----                
C                                                                    
C***                         

c      COSLON=1.0
c      SINLON=0.0                                        
       COSLON=COS(XLON)
       SINLON=SIN(XLON)
      IF(LDIUR) THEN                                                 
C***
      AMU0=ZTIM1+ZTIM2*COSLON+ZTIM3*SINLON               
      LO=AMU0.GE.0.                                              
      AMU0=CVMGT(AMU0,0.0,LO)
      RDAYL=CVMGT(1.0,0.0,LO)
      IF (LO) THEN
        RDAYL=1.0
        AMU0=AMU0
      ELSE
        RDAYL=0.0
        AMU0=0.0
      ENDIF
C                                                                    
C     -----------------------------------------------------------------
C                                                                      
C*         3.     COMPUTATIONS IF DIURNAL CYCLE "OFF".                 
C                 ------------ -- ------- ----- ------                 
      ELSE                                                             
C***                                                                   
      DO 301 JL=1,128
      ZL=2.*API*(JL-1.)/128.
      ZMU0(JL)=ZTIM1+ZTIM2*COS(ZL)+ZTIM3*SIN(ZL)

      LO=ZMU0(JL).GE.0.                                                
      ZMU0(JL)=CVMGT(ZMU0(JL),0.0,LO)                                 
      ZRDAYL(JL)=CVMGT(1.0,0.0,LO)
  301 CONTINUE                                                         
      ZS1=SIGMA2(128,ZMU0(1),1)                                        
      ZS2=SIGMA2(128,ZRDAYL(1),1)                                      
      IF(ZS2.NE.0.) THEN                                               
      ZS1=ZS1/ZS2                                                      
      ZS2=ZS2/128.                                                     
      END IF                                                           
      AMU0=ZS1                                                     
      RDAYL=ZS2                                                    
C***              
      END IF                                                           
C                
      AMU0=CRAE/(SQRT(AMU0**2+ZCRAE)-AMU0)

      RETURN                          
      END                             
