      SUBROUTINE inipar
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *inipar*  - Get run parameters
C
C     Purpose:
C     -------
C     Reads and prints out run parameters.
C
C**   Interface:
C     ---------
C       *CALL*  *inipar*
C
C     Input:
C     -----
C     None
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     parse
C
C     Reference:
C     ---------
C     See OASIS manual (1995) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       1.0       L. Terray      94/01/01  created
C       1.1       L. Terray      94/08/01  modified: change in namelist
C                                          nice flag + new case for nmode
C       1.1       L. Terray      94/10/01  modified: change printing
C       2.0beta   L. Terray      95/07/24  modified: new structure
C       2.0       L. Terray      96/02/01  modified: lecture of cdqdt for
C                                          subgrid and add mozaic analysis
C                                          Lecture of a unit for filling
C       2.1       L. Terray      96/09/25  Changes to mozaic and subgrid
C                                          analysis, addition of nfend and
C                                          nintflx, check[in-out] analysis
C                                          addition of nointerp case.
C       2.2       L. Terray      97/02/12  Printing bug on analysis sub-
C                                          grid (SOLAR) corrected
C       2.2       L. Terray      97/02/20  Printing bug on analysis ANAIS
C                                          corrected
C       2.2       L. Terray      97/12/14  Add new input: MODINFO and new
C                                          extrapolation technique
C       2.3       S. Valcke      99/03/14  cjobnam with 3 or 4 characters
C       2.3       S. Valcke      99/03/25  troncature as NOxxxx in namcouple
C       2.3       S. Valcke      99/03/30  READ/WRITE flag and dataset index
C                                          for NINENN weights
C       2.3       S. Valcke      99/04/30  NLOGPRT for printing levels
C       2.3       L. Terray      99/09/15  changed periodicity variables
C                                          and input them as field parameters
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'parameter.h'
      INCLUDE 'clim.h'
      INCLUDE 'experiment.h'
      INCLUDE 'timestep.h'
      INCLUDE 'parallel.h'
      INCLUDE 'calendar.h'
      INCLUDE 'hardware.h'
      INCLUDE 'unit.h'
      INCLUDE 'field.h'
      INCLUDE 'string.h'
      INCLUDE 'analysis.h'
      INCLUDE 'anais.h'
      INCLUDE 'coast.h'
      INCLUDE 'label.h'
      INCLUDE 'rainbow.h'
      INCLUDE 'extrapol.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*80 clline, clvari
      CHARACTER*9 clword, clfield, clstring, clmach, clchan, clprint
      CHARACTER*9 cljob, clmod, cltime, clseq, cldate, clhead
      CHARACTER*3 clinfo
      CHARACTER*2 cldeb
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Get basic info for the simulation 
C        ---------------------------------
C
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE inipar  -  Level 0'
      WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' Initialization of run parameters'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' Reading input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
C
C* Initialize character keywords to locate appropriate input
C
      clfield  = ' $NFIELDS'
      clstring = ' $STRINGS'
      clmach   = ' $MACHINE'
      clchan   = ' $CHANNEL'
      cljob    = ' $JOBNAME'
      clmod    = ' $NBMODEL'
      cltime   = ' $RUNTIME'
      clseq    = ' $SEQMODE'
      cldate   = ' $INIDATE'
      clhead   = ' $MODINFO'
      clprint  = ' $NLOGPRT'
C
C* First get experiment name 
C
      REWIND nulin
 100  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 110) clword
      IF (clword .NE. cljob) GO TO 100
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $JOBNAME '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE IF (ilen .GT. 0 .AND. ilen .NE. 3 .AND. ilen .NE .4 ) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Input variable length is incorrect'
          WRITE (UNIT = nulou,FMT = *) ' ilen = ', ilen  
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Check $JOBNAME variable spelling '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE
          IF (ilen .EQ. 3) THEN
              WRITE (cjobnam,FMT='(A1,A3)') ' ',clvari
          ELSE IF (ilen .EQ. 4) THEN
              WRITE (cjobnam,FMT='(A4)') clvari
          ENDIF
      ENDIF
C
C* Print out experiment name
C
      CALL prcout
     $    ('The experiment name for this run is cjobnam =', cjobnam,1)
C
C* Get number of models involved in this simulation
C
      REWIND nulin
 120  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 130) clword
      IF (clword .NE. clmod) GO TO 120
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $NBMODEL '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE IF (ilen .GT. 1) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Input variable length is incorrect'
          WRITE (UNIT = nulou,FMT = *) ' There are too many models '
          WRITE (UNIT = nulou,FMT = *) ' ilen = ', ilen  
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Check $NBMODEL variable spelling '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
        ELSE
          READ (clvari,FMT = 1003) nmodel
      ENDIF
C
C* Print out the number of models
C
      CALL prtout
     $    ('The number of models for this run is nmodel =', nmodel, 1)
C
C* Get models name
C
      DO 140 jm = 1, nmodel
        imodel = jm + 1
        CALL parse (clline, clvari, imodel, jpeighty, ilen)
        cmodnam(jm) = clvari
C
C* Print out models name
C
        WRITE (UNIT = nulou,FMT ='
     $      (''   Name for model '',I1,'' is '',A6,/)') 
     $      jm, cmodnam(jm)
 140  CONTINUE 
C
C* Get hardware info for this OASIS simulation
C
C --> First get the type of machine OASIS is on
C 
      REWIND nulin
 150  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 160) clword
      IF (clword .NE. clmach) GO TO 150
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $MACHINE '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE IF (ilen .GT. 0 .AND. ilen .NE. 4) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Input variable length is incorrect'
          WRITE (UNIT = nulou,FMT = *) ' ilen = ', ilen  
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Check $MACHINE variable spelling '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE
          cmach = clvari
      ENDIF
C
C* Print out the type of machine
C
      CALL prcout
     $    ('The machine used to run OASIS is cmach =', cmach, 1)
C
C --> Second get the message passing technique we are using
C
      REWIND nulin
 170  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 180) clword
      IF (clword .NE. clchan) GO TO 170
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $CHANNEL '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE IF (ilen .GT. 0 .AND. ilen .NE. 4) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Input variable length is incorrect'
          WRITE (UNIT = nulou,FMT = *) ' ilen = ', ilen  
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Check $CHANNEL variable spelling '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
        ELSE
          cchan = clvari
      ENDIF
C
C* Print out the message passing technique
C
      CALL prcout
     $    (' The message passing used in OASIS is cchan =', cchan, 1)
C
C* Get total time for this simulation
C
      REWIND nulin
 190  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 191) clword
      IF (clword .NE. cltime) GO TO 190
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $RUNTIME '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE
          READ (clvari,FMT = 1005) ntime
      ENDIF
      write(nulou,*)'Actual value of ntime = ',ntime
C
C* Print out total time
C
      CALL prtout
     $    ('The total time for this run is ntime =', ntime, 1)
C
C* Get initial date for this simulation
C
      REWIND nulin
 192  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 193) clword
      IF (clword .NE. cldate) GO TO 192
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $INIDATE '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE
          READ (clvari,FMT = 1004) ndate
      ENDIF
C
C* Print out initial date
C
      CALL prtout
     $    ('The initial date for this run is ndate = ', ndate, 1)
C
C* Get number of sequential models involved in this simulation
C
      REWIND nulin
 194  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 195) clword
      IF (clword .NE. clseq) GO TO 194
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $SEQMODE '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE IF (ilen .GT. 1) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Input variable length is incorrect'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Sequential models are too many'
          WRITE (UNIT = nulou,FMT = *) ' ilen = ', ilen  
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Check $SEQMODE variable spelling '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
        ELSE
          READ (clvari,FMT = 1003) nmseq
      ENDIF
C
C* Print out the number of sequential models
C
      CALL prtout
     $    ('The number of sequential models is nmseq =', nmseq, 1)
C
C* Get the information mode for this simulation
C
      REWIND nulin
 196  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 197) clword
      IF (clword .NE. clhead) GO TO 196
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $MODINFO '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE IF (ilen .GT. 0 .AND. ilen .NE. 3) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Input variable length is incorrect'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Info mode uncorrectly specified'
          WRITE (UNIT = nulou,FMT = *) ' ilen = ', ilen  
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Check $MODINFO variable spelling '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
        ELSE
          clinfo = clvari
          IF (clinfo .EQ. 'YES') THEN 
              lmodinf = .TRUE. 
            ELSE 
              lmodinf = .FALSE. 
          ENDIF 
      ENDIF
C
C* Print out the information mode
C
      CALL prcout
     $    ('The information mode is activated ? ==>', clinfo, 1)
C
C* Get the printing level for this simulation
C
      REWIND nulin
 198  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 199) clword
      IF (clword .NE. clprint) GO TO 198
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $NLOGPRT '
          WRITE (UNIT = nulou,FMT = *) ' Default value 2 will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
      ELSE IF (ilen .NE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Input variable length is incorrect'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Printing level uncorrectly specified'
          WRITE (UNIT = nulou,FMT = *) ' ilen = ', ilen  
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Check $NLOGPRT variable spelling '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
      ELSE
          READ (clvari,FMT = 1003) nlogprt
      ENDIF
C
C* Print out the printing level
C
      CALL prtout
     $    ('The printing level is nlogprt =', nlogprt, 1)
C
C* Formats
C
 1001 FORMAT(A9)
 1002 FORMAT(A80)
 1003 FORMAT(I1)
 1004 FORMAT(I8)
 1005 FORMAT(I11)
C
C*    2. Get field information
C        ---------------------
C
C* Read in number of fields and their set of self-consistent strings (SSCS)
C
      REWIND nulin
 200  CONTINUE
      READ (UNIT = nulin,FMT = 2001,END = 210) clword
      IF (clword .NE. clfield) GO TO 200
      READ (UNIT = nulin,FMT = 2002) clline
      CALL parse(clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Nothing on input for $FIELDS '
          WRITE (UNIT = nulou,FMT = *) ' Default value will be used '
          WRITE (UNIT = nulou,FMT = *) ' '
        ELSE
          READ (clvari,FMT = 2003) nfield
      ENDIF
C
C* Print out the total number of fields exchanged
C
      CALL prtout
     $    ('The number of exchanged fields is nfield =', nfield, 1)
C
C* Initialize arrays needed for ANAIS(G-M), mapping and subgrid interpolation
C
      lcoast = .TRUE.
      DO 215 jz = 1, nfield
        linit(jz) = .TRUE.
        lmapp(jz) = .TRUE.
        lsubg(jz) = .TRUE.
        lextra(jz) = .TRUE.
        nmapfl(jz) = 1
        nsubfl(jz) = 1
        nextfl(jz) = 1
        naismfl(jz) = 1
        naisgfl(jz) = 1
        varmul(jz) = 1.
 215  CONTINUE 
C
C* Initialize flag indicating IF EXTRAP/NINENN parameter sets have 
C* already been calculated or read (.TRUE.) or not (.FALSE.)
C
      DO 217 jfn = 1, jpnfn
        lweight(jfn) = .FALSE.
 217  CONTINUE
C
C* Initialize flags identifying different EXTRAP/NINENN parameter sets
C* and different EXTRAP/NINENN parameter sets called by GLORED
C
      DO 219 jfn = 1, jpfield
        nninnfl(jfn) = 0
        nninnflg(jfn) = 0
 219  CONTINUE
C
C* Get the SSCS for all fields
C
      REWIND nulin
 220  CONTINUE
      READ (UNIT = nulin,FMT = 2001,END = 230) clword
      IF (clword .NE. clstring) GO TO 220
C
C* Loop on total number of fields (NoF)
C
      DO 240 jf = 1, nfield
C
C* Read first two lines of strings for field n = 1,2...,nfield
C      --->>> Main characteristics of fields
C
C* First line
C
        READ (UNIT = nulin,FMT = 2002) clline
        CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get input field symbolic name
        cnaminp(jf) = clvari
        CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get output field symbolic name
        cnamout(jf) = clvari
        CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get field label number
        READ (clvari,FMT = 2003) numlab(jf)
        CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get field exchange frequency
        READ (clvari,FMT = 2004) nfexch(jf)
        CALL parse(clline, clvari, 5, jpeighty, ilen)
C* Get total number of analysis
        READ (clvari,FMT = 2003) ntrans(jf)
        CALL parse(clline, clvari, 6, jpeighty, ilen)
C* Get input file name
        cficinp(jf) = clvari
        CALL parse(clline, clvari, 7, jpeighty, ilen)
C* Get output file name
        cficout(jf) = clvari
        CALL parse(clline, clvari, 8, jpeighty, ilen)
C* Get input logical unit
        READ (clvari,FMT = 2005) nluinp(jf) 
        CALL parse(clline, clvari, 9, jpeighty, ilen)
C* Get output logical unit
        READ (clvari,FMT = 2005) nluout(jf)
        CALL parse(clline, clvari, 10, jpeighty, ilen)
C* Get field status
        cstate(jf) = clvari
C
C* Second line
C
        READ (UNIT = nulin,FMT = 2002) clline
        CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get number of longitudes for initial field
        READ(clvari,FMT = 2004) nlonbf(jf)
        CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get number of latitudes for initial field
        READ(clvari,FMT = 2004) nlatbf(jf)
        CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get number of longitudes for final field
        READ(clvari,FMT = 2004) nlonaf(jf)
        CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get number of latitudes for final field
        READ(clvari,FMT = 2004) nlataf(jf)
        CALL parse(clline, clvari, 5, jpeighty, ilen)
C* Get root name for specific grid-related files (initial field)
        cficbf(jf) = clvari
        CALL parse(clline, clvari, 6, jpeighty, ilen)
C* Get root name for specific grid-related files (final field)
        cficaf(jf) = clvari
        CALL parse(clline, clvari, 7, jpeighty, ilen)
C* Get model sequential index
        READ(clvari,FMT = 2003) nseqn(jf)
        CALL parse(clline, clvari, 8, jpeighty, ilen)
C* Get model initialization flag
        READ(clvari,FMT = 2003) nfinit(jf)
        CALL parse(clline, clvari, 9, jpeighty, ilen)
C* Get extra time step flag
        READ(clvari,FMT = 2003) nfend(jf)
        CALL parse(clline, clvari, 10, jpeighty, ilen)
C* Get field integral flag
        READ(clvari,FMT = 2005) nintflx(jf)
C
C* Third line
C 
        READ (UNIT = nulin,FMT = 2002) clline     
        CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get source grid periodicity type
        csper(jf) = clvari
        IF(csper(jf) .NE. 'P' .AND. csper(jf) .NE. 'R') THEN
            CALL prtout
     $      ('ERROR in namcouple for source grid type of field', jf, 1)
            WRITE (UNIT = nulou,FMT = *) '==> must be P or R'
            CALL HALTE('STOP in inipar')
        ENDIF
C 
        CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get number of overlapped longitudes for the Periodic type source grid
        READ(clvari,FMT = 2005) nosper(jf)
        CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get target grid periodicity type
        ctper(jf) = clvari
        IF(ctper(jf) .NE. 'P' .AND. ctper(jf) .NE. 'R') THEN
            CALL prtout
     $      ('ERROR in namcouple for target grid type of field', jf, 1)
            WRITE (UNIT = nulou,FMT = *) '==> must be P or R'
            CALL HALTE('STOP in inipar')
        ENDIF
C
        CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get number of overlapped longitudes for the Periodic type target grid
        READ(clvari,FMT = 2005) notper(jf)
C
C* Read fourth line of strings if message passing is CLIM
C      --->>> Stuff related to parallel decomposition
C             for incoming data
C
        cparal(jf) = 'NONE'
        IF (cchan .EQ. 'CLIM') THEN
            READ (UNIT = nulin,FMT = 2002) clline
            CALL parse(clline, clvari, 1, jpeighty, ilen)
            cparal(jf) = clvari
            IF (cparal(jf) .EQ. 'SERIAL') THEN
                nparal(CLIM_Strategy,jf) = CLIM_serial
                nparal(CLIM_Length,jf) = nlonbf(jf) * nlatbf(jf)
                nparal(CLIM_Offset,jf) = 0
              ELSE IF (cparal(jf) .EQ. 'APPLE') THEN
                nparal(CLIM_Strategy,jf) = CLIM_Apple
                CALL parse(clline, clvari, 2, jpeighty, ilen)
                READ(clvari,FMT = 2004) nparal(CLIM_Length,jf)
                CALL parse(clline, clvari, 3, jpeighty, ilen)
                READ(clvari,FMT = 2004) nparal(CLIM_Offset,jf)
              ELSE IF (cparal(jf) .EQ. 'BOX') THEN
                nparal(CLIM_Strategy,jf) = CLIM_Box
                CALL parse(clline, clvari, 2, jpeighty, ilen)
                READ(clvari,FMT = 2004) nparal(CLIM_Offset,jf)
                CALL parse(clline, clvari, 3, jpeighty, ilen)
                READ(clvari,FMT = 2004) nparal(CLIM_SizeX,jf)
                CALL parse(clline, clvari, 4, jpeighty, ilen)
                READ(clvari,FMT = 2004) nparal(CLIM_SizeY,jf)
                CALL parse(clline, clvari, 5, jpeighty, ilen)
                READ(clvari,FMT = 2004) nparal(CLIM_LdX,jf)
              ELSE IF (cparal(jf) .EQ. 'ORANGE') THEN
                nparal(CLIM_Strategy,jf) = CLIM_Orange
                CALL parse(clline, clvari, 2, jpeighty, ilen)
                READ(clvari,FMT = 2004) nparal(CLIM_Segments,jf)
                DO 250 js = 1, nparal(CLIM_Segments,jf)
                  CALL parse(clline, clvari, js+2, jpeighty, ilen)
                  READ(clvari,FMT = 2004) nparal(CLIM_Segments+js,jf)
 250            CONTINUE 
              ELSE
                WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
                WRITE (UNIT = nulou,FMT = *)
     $              ' Parallel decomposition not implemented yet '
                WRITE (UNIT = nulou,FMT = *) 
     $              ' The decomposition required in OASIS is : '
                WRITE (UNIT = nulou,FMT = *) ' cparal = ', cparal(jf)
                WRITE (UNIT = nulou,FMT = *) ' with jf = ', jf
                WRITE (UNIT = nulou,FMT = *) ' '
                CALL HALTE ('STOP in inipar')
            ENDIF
        ENDIF 
C
C* Read fourth or fifth line of strings
C      --->>> Stuff related to field transformation
C
        READ (UNIT = nulin,FMT = 2002) clline
        DO 260 ja = 1, ntrans(jf)
          CALL parse(clline, clvari, ja, jpeighty, ilen)
C* Get the whole set of analysis to be performed
          canal(ja,jf) = clvari
 260    CONTINUE 
C
C* Now read specifics for each transformation
C
        DO 270 ja = 1, ntrans(jf)
C
C* Read next line unless if analysis is a checking one (no input)
C
          IF(canal(ja,jf) .NE. 'CHECKIN' .AND.
     $        canal(ja,jf) .NE. 'CHECKOUT' .AND.
     $        canal(ja,jf) .NE. 'NOINTERP') THEN 
              READ (UNIT = nulin,FMT = 2002) clline
          ENDIF 
          IF (canal(ja,jf) .EQ. 'MASK') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get mask value
              READ(clvari,FMT = 2006) amskval(jf)
            ELSE IF (canal(ja,jf) .EQ. 'MOZAIC') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get file name for grid mapping
              cgrdmap(jf) = clvari
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get related logical unit 
              READ(clvari,FMT = 2005) nlumap(jf)
              CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get dataset number
              READ(clvari,FMT = 2005) nmapfl(jf)
              CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get maximum number of neighbors for the grids associated to current field
              READ(clvari,FMT = 2003) nmapvoi(jf)              
            ELSE IF (canal(ja,jf) .EQ. 'INVERT') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get lat-lon ordering for initial fields
              cxordbf(jf) = clvari
              CALL parse(clline, clvari, 2, jpeighty, ilen)
              cyordbf(jf) = clvari
            ELSE IF (canal(ja,jf) .EQ. 'CHECKIN') THEN
C* Check input fields
              CONTINUE 
            ELSE IF (canal(ja,jf) .EQ. 'CHECKOUT') THEN
C* Check output fields
              CONTINUE
            ELSE IF (canal(ja,jf) .EQ. 'NOINTERP') THEN
C* No interpolation case
              CONTINUE
            ELSE IF (canal(ja,jf) .EQ. 'REVERSE') THEN
C* Get lat-lon ordering for final fields
              CALL parse(clline, clvari, 1, jpeighty, ilen)
              cxordaf(jf) = clvari
              CALL parse(clline, clvari, 2, jpeighty, ilen)
              cyordaf(jf) = clvari
            ELSE IF (canal(ja,jf) .EQ. 'EXTRAP') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get extrapolation method
              cextmet(jf) = clvari
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get number of neighbors used in extrapolation
C If extrapolation method is NINENN, next variable is the MINIMUM
C number of neighbors required (among the 8 closest) to perform
C the extrapolation (cannot be greater than 4 for convergence). 
C In case it is WEIGHT, it is the MAXIMUM number
C of neighbors required by the extrapolation operation.
C
              READ(clvari,FMT = 2003) neighbor(jf)
              IF (cextmet(jf) .EQ. 'NINENN' .AND. 
     $                       neighbor(jf) .GT. 4) THEN
                  neighbor(jf)=4
                  WRITE(UNIT = nulou,FMT = *) '        ***WARNING***'
                  WRITE(UNIT = nulou,FMT = *) 
     $                  'For EXTRAP/NINENN extrapolation' 
                  WRITE(UNIT = nulou,FMT = *) 
     $                  'the number of neighbors has been set to 4'
              ENDIF
C* If choice is NINENN, read one more data
              IF (cextmet(jf) .EQ. 'NINENN') THEN
                  CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get NINENN weights read/write flag
                  READ(clvari,FMT = 2005) niwtn(jf)
                  CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get NINENN dataset identificator
                  READ(clvari,FMT = 2005) nninnfl(jf)
                  IF (nninnfl(jf) .EQ. 0) THEN
                      WRITE(UNIT = nulou,FMT = *) '      ***WARNING***'
                      WRITE(UNIT = nulou,FMT = *) 
     $            'The EXTRAP/NINENN dataset identificator cannot be 0' 
                      CALL HALTE('STOP in inipar')
                  ENDIF
              ENDIF
C* If choice is WEIGHT, read more data
              IF (cextmet(jf) .EQ. 'WEIGHT') THEN
                  CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get file name for grid mapping
                  cgrdext(jf) = clvari
                  CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get related logical unit 
                  READ(clvari,FMT = 2005) nluext(jf)
                  CALL parse(clline, clvari, 5, jpeighty, ilen)
C* Get dataset number
                  READ(clvari,FMT = 2005) nextfl(jf)
              ENDIF 
            ELSE IF (canal(ja,jf) .EQ. 'INTERP') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get interpolation method
              cintmet(jf) = clvari
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get source grid type
              cgrdtyp(jf) = clvari
              CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get field type (scalar or vector)
              cfldtyp(jf) = clvari
C* If interpolation uses ANAIS(G-M), read in more data
              IF (cintmet(jf) .EQ. 'SURFMESH') THEN
                  CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get Anaism dataset identificator
                  READ(clvari,FMT = 2005) naismfl(jf)
                  CALL parse(clline, clvari, 5, jpeighty, ilen)
C* Get maximum number of neighbors for the grids related to current field
                  READ(clvari,FMT = 2003) naismvoi(jf)
                  CALL parse(clline, clvari, 6, jpeighty, ilen)
C* Get Anaism weights read/write flag
                  READ(clvari,FMT = 2005) niwtm(jf)
              ENDIF 
              IF (cintmet(jf) .EQ. 'GAUSSIAN') THEN
                  CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get Anaisg dataset identificator
                  READ(clvari,FMT = 2005) naisgfl(jf)
                  CALL parse(clline, clvari, 5, jpeighty, ilen)
C* Get maximum number of neighbors for the grids related to current field
                  READ(clvari,FMT = 2003) naisgvoi(jf)
                  CALL parse(clline, clvari, 6, jpeighty, ilen)
C* Read variance multiplicator for gaussian weights
                  READ(clvari,FMT = 2006) varmul(jf)
                  CALL parse(clline, clvari, 7, jpeighty, ilen)
C* Get Anaisg weights read/write flag
                  READ(clvari,FMT = 2005) niwtg(jf)
              ENDIF
            ELSE IF (canal(ja,jf) .EQ. 'FILLING') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get data file name (used to complete the initial field array)
              cfilfic(jf) = clvari
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get logical unit connected to previous file
              READ(clvari,FMT = 2005) nlufil(jf)
              CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get filling method
              cfilmet(jf) = clvari
C* If current field is SST
              IF(cfilmet(jf)(4:6) .EQ. 'SST') THEN
                  CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get flag for coast mismatch correction
                  READ(clvari,FMT = 2005) nfcoast
                  IF(cfilmet(jf)(1:3) .EQ. 'SMO') THEN
                      CALL parse(clline, clvari, 5, jpeighty, ilen)
C* Get field name for flux corrective term 
                      cfldcor = clvari
                      CALL parse(clline, clvari, 6, jpeighty, ilen)
C* Get logical unit used to write flux corrective term
                      READ(clvari,FMT = 2005) nlucor
                  ENDIF
              ENDIF 
            ELSE IF (canal(ja,jf) .EQ. 'CONSERV') THEN            
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get conservation method
              cconmet(jf) = clvari
            ELSE IF (canal(ja,jf) .EQ. 'REDGLO') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get half the number of lats for reduced<->global gaussian grid switch
              READ(clvari,FMT = 2008) cldeb, ntronca(jf)
              IF (cldeb .NE. 'NO') THEN
                  CALL prcout
     $            ('ERROR in namcouple for analysis', canal(ja,jf), 1) 
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Since version 2.3, the information on the reduced'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'grid in namcouple has to be NOxx WHERE xx is half'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'the number of latitude lines.'
                  CALL HALTE ('STOP in inipar')
              ENDIF
C* Get extrapolation flag to go from reduced to global gaussian grid
              CALL parse(clline, clvari, 2, jpeighty, ilen)
              cmskrd(jf) = clvari
            ELSE IF (canal(ja,jf) .EQ. 'GLORED') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get gaussian troncature for reduced <-> global gaussian grid switch
              READ(clvari,FMT = 2008) cldeb, ntronca(jf)
              IF (cldeb .NE. 'NO') THEN
                  CALL prcout
     $            ('ERROR in namcouple for analysis', canal(ja,jf), 1) 
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Since version 2.3, the information on the reduced'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'grid in namcouple has to be NOxx WHERE xx is half'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'the number of latitude lines.'
                  CALL HALTE ('STOP in inipar')
              ENDIF
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get number of neighbors used in EXTRAP/NINENN extrapolation always
C  performed within GLORED (cannot be greater than 4 for convergence).
              READ(clvari,FMT = 2003) neighborg(jf)
              CALL parse(clline, clvari, 3, jpeighty, ilen)
              IF (neighborg(jf) .GT. 4) THEN
                  neighborg(jf)=4
                  WRITE(UNIT = nulou,FMT = *) '        ***WARNING***'
                  WRITE(UNIT = nulou,FMT = *) 
     $                  'For EXTRAP/NINENN extrapolation in GLORED' 
                  WRITE(UNIT = nulou,FMT = *) 
     $                  'the number of neighbors has been set to 4'
              ENDIF
C* Get EXTRAP/NINENN weights read/write flag
              READ(clvari,FMT = 2005) niwtng(jf)
              CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get NINENN dataset identificator
              READ(clvari,FMT = 2005) nninnflg(jf)
              IF (nninnflg(jf) .EQ. 0) THEN
                  WRITE(UNIT = nulou,FMT = *) '        ***WARNING***'
                  WRITE(UNIT = nulou,FMT = *) 
     $            'The EXTRAP/NINENN dataset identificator in GLORED' 
                  WRITE(UNIT = nulou,FMT = *) 
     $            'cannot be 0'
                  CALL HALTE('STOP in inipar')
              ENDIF
            ELSE IF (canal(ja,jf) .EQ. 'CORRECT') THEN
C* Get flux correction parameters
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get main field multiplicative coefficient
              READ(clvari,FMT = 2006) afldcoef(jf)
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get number of auxilary fields in correction formula
              READ(clvari,FMT = 2003) ncofld (jf)
C* Read auxilary field parameters
              icofld = ncofld(jf)
              DO 280 jc = 1, icofld
                READ (UNIT = nulin,FMT = 2002) clline   
                CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get symbolic names for additional fields
                ccofld(jc,jf) = clvari
                CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get multiplicative coefficients for  additional fields
                READ(clvari,FMT = 2006) acocoef (jc,jf)
                CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get file names for external data files 
                ccofic(jc,jf) = clvari
C* Get related logical units 
                CALL parse(clline, clvari, 4, jpeighty, ilen)
                READ(clvari,FMT = 2005) nludat(jc,jf)
 280          CONTINUE
            ELSE IF (canal(ja,jf) .EQ. 'BLASOLD') THEN
C* Get linear combination parameters for initial fields
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get main field multiplicative coefficient
              READ(clvari,FMT = 2006) afldcobo(jf)
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get number of additional fields in linear formula
              READ(clvari,FMT = 2003) nbofld (jf)
              DO 290 jc = 1, nbofld(jf)
                READ (UNIT = nulin,FMT = 2002) clline   
                CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get symbolic names for additional fields
                cbofld(jc,jf) = clvari
                CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get multiplicative coefficients for  additional fields
                READ(clvari,FMT = 2006) abocoef (jc,jf)
 290          CONTINUE
            ELSE IF (canal(ja,jf) .EQ. 'BLASNEW') THEN
C* Get linear combination parameters for final fields
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get main field multiplicative coefficient
              READ(clvari,FMT = 2006) afldcobn(jf)
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get number of additional fields in linear formula
              READ(clvari,FMT = 2003) nbnfld (jf)
              DO 291 jc = 1, nbnfld(jf)
                READ (UNIT = nulin,FMT = 2002) clline   
                CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get symbolic names for additional fields
                cbnfld(jc,jf) = clvari
                CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get multiplicative coefficients for  additional fields
                READ(clvari,FMT = 2006) abncoef (jc,jf)
 291          CONTINUE
C* Get fields to restore subgrid variability 
            ELSE IF (canal(ja,jf) .EQ. 'SUBGRID') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen)
C* Get file name for subgrid interpolation
              cgrdsub(jf) = clvari
              CALL parse(clline, clvari, 2, jpeighty, ilen)
C* Get related logical unit 
              READ(clvari,FMT = 2005) nlusub(jf)
              CALL parse(clline, clvari, 3, jpeighty, ilen)
C* Get dataset number
              READ(clvari,FMT = 2005) nsubfl(jf)
              CALL parse(clline, clvari, 4, jpeighty, ilen)
C* Get maximum number of neighbors for the grids related to current field
              READ(clvari,FMT = 2003) nsubvoi(jf)
              CALL parse(clline, clvari, 5, jpeighty, ilen)
C* Get type of subgrid interpolation (solar or non solar flux)
              ctypsub(jf) = clvari
              CALL parse(clline, clvari, 6, jpeighty, ilen)
C* Get additional field name on coarse grid
              cfldcoa(jf) = clvari
              CALL parse(clline, clvari, 7, jpeighty, ilen)
C* Get additional field name on fine grid
              cfldfin(jf) = clvari
              IF (ctypsub(jf) .EQ. 'NONSOLAR') THEN 
                  CALL parse(clline, clvari, 8, jpeighty, ilen)
C* Get coupling ratio on coarse grid
                  cdqdt(jf) = clvari
              ENDIF 
            ELSE 
              WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulou,FMT = *)
     $            ' Type of analysis not implemented yet '
              WRITE (UNIT = nulou,FMT = *) 
     $            ' The analysis required in OASIS is :'
              WRITE (UNIT = nulou,FMT = *) ' canal = ', canal(ja,jf)
              WRITE (UNIT = nulou,FMT = *) 
     $            ' with ja = ', ja, ' jf = ', jf
              WRITE (UNIT = nulou,FMT = *) ' '
              CALL HALTE ('STOP in inipar')
          ENDIF
 270    CONTINUE
C
C* End of loop on NoF
C 
 240  CONTINUE
C
C* Formats
C
 2001 FORMAT(A9)
 2002 FORMAT(A80)
 2003 FORMAT(I4)
 2004 FORMAT(I8)
 2005 FORMAT(I2)
 2006 FORMAT(E15.6)
 2008 FORMAT(A2,I4)
C
C*    3. Printing
C        --------
C* Warning: no indentation for the next if (nightmare ...)
      IF (nlogprt .GE. 1) THEN 
      DO 310 jf = 1, nfield
C* Local indexes
        ilab = numlab(jf)
        ifcb = ilenstr(cficbf(jf),jpeight)
        ifca = ilenstr(cficaf(jf),jpeight)
        WRITE (UNIT = nulou,FMT = 3001) jf
        WRITE (UNIT = nulou,FMT = 3002)
        WRITE (UNIT = nulou,FMT = 3003)
        WRITE (UNIT = nulou,FMT = 3004)
        WRITE (UNIT = nulou,FMT = 3005)
     $      cnaminp(jf), cnamout(jf), cfldlab(ilab), nfexch(jf),
     $      nseqn(jf), nfinit(jf), nfend(jf), nintflx(jf),
     $      cstate(jf), ntrans(jf), cparal(jf)
C* Warning: no indentation for the next if (nightmare ...)
        IF (nlogprt .EQ. 2) THEN 
        WRITE (UNIT = nulou,FMT = 3006)
     $      cficinp(jf), cficout(jf), nluinp(jf), nluout(jf)
        WRITE (UNIT = nulou,FMT = 3007)
     $      nlonbf(jf), nlatbf(jf), nlonaf(jf), nlataf(jf),
     $      csper(jf), nosper(jf), ctper(jf), notper(jf)
        WRITE (UNIT = nulou,FMT = 3008)
     $      cficbf(jf)(1:ifcb)//cglonsuf, cficbf(jf)(1:ifcb)//cglatsuf, 
     $      cficbf(jf)(1:ifcb)//cmsksuf, cficbf(jf)(1:ifcb)//csursuf,
     $      cficaf(jf)(1:ifca)//cglonsuf, cficaf(jf)(1:ifca)//cglatsuf,
     $      cficaf(jf)(1:ifca)//cmsksuf, cficaf(jf)(1:ifca)//csursuf
        WRITE (UNIT = nulou,FMT = 3009) 
        WRITE (UNIT = nulou,FMT = 3010)
        DO 320 ja = 1, ntrans(jf)
          WRITE (UNIT = nulou,FMT = 3011) ja, canal(ja,jf)
          IF (canal(ja,jf) .EQ. 'MASK') THEN
              WRITE(UNIT = nulou,FMT = 3012) amskval(jf)
            ELSE IF (canal(ja,jf) .EQ. 'MOZAIC') THEN
              WRITE(UNIT = nulou,FMT = 3013) cgrdmap(jf), nlumap(jf),
     $                                       nmapfl(jf), nmapvoi(jf)
            ELSE IF (canal(ja,jf) .EQ. 'INVERT') THEN
              WRITE(UNIT = nulou,FMT = 3014) cxordbf(jf)
              WRITE(UNIT = nulou,FMT = 3015) cyordbf(jf)
            ELSE IF (canal(ja,jf) .EQ. 'REVERSE') THEN
              WRITE(UNIT = nulou,FMT = 3016) cxordaf(jf)
              WRITE(UNIT = nulou,FMT = 3017) cyordaf(jf)
            ELSE IF (canal(ja,jf) .EQ. 'EXTRAP') THEN
              WRITE(UNIT = nulou,FMT = 3018) cextmet(jf), neighbor(jf)
              IF (cextmet(jf) .EQ. 'WEIGHT') THEN 
                  WRITE(UNIT = nulou,FMT = 3019) cgrdext(jf), 
     $                nluext(jf), nextfl(jf)
              ELSE IF (cextmet(jf) .EQ. 'NINENN') THEN 
                  WRITE(UNIT = nulou,FMT = 3038) niwtn(jf), nninnfl(jf)
              ENDIF
            ELSE IF (canal(ja,jf) .EQ. 'INTERP') THEN
              WRITE(UNIT = nulou,FMT = 3020) cintmet(jf), cgrdtyp(jf),
     $                                       cfldtyp(jf)
              IF (cintmet(jf) .EQ. 'SURFMESH') THEN 
                  WRITE(UNIT = nulou,FMT = 3021) 
     $                naismfl(jf), naismvoi(jf), niwtm(jf)
              ENDIF 
              IF (cintmet(jf) .EQ. 'GAUSSIAN') THEN 
                  WRITE(UNIT = nulou,FMT = 3021) 
     $                naisgfl(jf), naisgvoi(jf), niwtg(jf)
                  WRITE(UNIT = nulou,FMT = 3022) varmul(jf)
              ENDIF 
            ELSE IF (canal(ja,jf) .EQ. 'FILLING') THEN
              WRITE(UNIT = nulou,FMT = 3023) cfilfic(jf), nlufil(jf),
     $              cfilmet(jf)
              IF(cfilmet(jf)(1:6) .EQ. 'SMOSST')
     $            WRITE(UNIT = nulou,FMT = 3024) 
     $            nfcoast, cfldcor, nlucor
            ELSE IF (canal(ja,jf) .EQ. 'CONSERV') THEN            
              WRITE(UNIT = nulou,FMT = 3025) cconmet(jf)
            ELSE IF (canal(ja,jf) .EQ. 'REDGLO') THEN
              WRITE(UNIT = nulou,FMT = 3026) ntronca(jf), cmskrd(jf)
            ELSE IF (canal(ja,jf) .EQ. 'CORRECT') THEN
              WRITE(UNIT = nulou,FMT = 3027) cnamout(jf), afldcoef(jf)
              WRITE(UNIT = nulou,FMT = 3028) ncofld (jf)
              icofld = ncofld(jf)
              DO 330 jc = 1, icofld
                WRITE(UNIT = nulou,FMT = 3029) 
     $              ccofic(jc,jf),nludat(jc,jf)
                WRITE (UNIT = nulou,FMT = 3030) 
     $              ccofld(jc,jf), acocoef(jc,jf)
 330          CONTINUE
            ELSE IF (canal(ja,jf) .EQ. 'BLASOLD') THEN
              WRITE(UNIT = nulou,FMT = 3027) cnaminp(jf), afldcobo(jf)
              WRITE(UNIT = nulou,FMT = 3028) nbofld (jf)
              DO 340 jc = 1, nbofld(jf)
                WRITE (UNIT = nulou,FMT = 3030) 
     $              cbofld(jc,jf), abocoef (jc,jf)
 340          CONTINUE
            ELSE IF (canal(ja,jf) .EQ. 'BLASNEW') THEN
              WRITE(UNIT = nulou,FMT = 3027) cnamout(jf), afldcobn(jf)
              WRITE(UNIT = nulou,FMT = 3028) nbnfld (jf)
              DO 350 jc = 1, nbnfld(jf)
                WRITE (UNIT = nulou,FMT = 3030) 
     $              cbnfld(jc,jf), abncoef (jc,jf)
 350          CONTINUE
            ELSE IF (canal(ja,jf) .EQ. 'SUBGRID') THEN
              WRITE(UNIT = nulou,FMT = 3031) cgrdsub(jf), nlusub(jf),
     $              nsubfl(jf), nsubvoi(jf), ctypsub(jf)
              IF (ctypsub(jf) .EQ. 'NONSOLAR') THEN 
                  WRITE(UNIT = nulou,FMT = 3032) cdqdt(jf),
     $                cfldcoa(jf), cfldfin(jf)
                ELSE IF (ctypsub(jf) .EQ. 'SOLAR') THEN
                  WRITE(UNIT = nulou,FMT = 3033)
     $                cfldfin(jf), cfldcoa(jf)
              ENDIF
            ELSE IF (canal(ja,jf) .EQ. 'CHECKIN') THEN
                WRITE(UNIT = nulou,FMT = 3034)
            ELSE IF (canal(ja,jf) .EQ. 'CHECKOUT') THEN
                WRITE(UNIT = nulou,FMT = 3035) 
            ELSE IF (canal(ja,jf) .EQ. 'GLORED') THEN
              WRITE(UNIT = nulou,FMT = 3036) ntronca(jf) 
            ELSE IF (canal(ja,jf) .EQ. 'NOINTERP') THEN
                WRITE(UNIT = nulou,FMT = 3037)     
            ELSE 
              WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulou,FMT = *)
     $            ' Type of analysis not implemented yet '
              WRITE (UNIT = nulou,FMT = *) 
     $            ' The analysis required in OASIS is :'
              WRITE (UNIT = nulou,FMT = *) ' canal = ', canal(ja,jf)
              WRITE (UNIT = nulou,FMT = *) 
     $            ' with ja = ', ja, ' jf = ', jf
              WRITE (UNIT = nulou,FMT = *) ' '
              CALL HALTE ('STOP in inipar')
          ENDIF
 320    CONTINUE
      ENDIF
 310  CONTINUE
      ENDIF
C
C* Formats
C
 3001 FORMAT(//,15X,'  FIELD NUMBER ',I3)
 3002 FORMAT(15X,'  ************  ')
 3003 FORMAT(/,10X,'  Field parameters ')
 3004 FORMAT(10X,'  ****************  ',/)
 3005 FORMAT(/,10X,'  Input field symbolic name       = ',A8,
     $       /,10X,'  Output field symbolic name      = ',A8,
     $       /,10X,'  Field definition                = ',A30,
     $       /,10X,'  Field exchange frequency        = ',I8,
     $       /,10X,'  Model sequential index          = ',I2,
     $       /,10X,'  Model initialization flag       = ',I2,
     $       /,10X,'  Extra time step flag            = ',I2,
     $       /,10X,'  Field integral flag             = ',I2,
     $       /,10X,'  Field I/O status                = ',A8,
     $       /,10X,'  Number of basic operations      = ',I4,
     $       /,10X,'  Parallel decomposition strategy = ',A8,/)
 3006 FORMAT(/,10X,'  Input file name                 = ',A8,
     $       /,10X,'  Output file name                = ',A8,
     $       /,10X,'  Input logical unit              = ',I2,
     $       /,10X,'  Output logical unit             = ',I2,/)
 3007 FORMAT(/,10X,'  Number of longitudes for source grid = ',I4,
     $       /,10X,'  Number of latitudes for source grid  = ',I4,
     $       /,10X,'  Number of longitudes for target grid = ',I4,
     $       /,10X,'  Number of latitudes for target grid  = ',I4,
     $       /,10X,'  Source grid periodicity type is      = ',A8,
     $       /,10X,'  Number of overlapped grid points is  = ',I2,
     $       /,10X,'  Target grid periodicity type is      = ',A8,
     $       /,10X,'  Number of overlapped grid points is  = ',I2,/)
 3008 FORMAT(/,10X,'  Source longitude file string    = ',A8,
     $       /,10X,'  Source latitude file string     = ',A8,
     $       /,10X,'  Source mask file string         = ',A8,
     $       /,10X,'  Source surface file string      = ',A8,
     $       /,10X,'  Target longitude file string    = ',A8,
     $       /,10X,'  Target latitude file string     = ',A8,
     $       /,10X,'  Target mask file string         = ',A8,
     $       /,10X,'  Target surface file string      = ',A8,/)
 3009 FORMAT(/,10X,'  ANALYSIS PARAMETERS ')
 3010 FORMAT(10X,'  ******************* ',/)
 3011 FORMAT(/,5X,'  ANALYSIS number ',I2,' is ',A8,
     $       /,5X,'  ***************  ',/)
 3012 FORMAT(5X,' Value for masked points is        = ',E15.6)
 3013 FORMAT(5X,' Grid mapping file = ',A8,' linked to unit = ',I2,
     $     /,5X,' Dataset identificator number      = ',I2,
     $     /,5X,' Maximum number of neighbors is    = ',I4)
 3014 FORMAT(5X,' Source grid longitude order is    = ',A8)
 3015 FORMAT(5X,' Source grid latitude order is     = ',A8)
 3016 FORMAT(5X,' Target grid longitude order is    = ',A8)
 3017 FORMAT(5X,' Target grid latitude order is     = ',A8)
 3018 FORMAT(5X,' Extrapolation method is           = ',A8,
     $     /,5X,' Number of neighbors used is       = ',I2)
 3019 FORMAT(5X,' Extrapolation file = ',A8,' linked to unit = ',I2,
     $     /,5X,' Dataset identificator number      = ',I2)
 3020 FORMAT(5X,' Interpolation method is           = ',A8,
     $     /,5X,' Source grid type is               = ',A8,
     $     /,5X,' Field type is                     = ',A8)
 3021 FORMAT(5X,' Pointer for ANAIS storage is      = ',I2,
     $     /,5X,' Maximum number of neighbors is    = ',I4,
     $     /,5X,' Write/Read flag for weights is    = ',I2)
 3022 FORMAT(5X,' Variance multiplicator for ANAISG = ',E15.6)
 3023 FORMAT(5X,' Data to fill up field is in file  = ',A8,
     $     /,5X,' Connected to logical unit number  = ',I2,
     $     /,5X,' Filling method to blend field is  = ',A8)
 3024 FORMAT(5X,' Flag for coasts mismatch is       = ',I2, 
     $     /,5X,' Name for flux correction field is = ',A8,
     $     /,5X,' It is written on logical unit     = ',I2)
 3025 FORMAT(5X,' Conservation method for field is  = ',A8)
 3026 FORMAT(5X,' Half number of latitudes for gaussian grid is = ',I3,
     $     /,5X,' Extrapolation flag is             = ',A8)
 3027 FORMAT(5X,' Field ',A8,' is multiplied by Cst = ',E15.6)
 3028 FORMAT(5X,' It is combined with N fields    N = ',I2)
 3029 FORMAT(5X,' Data file = ',A8,' linked to unit = ',I2)
 3030 FORMAT(5X,'   With field ',A8,'   coefficient = ',E15.6)
 3031 FORMAT(5X,' Subgrid data file = ',A8,' linked to unit = ',I2,
     $     /,5X,' Dataset identificator number      = ',I2,
     $     /,5X,' Maximum number of neighbors is    = ',I4,
     $     /,5X,' Type of subgrid interpolation is  = ',A8)
 3032 FORMAT(5X,' Subgrid variability is restored with addition of',
     $       /,5X,A8,' x (',A8,' - ',A8,')')
 3033 FORMAT(5X,' Subgrid variability is restored multiplying by',
     $       /,5X,'( 1 - ',A8,') / ( 1 - ',A8,')')
 3034 FORMAT(5X,' Input field values are checked in ')
 3035 FORMAT(5X,' Output field values are checked out ')
 3036 FORMAT(5X,' Half number of latitudes for gaussian grid is = ',I3)
 3037 FORMAT(5X,' No interpolation for this field ')
 3038 FORMAT(5X,' Write/Read flag for weights is    = ',I2,
     $     /,5X,' Dataset identificator number      = ',I2)
C
C
C*    4. End of routine
C        --------------
C
      WRITE(UNIT = nulou,FMT = *) ' '
      WRITE(UNIT = nulou,FMT = *) 
     $    '          ---------- End of routine inipar ---------'
      CALL FLUSH (nulou)
      RETURN
C
C* Error branch output
C
 110  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *) 
     $    ' No active $JOBNAME data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 130  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *)
     $    ' No active $NBMODEL data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 160  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *) 
     $    ' No active $MACHINE data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 180  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *)
     $    ' No active $CHANNEL data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 191  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *)
     $    ' No active $RUNTIME data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 193  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *)
     $    ' No active $INIDATE data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 195  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *)
     $    ' No active $SEQMODE data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 197  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *)
     $    ' No active $MODINFO data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 199  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *)
     $    ' No active $NLOGPRT found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 210  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *) 
     $    ' No active $FIELDS data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
 230  CONTINUE
      WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulou,FMT = *) 
     $    ' No active $STRING data found in input file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    ' We STOP!!! Check the file namcouple'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL HALTE ('STOP in inipar')
      END


