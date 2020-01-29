      SUBROUTINE iniiof
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *iniiof*  - Open files
C
C     Purpose:
C     -------
C     Open files for grid related parameters, data transfer and 
C     auxilary outputs
C
C**   Interface:
C     ---------
C       *CALL*  *iniiof*
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
C     None
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
C       2.0beta   L. Terray      95/08/23  modified: new structure
C       2.0       L. Terray      96/02/01  modified: no opening of
C                                          CLIM trace file within oasis
C       2.1       L. Terray      96/08/26  modified: open data file for
C                                          SUBGRID Analysis
C       2.3       S. Valcke      99/03/30  modified: open data file for
C                                          NINENN analysis
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'parameter.h'
      INCLUDE 'unit.h'
      INCLUDE 'field.h'
      INCLUDE 'string.h'
      INCLUDE 'hardware.h'
      INCLUDE 'analysis.h'
      INCLUDE 'label.h'
      INCLUDE 'anais.h'
      INCLUDE 'extrapol.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*8 clfic
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Open files
C        ----------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE iniiof  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' open grid related files'
          WRITE (UNIT = nulou,FMT = *) ' '
          iost = 0
          WRITE (UNIT = nulou,FMT = *) '      open gcms grids file '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      OPEN (UNIT = nulgr,FILE = cgrdnam,STATUS='OLD',
     $     FORM ='UNFORMATTED',ERR = 110,IOSTAT = iost)
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) nulgr, cgrdnam
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
 110  CONTINUE
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $         ' ===>>>> : error opening grids file'
          WRITE (UNIT = nulou,FMT = *) 
     $         ' =======   =====               ===='
          WRITE (UNIT = nulou,FMT = *) 
     $         ' logical unit ',nulgr,' error number = ',
     $         iost
          WRITE (UNIT = nulou,FMT = *) 
     $         ' We STOP. Verify the file ', cgrdnam
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in iniiof')
      ENDIF
C
C* Masks file
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) '      open gcms masks file '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      OPEN (UNIT = nulma,FILE = cmsknam,STATUS='OLD',
     $      FORM ='UNFORMATTED',ERR = 120,IOSTAT = iost)
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) nulma, cmsknam
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
 120  CONTINUE
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $         ' ===>>>> : error opening masks file'
          WRITE (UNIT = nulou,FMT = *) 
     $         ' =======   =====               ===='
          WRITE (UNIT = nulou,FMT = *) 
     $         ' logical unit ',nulma,' error number = ',
     $                     iost
          WRITE (UNIT = nulou,FMT = *) 
     $         ' We STOP. Verify the file ', cmsknam
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE('STOP in iniiof')   
      ENDIF
C
C* Surfaces file
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) '      open gcms surfaces file'
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      OPEN (UNIT = nulsu,FILE = csurnam,STATUS='UNKNOWN',
     $      FORM ='UNFORMATTED',ERR = 130,IOSTAT = iost)
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) nulsu, csurnam
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
 130  CONTINUE
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $         ' ===>>>> : error opening surfaces file'
          WRITE (UNIT = nulou,FMT = *) 
     $         ' =======   =====                  ===='
          WRITE (UNIT = nulou,FMT = *) 
     $         ' logical unit ',nulsu,' error number = ',
     $         iost
          WRITE (UNIT = nulou,FMT = *) 
     $         ' We STOP. Verify the file ', csurnam
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in iniiof')   
      ENDIF
C
C* Reduced gaussian grid masks file
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $    '      open reduced gaussian grid masks file '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      OPEN (UNIT = nulrd,FILE = crednam,STATUS='UNKNOWN',
     $      FORM ='UNFORMATTED',ERR = 140,IOSTAT = iost)
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) nulrd, crednam
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
 140  CONTINUE
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $         ' ===>>>> : error opening red-grid masks file'
          WRITE (UNIT = nulou,FMT = *) 
     $         ' =======   =====                  ====='
          WRITE (UNIT = nulou,FMT = *) 
     $         ' logical unit ',nulrd,' error number = ',
     $                     iost
          WRITE (UNIT = nulou,FMT = *) 
     $         ' We STOP. Verify the file ', crednam
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE('STOP in iniiof')   
      ENDIF
C
C* Trace file for CLIM and PVM if used
C
      IF (nlogprt .GE. 1) THEN
          IF (cchan .EQ. 'CLIM') THEN
              WRITE (UNIT = nulou,FMT = *) 
     $        '      The CLIM trace file is opened'
              WRITE (UNIT = nulou,FMT = *) 
     $        '      within the CLIM program '
              WRITE (UNIT = nulou,FMT = *) 
     $        ' The name of the file is oasis.prt '
              WRITE (UNIT = nulou,FMT = *) 
     $        ' linked to logical unit = ', nultr
          ENDIF
      ENDIF
C
C* Formats
C
 1001 FORMAT(10X,' open unit = ',I3,'    file ',A8,' ok')
C
C
C*    2. Deal with transfer files
C        ------------------------
C
C* Files for input fields
C
      DO 210 jf = 1, nfield
        iunit = nluinp(jf)
        clfic = cficinp(jf)
        OPEN (UNIT = iunit,FILE = clfic,STATUS = 'UNKNOWN',
     $      FORM = 'UNFORMATTED',ERR = 220,IOSTAT = iost)
 220    CONTINUE
        IF (iost .ne. 0) THEN
            WRITE (UNIT = nulou,FMT = *) 
     $          ' ===>>>> : error opening transfer file'
            WRITE (UNIT = nulou,FMT = *) 
     $          ' =======   =====                  ===='
            WRITE (UNIT = nulou,FMT = *) 
     $          ' logical unit ',iunit,' error number = ',
     $          iost
            WRITE (UNIT = nulou,FMT = *) 
     $          ' We STOP. Verify the file ', clfic
            WRITE (UNIT = nulou,FMT = *) ' '
            CALL HALTE('STOP in iniiof')
        ENDIF
C
C* Files for output fields
C
        iunit = nluout(jf)
        clfic = cficout(jf)
        OPEN (UNIT = iunit,FILE = clfic,STATUS = 'UNKNOWN',
     $          FORM = 'UNFORMATTED',ERR = 230,IOSTAT = iost)
 230    CONTINUE
        IF (iost .ne. 0) THEN
            WRITE (UNIT = nulou,FMT = *) 
     $          ' ===>>>> : error opening transfer file'
            WRITE (UNIT = nulou,FMT = *) 
     $          ' =======   =====                  ===='
            WRITE (UNIT = nulou,FMT = *) 
     $          ' logical unit ',iunit,' error number = ',
     $          iost
            WRITE (UNIT = nulou,FMT = *) 
     $          ' We STOP. Verify the file ', clfic
            WRITE (UNIT = nulou,FMT = *) ' '
            CALL HALTE('STOP in iniiof')
        ENDIF
 210  CONTINUE
C
C
C*    3. Anais weights and output files and NINENN weight file
C        -----------------------------------------------------
C
C* Weights file for ANAISM
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' open interpolation related files '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '      need file with surface mesh weights'
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      OPEN (UNIT = nulcc,FILE = cwanaism, STATUS='UNKNOWN',
     $    FORM ='UNFORMATTED',ERR = 310,IOSTAT = iost)
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) nulcc, cwanaism
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
 310  CONTINUE
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $        ' ===>>>> : error opening surfmesh weights file'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' =======   =====                          ===='
          WRITE (UNIT = nulou,FMT = *) 
     $        ' logical unit ',nulcc,' error number = ',
     $        iost
          WRITE (UNIT = nulou,FMT = *) 
     $        ' We STOP. Verify the file', cwanaism
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in iniiof')   
      ENDIF
C* Weights file for ANAISG
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $    '      need file with curvilinear grid weights'
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      OPEN (UNIT = nulgg,FILE = cwanaisg,STATUS='UNKNOWN',
     $    FORM ='UNFORMATTED',ERR = 320,IOSTAT = iost)
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) nulgg, cwanaisg
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
 320  CONTINUE 
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $        ' ===>>>> : error opening curvilinear weights file'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' =======   =====                             ===='
          WRITE (UNIT = nulou,FMT = *) 
     $        ' logical unit ',nulgg,' error number = ',
     $        iost
          WRITE (UNIT = nulou,FMT = *) 
     $        ' We STOP. Verify the file', cwanaisg
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in iniiof')   
      ENDIF
C* ANAIS output FILE
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) '      open ANAIS output file'
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      OPEN (UNIT = nulan,FILE = cnaisout, STATUS='NEW',
     $    FORM ='FORMATTED',ERR = 330,IOSTAT = iost)
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) nulan, cnaisout
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
 330  CONTINUE
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $        ' ===>>>> : error opening ANAIS output file'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' =======   =====                      ===='
          WRITE (UNIT = nulou,FMT = *) 
     $        ' logical unit ',nulan,' error number = ',
     $        iost
          WRITE (UNIT = nulou,FMT = *) 
     $        ' We STOP. Verify the file', cnaisout
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in iniiof')   
      ENDIF
C
C* Weights file for NINENN
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $    '      need file with weights, address and iteration number'
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      OPEN (UNIT = nulgn,FILE = cwninenn,STATUS='UNKNOWN',
     $    FORM ='UNFORMATTED',ERR = 340,IOSTAT = iost)
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = 1001) nulgn, cwninenn
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
 340  CONTINUE 
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $        ' ===>>>> : error opening weight file'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' =======   =====                ===='
          WRITE (UNIT = nulou,FMT = *) 
     $        ' logical unit ',nulgn,' error number = ',
     $        iost
          WRITE (UNIT = nulou,FMT = *) 
     $        ' We STOP. Verify the file', cwninenn
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in iniiof')   
      ENDIF
C
C*    4. Deal with data files used in analysis routines
C        ----------------------------------------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' open analysis related files '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      DO 410 jf = 1, nfield
        DO 420 ja = 1, ntrans(jf)
          IF(canal(ja,jf) .EQ. 'CORRECT') THEN
              DO 430 jc = 1, ncofld(jf)
                iunit = nludat(jc,jf)
                clfic = ccofic(jc,jf)
                OPEN(UNIT = iunit,FILE = clfic, STATUS='UNKNOWN',
     $               FORM ='UNFORMATTED',ERR = 440,IOSTAT = iost)
                IF (nlogprt .GE. 1) THEN
                    WRITE (UNIT = nulou,FMT = 1001) iunit, clfic
                    WRITE (UNIT = nulou,FMT = *) ' '
                ENDIF
 440            IF (iost .ne. 0) THEN
                    WRITE (UNIT = nulou,FMT = *) 
     $                  ' ===>>>> : error opening data file'
                    WRITE (UNIT = nulou,FMT = *) 
     $                  ' =======   =====              ===='
                    WRITE (UNIT = nulou,FMT = *) 
     $                  ' logical unit ',iunit,' error number = ',
     $                  iost
                    WRITE (UNIT = nulou,FMT = *) 
     $                  ' We STOP. Verify the file ', clfic
                    WRITE (UNIT = nulou,FMT = *) ' '
                    CALL HALTE('STOP in iniiof')
                ENDIF
 430          CONTINUE 
            ELSE IF(canal(ja,jf) .EQ. 'MOZAIC') THEN
              iunit = nlumap(jf)
              clfic = cgrdmap(jf)
              OPEN(UNIT = iunit,FILE = clfic, STATUS='UNKNOWN',
     $            FORM ='UNFORMATTED',ERR = 450,IOSTAT = iost)
              IF (nlogprt .GE. 1) THEN
                  WRITE (UNIT = nulou,FMT = 1001) iunit, clfic
                  WRITE (UNIT = nulou,FMT = *) ' '
              ENDIF
 450          IF (iost .ne. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' ===>>>> : error opening mapping file'
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' =======   =====                 ===='
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' logical unit ',iunit,' error number = ',
     $                iost
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' We STOP. Verify the file ', clfic
                  WRITE (UNIT = nulou,FMT = *) ' '
                  CALL HALTE('STOP in iniiof')
              ENDIF
            ELSE IF(canal(ja,jf) .EQ. 'FILLING') THEN
              iunit = nlufil(jf)
              clfic = cfilfic(jf)
              OPEN(UNIT = iunit,FILE = clfic, STATUS='UNKNOWN',
     $            FORM ='UNFORMATTED',ERR = 460,IOSTAT = iost)
              IF (nlogprt .GE. 1) THEN
                  WRITE (UNIT = nulou,FMT = 1001) iunit, clfic
                  WRITE (UNIT = nulou,FMT = *) ' '
              ENDIF
 460          IF (iost .ne. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' ===>>>> : error opening filling data file'
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' =======   =====         ======= ===='
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' logical unit ',iunit,' error number = ',
     $                iost
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' We STOP. Verify the file ', clfic
                  WRITE (UNIT = nulou,FMT = *) ' '
                  CALL HALTE('STOP in iniiof')
              ENDIF
            ELSE IF(canal(ja,jf) .EQ. 'SUBGRID') THEN
              iunit = nlusub(jf)
              clfic = cgrdsub(jf)
              OPEN(UNIT = iunit,FILE = clfic, STATUS='UNKNOWN',
     $            FORM ='UNFORMATTED',ERR = 470,IOSTAT = iost)
              IF (nlogprt .GE. 1) THEN
                  WRITE (UNIT = nulou,FMT = 1001) iunit, clfic
                  WRITE (UNIT = nulou,FMT = *) ' '
              ENDIF
 470          IF (iost .ne. 0) THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' ===>>>> : error opening subgrid data file'
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' =======   =====         ======= ===='
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' logical unit ',iunit,' error number = ',
     $                iost
                  WRITE (UNIT = nulou,FMT = *) 
     $                ' We STOP. Verify the file ', clfic
                  WRITE (UNIT = nulou,FMT = *) ' '
                  CALL HALTE('STOP in iniiof')
              ENDIF
          ENDIF
 420    CONTINUE 
 410  CONTINUE 
C
C
C*    5. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE(UNIT = nulou,FMT = *) ' '
          WRITE(UNIT = nulou,FMT = *) 
     $    '          ---------- End of routine iniiof ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
