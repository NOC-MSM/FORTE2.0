      SUBROUTINE correct (pfild, ksize, pmcoef, kaux, pacoef, pwork,
     $                    kunit, cdfic, cdfld)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *correct* - Flux correction routine
C
C     Purpose:
C     -------
C     Use external data to modify coupling fields
C
C**   Interface:
C     ---------
C       *CALL*  *correct (pfild, ksize, pmcoef, kaux, pacoef, pwork,
C                         kunit, cdfic, cdfld)*
C
C     Input:
C     -----
C                pfild  : field on source grid (real 1D)
C                ksize  : size of field array (integer)
C                pmcoef : main field coefficient (real)
C                kaux   : number of auxilary fields (integer)
C                pacoef : auxilary field coefficients (real 1D)
C                pwork  : temporary array to read auxilary fields (real 1D)
C                kunit  : logical unit numbers for data files (INTEGER 1D)
C                cdfic  : filenames for external data (character 1D)
C                cdfld  : auxilary field names (character 1D) 
C
C     Output:
C     ------
C                pfild  : corrected field on source grid  (real 1D)
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
C       2.0       L. Terray      95/10/01  created
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      REAL pfild(ksize), pwork(ksize), pacoef(kaux)
      INTEGER kunit(kaux)
      CHARACTER*8 cdfic(kaux), cdfld(kaux)
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*8 clfic
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE correct  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Flux correction with external data'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* initialize error flag for I/O routine
C
      iflag = 0
C
C
C*    2. Multiply main field by its coefficient
C        --------------------------------------
C
      DO 210 ji = 1, ksize
        pfild(ji) = pfild(ji) * pmcoef
 210  CONTINUE
C
C
C*    3. Read external data and add it to main field
C        -------------------------------------------
C
C* Loop on additional fields
C
      DO 310 ji = 1, kaux
C
C* Flush data files
C
        iunit = kunit(ji)
        clfic = cdfic(ji)
        CLOSE(UNIT = iunit, ERR = 3010, IOSTAT = ios)
        WRITE(UNIT = nulou, FMT = 3100) iunit, clfic
 3010   CONTINUE
        IF (ios .NE. 0) THEN
            CALL prtout('WARNING: problem in closing unit',
     $          iunit, 1)
            CALL prtout('Error message number is = ', ios, 1)
            CALL HALTE('STOP in correct')
        ENDIF 
        OPEN(UNIT=iunit,FILE=clfic,FORM='UNFORMATTED',
     $      STATUS='UNKNOWN',ERR = 3020, IOSTAT = ios)
        WRITE(UNIT = nulou, FMT = 3200) iunit, clfic
 3020   CONTINUE
        IF (ios .NE. 0) THEN
            CALL prtout('WARNING: problem in connecting unit',
     $          iunit, 1)
            CALL prtout('Error message number is = ', ios, 1)
            CALL HALTE('STOP in correct')
        ENDIF 
C
C* Reading of the auxilary fields
C
        CALL locread (cdfld(ji), pwork, ksize, kunit(ji), iflag)
C
C* Checking
C
        IF (iflag .NE. 0) THEN 
            CALL prcout
     $          ('WARNING: problem in reading field',
     $          cdfld(ji), 1)
            CALL prtout
     $          ('Error reading logical unit', kunit(ji), 1)
            CALL prcout
     $          ('It is connected to file',cdfic(ji),1)
            WRITE(UNIT = nulou,FMT = *) 
     $          ' If very first iteration and parallel simulation '
            WRITE(UNIT = nulou,FMT = *)
     $          ' or sequential one starting with atmosphere '
            WRITE(UNIT = nulou,FMT = *)
     $          ' No file present !! It is normal !! '

        ENDIF
C
C* Add to original field
C
        DO 320 jj = 1, ksize
          pfild(jj) = pfild(jj) + pacoef(ji) * pwork(jj)
 320    CONTINUE 
 310  CONTINUE
C
C* Formats
C
 3100 FORMAT(/,5X,' Unit ',I2,' has been disconnected from file ',A8)
 3200 FORMAT(/,5X,' Unit ',I2,' has been reconnected to file ',A8) 
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine correct ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
