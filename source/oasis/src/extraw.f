      SUBROUTINE extraw (pfild, kmask, ksize,
     $                   cdfic, kunit, cdgrd, knumb,
     $                   pwork, kwork, knbor, ldread)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *extraw* - Weighted extrapolation
C
C     Purpose:
C     -------
C     Do simple weighted extrapolation to replace land point values
C
C**   Interface:
C     ---------
C       *CALL*  *extraw (pfild, kmask, ksize, cdfic, kunit,
C                        cdgrd. knumb, pwork, kwork, knbor, ldread)*
C
C     Input:
C     -----
C                pfild  : field on source grid (real 1D)
C                kmask  : land-sea mask on source grid (integer 1D)
C                ksize  : size of field array on source grid (integer)
C                kunit  : logical unit number for extrapolation file (integer)
C                cdfic  : filename for extrapolation data (character)
C                cdgrd  : locator to read  data in cdfic (character)
C                knumb  : extrapolation dataset identity number (integer)
C                pwork  : temporary array to read weights (real 2D)
C                kwork  : temporary array to read adresses (integer 2D)
C                knbor  : maximum number of source grid neighbors with non zero
C                         weights  with a source grid-square (integer)
C                ldread : logical flag to read extrapolation data
C
C     Output:
C     ------
C                pfild  : field on source grid (real 1D)
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     locrint, locread
C
C     Reference:
C     ---------
C     See OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.2       L. Terray      97/12/16  created
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
      REAL pfild(ksize), pwork(knbor,ksize)
      INTEGER kwork(knbor,ksize), kmask(ksize)
      CHARACTER*8 cdfic, cdgrd
      LOGICAL ldread
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*8 clweight, cladress
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
     $    '           ROUTINE extraw  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' weighted extrapolation'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* initialize error flag for I/O routine
C
      iflag = 0
C
C
C*    2. Read extrapolation data the first time
C        --------------------------------------
C
      IF (ldread) THEN 
C
C* Initialize locators and array sizes
C
          WRITE(clweight,'(''WEIGHTS'',I1)') knumb
          WRITE(cladress,'(''ADRESSE'',I1)') knumb
          isize = ksize * knbor
C
C* Adress of connected points on source grid
C
          CALL locrint (cladress, kwork, isize, kunit, iflag)
C
C* Checking
C
          IF (iflag .NE. 0) THEN 
              CALL prcout
     $            ('WARNING: problem in reading mapping data',
     $            cdgrd, 1)
              CALL prcout
     $            ('Could not get adress array', cladress, 1)
              CALL prtout
     $            ('Error reading logical unit', kunit, 1)
              CALL prcout
     $            ('It is connected to file', cdfic, 1)
              CALL HALTE ('STOP in extraw') 
          ENDIF
C
C* Weights of overlapped points on source grid
C
          CALL locread (clweight, pwork, isize, kunit, iflag)
C
C* Checking
C
          IF (iflag .NE. 0) THEN 
              CALL prcout
     $            ('WARNING: problem in reading mapping data',
     $            cdgrd, 1)
              CALL prcout
     $            ('Could not get weight array', clweight, 1)
              CALL prtout
     $            ('Error reading logical unit', kunit, 1)
              CALL prcout
     $            ('It is connected to file', cdfic, 1)
              CALL HALTE ('STOP in extraw') 
          ENDIF
          ldread = .FALSE. 
      ENDIF 
C
C
C*    3. Do the mapping interpolation
C        ----------------------------
C
C* Loop on all the target grid points
C
      DO 310 ji = 1, ksize
C* If it is a land point, we extrapolate
        IF (kmask(ji) .EQ. 1) THEN 
            zsum = 0.
            DO 320 jk = 1, knbor
              zsum = zsum + pwork(jk,ji) * pfild(kwork(jk,ji))
 320        CONTINUE 
            pfild(ji) = zsum
        ENDIF 
 310  CONTINUE
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine extraw ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
