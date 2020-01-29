      SUBROUTINE mozaic (pfldn, ksizn, pfldo, ksizo,
     $                   cdfic, kunit, cdgrd, knumb,
     $                   pwork, kwork, knbor, ldread)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *mozaic* - Mapping interpolation
C
C     Purpose:
C     -------
C     Do simple mapping interpolation from a source to a target grid
C
C**   Interface:
C     ---------
C       *CALL*  *mozaic (pfildn, ksizn, pfildo, ksizo, cdfic, kunit,
C                        cdgrd. knumb, pwork, kwork, knbor, ldread)*
C
C     Input:
C     -----
C                pfldo  : field on source grid (real 1D)
C                ksizn  : size of field array on target grid(integer)
C                ksizo  : size of field array on source grid(integer)
C                kunit  : logical unit numbers for mapping file (integer)
C                cdfic  : filename for mapping data (character)
C                cdgrd  : locator to read mapping data in cdfic (character)
C                knumb  : mapping dataset identity number (integer)
C                pwork  : temporary array to read mapping weights (real 2D)
C                kwork  : temporary array to read mapping array (integer 2D)
C                knbor  : maximum number of source grid neighbors with non zero
C                         intersection with a target grid-square (integer)
C                ldread : logical flag to read mapping data
C
C     Output:
C     ------
C                pfldn  : field on target grid (real 1D)
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     locrint
C
C     Reference:
C     ---------
C     See OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.0       L. Terray      96/02/01  created
C       2.1       L. Terray      96/08/05  modified: Add logical flag to
C                                          read adresses and weights
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
      REAL pfldn(ksizn), pfldo(ksizo), pwork(knbor,ksizn)
      INTEGER kwork(knbor,ksizn)
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
     $    '           ROUTINE mozaic  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' grid mapping interpolation'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* initialize error flag for I/O routine
C
      iflag = 0
C
C
C*    2. Read mapping data the first time
C        --------------------------------
C
      IF (ldread) THEN 
C
C* Initialize locators and array sizes
C
          WRITE(clweight,'(''WEIGHTS'',I1)') knumb
          WRITE(cladress,'(''ADRESSE'',I1)') knumb
          isize = ksizn * knbor
C
C* Adress of overlapped points on source grid
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
              CALL HALTE ('STOP in mozaic') 
          ENDIF
C
C* Weights of overlapped points on source grid
C
          WRITE(nulou,*) 'clweight=', clweight
          WRITE(nulou,*) 'isize=', isize
c         CALL FLUSH(nulou)
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
              CALL HALTE ('STOP in mozaic') 
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
      DO 310 ji = 1, ksizn
        zsum = 0.
        DO 320 jk = 1, knbor
          zsum = zsum + pwork(jk,ji) * pfldo(kwork(jk,ji))
 320    CONTINUE 
        pfldn(ji) = zsum
 310  CONTINUE
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine mozaic ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
