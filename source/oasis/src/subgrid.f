      SUBROUTINE subgrid (pfldn, pfldo, ksizn, ksizo,
     $                    pcoar, pfine, pdqdt,
     $                    cdfic, kunit, knumb, cdname,
     $                    pwork, kwork, knbor, ldread, cdtype)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *subgrid* - submesh variabiity
C
C     Purpose:
C     -------
C     Interpolate with subgrid linear technique. This is  rigorously
C     conservative if the models exchange fields at every timestep
C     and if sea-land mismatch is accounted for.
C
C**   Interface:
C     ---------
C       *CALL*  *subgrid (pfild, ksize, pcoar, pfine, pdqdt)*
C
C     Input:
C     -----
C                pfldo  : initial field on source grid (real 1D)
C                ksizn  : size of final field array (integer)
C                ksizo  : size of initial field array (integer)
C                pcoar  : coarse grid additional field (real 1D)
C                pfine  : fine grid additional field (real 1D)
C                pdqdt  : coarse grid coupling ratio (real 1D)
C                kunit  : logical unit numbers for subgrid file (integer)
C                cdfic  : filename for subgrid data (character)
C                knumb  : subgrid dataset identity number (integer)
C                cdname : name of final field on target grid (character)
C                pwork  : temporary array to read subgrid weights (real 1D)
C                kwork  : temporary array to read subgrid array (integer 1D)
C                knbor  : maximum number of source grid neighbors with non zero
C                         intersection with a target grid-square (integer)
C                         The source grid is here the coarse grid while the
C                         target grid is the fine one.
C                ldread : logical flag to read subgrid data (logical)
C                cdtype : type of subgrid interpolation (character)
C
C     Output:
C     ------
C                pfldn  : final field on target grid (real 1D)
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
C       2.0       L. Terray      96/02/01  created
C       2.1       L. Terray      96/08/05  modified: new structure
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
      REAL pfldn(ksizn), pfldo(ksizo)
      REAL pcoar(ksizo), pfine(ksizn), pdqdt(ksizo)
      REAL pwork(knbor,ksizn)
      INTEGER kwork(knbor,ksizn)
      CHARACTER*8 cdfic, cdname, cdtype
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
     $    '           ROUTINE subgrid  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Linear subgrid interpolation'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* initialize error flag for I/O routine
C
      iflag = 0
C
C
C*    2. Read subgrid data the first time
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
     $            ('WARNING: problem in reading 
     $            subgrid data for field',
     $            cdname, 1)
              CALL prcout
     $            ('Could not get adress array', cladress, 1)
              CALL prtout
     $            ('Error reading logical unit', kunit, 1)
              CALL prcout
     $            ('It is connected to file', cdfic, 1)
              CALL HALTE ('STOP in subgrid') 
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
     $            ('WARNING: problem in reading 
     $            subgrid data for field',
     $            cdname, 1)
              CALL prcout
     $            ('Could not get weight array', clweight, 1)
              CALL prtout
     $            ('Error reading logical unit', kunit, 1)
              CALL prcout
     $            ('It is connected to file', cdfic, 1)
              CALL HALTE ('STOP in subgrid') 
          ENDIF
          ldread = .FALSE. 
      ENDIF
C
C
C*    3. Modify main field according to type of subgrid interpolation
C        ------------------------------------------------------------
C* Case of non solar flux
C
      IF (cdtype .EQ. 'NONSOLAR') THEN
C
C* Loop on all target points
C
          DO 310 ji = 1, ksizn
            zsum = 0.0
C
C* Loop on active neighbors
C
            DO 320 jk = 1, knbor
              zsum = zsum + pwork(jk,ji) *
     $            ( pfldo(kwork(jk,ji)) + pdqdt(kwork(jk,ji)) 
     $            * ( pfine(ji) - pcoar(kwork(jk,ji)) ) )
 320        CONTINUE 
            pfldn(ji) = zsum
 310      CONTINUE
C
C* Case of solar flux
C
        ELSE IF (cdtype .EQ. 'SOLAR') THEN
          DO 330 ji = 1, ksizn
            zsum = 0.0
C
C* Loop on active neighbors
C
            DO 340 jk = 1, knbor
              zsum = zsum + pwork(jk,ji) * pfldo(kwork(jk,ji)) *
     $            ( 1. - pfine(ji)) / ( 1. - pcoar(kwork(jk,ji)))
 340        CONTINUE
            pfldn(ji) = zsum
 330      CONTINUE
      ENDIF 
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine subgrid ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
