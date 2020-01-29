      SUBROUTINE namset (px1, py1, kmsk1, kvmsk1, kngx1, kngy1, 
     $                   cdper1, kper1,
     $                   px2, py2, kmsk2, kvmsk2, kngx2, kngy2, 
     $                   cdper2, kper2,
     $                   pr1to2, k1to2, kw1to2, psgr1, psgr2,
     $                   krdwt, kwlun, kvmsz2,
     $                   kmskz2, knumber)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *namset* - Initializations for the mesh naive method
C
C     Purpose:
C     -------
C     Read or calculate the interpolation weights used in Anaism
C
C**   Interface:
C     ---------
C       *CALL*  *namset(px1, py1, kmsk1, kvmsk1, kngx1, kngy1, cdper1, kper1,
C                       px2, py2, kmsk2, kvmsk2, kngx2, kngy2, cdper2, kper2,
C                       pr1to2, k1to2, kw1to2, psgr1, psgr2,
C                       krdwt, kwlun, kvmsz2,
C                       kmskz2, knumber)*
C     Input:
C     -----
C                kngx1   : number of longitudes for source grid
C                kngy1   : number of latitudes for source grid
C                px1     : longitudes for source grid (real 2D)
C                py1     : latitudes for source grid (real 2D)
C                kmsk1   : the mask for source grid (integer 2D)
C                kvmsk1  : the value of the mask for source grid
C                cdper1  : source grid periodicity
C                kper1   : number of overlapped points for source grid 
C                kngx2   : number of longitudes for target grid
C                kngy2   : number of latitudes for target grid
C                px2     : longitudes for target grid (real 2D)
C                py2     : latitudes for target grid (real 2D)
C                kmsk2   : the mask of target grid (integer 2D)
C                kvmsk2  : the value of the mask for target grid 
C                cdper2  : target grid periodicity
C                kper2   : number of overlapped points for target grid 
C                kw1to2  : maximum number of overlapped neighbors
C                krdwt   : read/write flag for the weights
C                kwlun   : logical unit for the weights
C                kvmsz2  : mask value for array kmskz2
C                knumber : flag to identify appropriate Anaism dataset
C
C     Output:
C     ------
C                pr1to2  : weights for Anaism interpolation (real 2D)
C                k1to2   : source grid neighbors adresses (integer 2D)
C                kmskz2  : number of source grid neighbors (integer 2D)
C                psgr1   : source grid square surfaces (real 2D)
C                psgr2   : target grid square surfaces (real 2D)
C                  
C
C     Workspace:
C     ---------
C     None
C
C     External:
C     --------
C     pcssph, ssumr, grstat, pmrhal, locwrint, locwrite, locread, locrint,
C     icoor, jcoor
C
C     References:
C     ----------
C     O. Thual, Simple ocean-atmosphere interpolation. 
C               Part A: The method, EPICOA 0629 (1992)
C               Part B: Software implementation, EPICOA 0630 (1992)
C     See also OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      ----------- 
C       1.1       O. Thual       93/04/15  created 
C       2.0       L. Terray      95/10/01  modified: new structure
C       2.3       S. Valcke      99/04/30  added: printing levels
C       2.3       L. Terray      99/09/15  changed periodicity variables
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
      REAL px1(kngx1,kngy1), py1(kngx1,kngy1), psgr1(kngx1,kngy1)
      REAL px2(kngx2,kngy2), py2(kngx2,kngy2), psgr2(kngx2,kngy2)
      REAL pr1to2(kw1to2,kngx2*kngy2)
      INTEGER kmsk1(kngx1,kngy1), kmsk2(kngx2,kngy2)
      INTEGER k1to2(kw1to2,kngx2*kngy2), kmskz2(kngx2,kngy2)
      CHARACTER*8 cdper1, cdper2
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*8 cladress, clweight, cloverlp
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initializations
C        ---------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           Routine namset  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Set up ANAIS-MESH interpolation'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Define global dimensions + other local variables
C
      ing1 = kngx1 * kngy1
      ing2 = kngx2 * kngy2
      ivmsk1 = kvmsk1
      ivmsk2 = kvmsk2
      iflag = 0
C
C* Define character strings to locate needed data
C
      WRITE(clweight,'(''WEIGHTS'',I1)') knumber
      WRITE(cladress,'(''ADRESSE'',I1)') knumber
      WRITE(cloverlp,'(''OVERLAP'',I1)') knumber
C
C
C*    2. Statistics of source grid
C        -------------------------
C
C* The following routines calculate some interesting info about the grids
C
C* Calculate surface elements (assume spherical and periodic grid)
C
      CALL pcssph (px1, py1, psgr1, kngx1, kngy1)
      zsum = ssumr (psgr1, ing1)
C
C* Printing on ANAIS output file
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulan,FMT = *) '           ANAIS output FILE '
          WRITE (UNIT = nulan,FMT = *) '           ***** ****** **** '
          WRITE (UNIT = nulan,FMT = *) ' '
          WRITE (UNIT = nulan,FMT = *) '            Routine namset '
          WRITE (UNIT = nulan,FMT = *) '            -------------- '
          WRITE (UNIT = nulan,FMT = *) ' '
          WRITE (UNIT = nulan,FMT = *) 
     $    '      Some statistics about the source grid '
          WRITE (UNIT = nulan,FMT = *) 
     $    '      ************************************* '
          WRITE (UNIT = nulan,FMT = *) ' '
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Sum of grid square surfaces = ', zsum
          WRITE (UNIT = nulan,FMT = *) 
     $    ' It must be equal to 4 x PI  = ', 16. * atan(1.) 
          WRITE (UNIT = nulan,FMT = *) ' '
      ENDIF
C
C* Calculate for ocean points (unmasked points) the following data :
C                ************
C           - total surface
C           - average inter-point distance
C           - inverse of the sum of surface elements squared
C
      CALL grstat (px1, py1, psgr1, kmsk1, kngx1, kngy1,
     $             zamsh1, zstot1, zhhi1, ivmsk1) 
C  
C* Printing
C 
      IF (nlogprt .GE. 2) THEN 
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Some info for ocean points only '
          WRITE (UNIT = nulan,FMT = *) 
     $    ' ******************************* '
          WRITE (UNIT = nulan,FMT = *) ' '
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Average mesh distance = ', sqrt(zamsh1)
          WRITE (UNIT = nulan,FMT = *) ' Total surface = ', zstot1
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Inverse of sum surf. elts. squared = ', zhhi1
      ENDIF
C
C
C*    3. Statistics of target grid
C        -------------------------
C
C* The following routines calculate some interesting info about the grids
C
C* Calculate surface elements (assume spherical and periodic grid)
C
      CALL pcssph (px2, py2, psgr2, kngx2, kngy2)
      zsum = ssumr (psgr2, ing2)
C
C* Printing
C
      IF (nlogprt .GE. 2) THEN  
          WRITE (UNIT = nulan,FMT = *) ' '
          WRITE (UNIT = nulan,FMT = *) 
     $    '      Some statistics about the target grid '
          WRITE (UNIT = nulan,FMT = *) 
     $    '      ************************************* '
          WRITE (UNIT = nulan,FMT = *) ' '
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Sum of grid square surfaces = ', zsum
          WRITE (UNIT = nulan,FMT = *) 
     $    ' It must be equal to 4 x PI  = ', 
     $                        16. * atan(1.) 
          WRITE (UNIT = nulan,FMT = *) ' '
      ENDIF
C
C* Calculate for ocean points (unmasked points) the following data :
C                ************
C           - total surface
C           - average inter-point distance
C           - inverse of the sum of surface elements squared
C
      CALL grstat (px2, py2, psgr2, kmsk2, kngx2, kngy2,
     $             zamsh2, zstot2, zhhi2, ivmsk2) 
C  
C* Printing
C
      IF (nlogprt .GE. 2) THEN   
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Some info for ocean points only '
          WRITE (UNIT = nulan,FMT = *) 
     $    ' ******************************* '
          WRITE (UNIT = nulan,FMT = *) ' '
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Average mesh distance = ', sqrt(zamsh2)
          WRITE (UNIT = nulan,FMT = *) ' Total surface = ', zstot2
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Inverse of sum surf. elts. squared = ', zhhi2
      ENDIF
C 
C
C*    4. Preparation of the neighbors search
C        -----------------------------------
C
C* Consistency checking
C
      IF (kw1to2 .GT. ing2) THEN 
          WRITE (UNIT = nulou,FMT = *) ' WARNING: kw1to2 .gt. ing2 '
          WRITE (UNIT = nulou,FMT = *) ' *******  ------      ---- '
          WRITE (UNIT = nulou,FMT = *) 
     $        ' kw1to2 = ',kw1to2,' ing2 = ',ing2
      ENDIF 
C
C
C*    5. Weights determination
C        ---------------------
C
C* Initialization for read write options
C 
      IF (krdwt .EQ. 1) THEN
C
C* Writing
C  -------
C
C* Calculate and write weights after checking numbers 
C 
C* Calculates weights 
C
          CALL pmrhal (pr1to2, k1to2, kw1to2,
     $                 px1, py1, kmsk1, kngx1, kngy1, cdper1, kper1,
     $                 px2, py2, kmsk2, kngx2, kngy2, cdper2, kper2,
     $                 ivmsk1, ivmsk2, kmskz2, kvmsz2)
C
C* Write  weights + other useful stuff
C
C* - Write main locator string + weights
C
          CALL locwrite (clweight, pr1to2, kw1to2*kngx2*kngy2, 
     $                   kwlun, iflag)
          IF (iflag .NE. 0) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in writing on UNIT = ', kwlun
              WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', clweight
              CALL HALTE ('Stop in namset')
          ENDIF
C 
C* - Write overlapped neighbors adresses
C
          CALL locwrint (cladress, k1to2, kw1to2*kngx2*kngy2,
     $                   kwlun, iflag)
          IF (iflag .NE. 0) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in writing on UNIT = ', kwlun
              WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', cladress
              CALL HALTE ('Stop in namset')
          ENDIF
C
C* - Write overlapped neighbors numbers
C
          CALL locwrint (cloverlp, kmskz2, kngx2*kngy2,
     $                   kwlun, iflag)
          IF (iflag .NE. 0) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in writing on UNIT = ', kwlun
              WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', cloverlp
              CALL HALTE ('Stop in namset')
          ENDIF
C
C* - Write  checkings numbers (grid parameters)
C
          WRITE(kwlun) kngx1, kngy1, kngx2, kngy2
C
C* Job is done
C
          IF (nlogprt .GE. 2) THEN  
              CALL prtout('Wrote weights on unit = ', kwlun, 1) 
          ENDIF
C
C* Reading
C  -------
C
        ELSE
C
C
C* Reads weights and checks  
C
C* - Read main locator string + weights
          CALL locread (clweight, pr1to2, kw1to2*kngx2*kngy2, 
     $                   kwlun, iflag)
          IF (iflag .NE. 0) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in reading on UNIT = ', kwlun
              WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', clweight
              CALL HALTE ('Stop in namset')
          ENDIF
C
C* - Read overlapped neighbors adresses
C
          CALL locrint (cladress, k1to2, kw1to2*kngx2*kngy2,
     $                   kwlun, iflag)
          IF (iflag .NE. 0) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in reading on UNIT = ', kwlun
              WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', cladress
              CALL HALTE ('Stop in namset')
          ENDIF
C
C* - Read overlapped neighbors numbers
C
          CALL locrint (cloverlp, kmskz2, kngx2*kngy2,
     $                   kwlun, iflag)
          IF (iflag .NE. 0) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            'Problem in reading on UNIT = ', kwlun
              WRITE (UNIT = nulou,FMT = *)
     $            'String locator is = ', cloverlp
              CALL HALTE ('Stop in namset')
          ENDIF
C
C* - Read  checkings numbers (grid parameters)
C
          READ(kwlun) ingx1, ingy1, ingx2, ingy2
C
C* Checks
C
          IF (ingx1 .NE. kngx1 .OR. ingy1 .NE. kngy1 .OR. 
     $        ingx2 .NE. kngx2 .OR. ingy2 .NE. kngy2) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            ' Inconsistency in mweights file'
              WRITE (UNIT = nulou,FMT = *) 
     $            'ingx1 = ',ingx1,'kngx1 = ',kngx1
              WRITE (UNIT = nulou,FMT = *) 
     $            'ingy1 = ',ingy1,'kngy1 = ',kngy1
              WRITE (UNIT = nulou,FMT = *) 
     $            'ingx2 = ',ingx2,'kngx2 = ',kngx2
              WRITE (UNIT = nulou,FMT = *) 
     $            'ingy2 = ',ingy2,'kngy2 = ',kngy2
              CALL HALTE ('Stop in namset')
          ENDIF 
C
C* Reading of weights done
C
          IF (nlogprt .GE. 2) THEN 
              CALL prtout
     $        ('Reading of weights done on unit = ', kwlun, 1) 
          ENDIF
      ENDIF
C
C
C*    6. Printing weights and adresses on ANAIS output file
C        --------------------------------------------------
C
      IF (nlogprt .GE. 2) THEN 
          WRITE (UNIT = nulan,FMT = *) ' '
          WRITE (UNIT = nulan,FMT = *) 
     $    ' Print weights and adresses of overlapped neighbors '
          WRITE (UNIT = nulan,FMT = *)
     $    ' ************************************************** '
          WRITE (UNIT = nulan,FMT = *) ' '
          DO 610 jj = 1, kngy2 
            DO 620 ji = 1, kngx2
              WRITE (UNIT = nulan,FMT = 6010) ji, jj
              WRITE (UNIT = nulan,FMT = 6020) px2(ji,jj), py2(ji,jj)
              WRITE (UNIT = nulan,FMT = 6030) kmskz2(ji,jj)
              IF (kmskz2(ji,jj) .NE. 0) THEN
                  IF (kmsk2(ji,jj) .EQ. 0) THEN 
                      DO 630 jn = 1, kmskz2(ji,jj)
                        iind = ji + kngx2 * (jj-1)
                        iadr = k1to2(jn,iind)
                        ix = icoor(iadr,kngx1)
                        iy = jcoor(iadr,kngx1)
                        WRITE(UNIT = nulan,FMT = 6040) 
     $                  jn, pr1to2(jn,iind)
                        WRITE(UNIT = nulan,FMT = 6050) 
     $                  ix, iy, px1(ix,iy), py1(ix,iy)
 630                  CONTINUE 
                  ELSE
                      WRITE(UNIT = nulan,FMT = 6060)
                  ENDIF 
              ENDIF
 620        CONTINUE
 610      CONTINUE 
          CALL FLUSH(nulan)
      ENDIF
C
C* Formats
C
 6010 FORMAT(/,' Target grid point --  i = ',I3,' j = ',I3)
 6020 FORMAT(' Point coordinates -- long = ',F9.4,' lat = ',F9.4)
 6030 FORMAT(3X,'Number of overlapped source grid squares = ',I3)
 6040 FORMAT(5X,' neighbor number = ',I3,' weight is = ',F6.4)
 6050 FORMAT(5X,' i = ',I3,' j = ',I3,' lon = ',F9.4,' lat = ',F9.4)
 6060 FORMAT(5X,' This is a continental point on the target grid ')
C
C
C*    7. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE(UNIT = nulou,FMT = *) ' '
          WRITE(UNIT = nulou,FMT = *) 
     $    '          --------- End of routine namset ---------'
          CALL FLUSH(nulou)
      ENDIF
      RETURN
      END




