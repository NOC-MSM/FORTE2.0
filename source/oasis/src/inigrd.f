      SUBROUTINE inigrd
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *inigrd*  - Initialize gcm's grids
C
C     Purpose:
C     -------
C     Get atmospheric and oceanic grids and masks as well as scale factors
C     for interpolation purposes.
C
C     ***NOTE***
C     ----------
C     Grids, masks and scale factors must be ordered with the OASIS convention.
C     They go from South to North and from Greenwhich to the east.
C     EXCEPTION: array maskr (mask for reduced gaussian grid) must
C     be ordered as in the atmospheric code (North to South in 
C     Arpege case).
C
C**   Interface:
C     ---------
C       *CALL*  *inigrd*
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
C     locread, locrint
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
C       2.0       L. Terray      95/08/31  modified : new structure
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'parameter.h'
      INCLUDE 'memory.h'
      INCLUDE 'field.h'
      INCLUDE 'string.h'
      INCLUDE 'label.h'
      INCLUDE 'unit.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*8 clwork
      CHARACTER*8 clstrg
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE inigrd  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' get gcms grids, masks and surfaces'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Initializa error codes
C
      infos = 0
      iflag = 0
C
C
C*    2. Get gcm's grids, masks and mesh surfaces
C        ----------------------------------------
C
      DO 200 jf = 1, nfield
C
C* Print
C
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read source grid data for field number =',jf,2)
        ENDIF
C
C* Assign local variables to array sizes and pointers
C
        iadrold = nadrold(jf)
        isizold = nsizold(jf)
        iadrnew = nadrnew(jf)
        isiznew = nsiznew(jf)
C
C* Get the right locator string for initial grids files
C
        clwork = cficbf(jf)
        icount = ilenstr(clwork,jpeight)
        clstrg = clwork(1:icount)//cglonsuf
C
C* Read longitudes of initial grid associated to field jf
C
        iunit = nulgr
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read grid longitudes for field number =',jf,1)
        ENDIF
        CALL locread (clstrg, xgrold(iadrold), isizold, iunit, iflag)
        IF (iflag .NE. 0) THEN
            CALL prtout('Problem in reading unit =',iunit,2)
            CALL prcout
     $          ('Problem with array linked to string =',clstrg,1)
            infos = infos + iflag
        ENDIF 
        clstrg = clwork(1:icount)//cglatsuf
C
C* Read latitudes of initial grid associated to field jf
C
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read grid latitudes for field number =',jf,1)
        ENDIF
        CALL locread(clstrg, ygrold(iadrold), isizold, iunit, iflag)
        IF (iflag .NE. 0) THEN
            CALL prtout('Problem in reading unit =',iunit,2)
            CALL prcout
     $          ('Problem with array linked to string =',clstrg,1)
            infos = infos + iflag
        ENDIF
        clstrg = clwork(1:icount)//cmsksuf
C
C* Read initial mask associated to field jf
C
        iunit = nulma
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read grid mask for field number =',jf,1)
        ENDIF
        CALL locrint (clstrg, mskold(iadrold), isizold, iunit, iflag)
        IF (iflag .NE. 0) THEN
            CALL prtout('Problem in reading unit =',iunit,2)
            CALL prcout
     $          ('Problem with array linked to string =',clstrg,1)
            infos = infos + iflag
        ENDIF
        clstrg = clwork(1:icount)//csursuf
C
C* Read initial mesh surface associated to field jf
C
        iunit = nulsu
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read grid surface for field number =',jf,1)
        ENDIF
        CALL locread (clstrg, surold(iadrold), isizold, iunit, iflag)
        IF (iflag .NE. 0) THEN
            CALL prtout('Problem in reading unit =',iunit,2)
            CALL prcout
     $          ('Problem with array linked to string =',clstrg,1)
            infos = infos + iflag
        ENDIF
C
C* Get the right locator string for final grids files
C
        clwork = cficaf(jf)
        icount = ilenstr(clwork,jpeight)
        clstrg = clwork(1:icount)//cglonsuf
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read target grid data for field number =',jf,2)
        ENDIF
C
C* Read longitudes of final grid associated to field jf
C
        iunit = nulgr
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read grid longitudes for field number =',jf,1)
        ENDIF
        CALL locread (clstrg, xgrnew(iadrnew), isiznew, iunit, iflag)
        IF (iflag .NE. 0) THEN
            CALL prtout('Problem in reading unit =',iunit,2)
            CALL prcout
     $          ('Problem with array linked to string =',clstrg,1)
            infos = infos + iflag
        ENDIF
        clstrg = clwork(1:icount)//cglatsuf
C
C* Read latitudes of final grid associated to field jf
C
        iunit = nulgr
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read grid latitudes for field number =',jf,1)
        ENDIF
        CALL locread (clstrg, ygrnew(iadrnew), isiznew, iunit, iflag)
        IF (iflag .NE. 0) THEN
            CALL prtout('Problem in reading unit =',nulgr,2)
            CALL prcout
     $          ('Problem with array linked to string =',clstrg,1)
            infos = infos + iflag
        ENDIF
        clstrg = clwork(1:icount)//cmsksuf
C
C* Read final mask associated to field jf
C
        iunit = nulma
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read grid mask for field number =',jf,1)
        ENDIF
        CALL locrint (clstrg, msknew(iadrnew), isiznew, iunit, iflag)
        IF (iflag .NE. 0) THEN
            CALL prtout('Problem in reading unit =',iunit,2)
            CALL prcout
     $          ('Problem with array linked to string =',clstrg,1)
            infos = infos + iflag
        ENDIF
        clstrg = clwork(1:icount)//csursuf
C
C* Read final mesh surface associated to field jf
C
        iunit = nulsu
        IF (nlogprt .GE. 2) THEN
            CALL prtout('Read grid surface for field number =',jf,1)
        ENDIF
        CALL locread (clstrg, surnew(iadrnew), isiznew, iunit, iflag)
        IF (iflag .NE. 0) THEN
            CALL prtout('Problem in reading unit =',iunit,2)
            CALL prcout
     $          ('Problem with array linked to string =',clstrg,1)
            infos = infos + iflag
        ENDIF
 200  CONTINUE
C
C* Finish up if problem in reading
C
      IF (infos .NE. 0) CALL HALTE('STOP in inigrd')
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine inigrd ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


