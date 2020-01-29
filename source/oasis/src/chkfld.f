      SUBROUTINE chkfld (cdname, cdlab, pfild, kmask, psurf, 
     $    ksize, klon, kflag)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *chkfld*  - Perform basic checks on a given field array
C
C     Purpose:
C     -------
C     Calculate mean and extremum values of a real array
C
C**   Interface:
C     ---------
C       *CALL*  *chkfld (cdname, cdlab, pfild, kmask, psurf, ksize, klon,
C                        kflag)*
C
C     Input:
C     -----
C                cdname : symbolic name of the field to be checked
C                cdlab  : definition of the field to be checked
C                pfild  : field array (real 1D)
C                kmask  : associated mask array (integer 1D)
C                psurf  : surface array (real 1D)
C                ksize  : field array dimension
C                klon   : number of longitudes
C                kflag  : flag to compute integral of field
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
C     ssumr, rmaxim, rminim
C
C     Reference:
C     ---------
C     See OASIS manual (1995) 
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.1       L. Terray      96/09/25  created
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
      REAL pfild(ksize), psurf(ksize)
      INTEGER kmask(ksize)
      CHARACTER*8 cdname
      CHARACTER*32 cdlab
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
     $    '           ROUTINE chkfld  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = 1010) cdname, cdlab
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C
C* Initialization of counters and sums
C
      iterre = 0
      imer = 0
      zmean = 0.0
      zterre = 0.0
      zmer = 0.0
C
C* Formats
C
 1010 FORMAT(5X,' Field alias name is = ',A8,
     $     /,5X,' Its definition is   = ',A32)
C
C
C*    2. Basic checks
C        ------------
C
C* Calculate the mean; first the global one
C
      IF (ksize .GT. 0) THEN 
          zmean = ssumr (pfild, ksize) / float(ksize)
        ELSE
          WRITE(UNIT = nulou,FMT = 2010) ksize
          CALL HALTE('STOP in chkfld')
      ENDIF 
C
C* The other ones
C
      DO 210 jn = 1, ksize
        IF (kmask(jn) .EQ. 0) THEN
            imer = imer + 1
            zmer = zmer + pfild(jn)
          ELSE IF (kmask(jn) .EQ. 1) THEN
            iterre = iterre + 1
            zterre = zterre + pfild(jn)
        ENDIF 
 210  CONTINUE 
      IF (imer .GT. 0) THEN 
          zmer = zmer / float(imer)
        ELSE
         WRITE(UNIT = nulou,FMT = 2020) imer
         CALL HALTE('STOP in chkfld')
      ENDIF 
      IF (iterre .GT. 0) THEN 
          zterre = zterre / float(iterre)
        ELSE
         WRITE(UNIT = nulou,FMT = 2030) iterre
      ENDIF 
C
C* Calculate extrema
C
      iflag = 0
      zglomin = rminim (pfild, kmask, ksize, iglomin, iflag)
      zglomax = rmaxim (pfild, kmask, ksize, iglomax, iflag)
C* Indexes for minimum
      IF (mod(iglomin,klon) .EQ. 0) THEN
          ijglomin = iglomin / klon
          iiglomin = klon
        ELSE 
          ztmp = float(iglomin/klon)
          ijglomin = int(ztmp) + 1
          iiglomin = iglomin - (ijglomin -1) * klon
      ENDIF
C* Indexes for maximum
      IF (mod(iglomax,klon) .EQ. 0) THEN
          ijglomax = iglomax / klon
          iiglomax = klon
        ELSE 
          ztmp = float(iglomax/klon)
          ijglomax = int(ztmp) + 1
          iiglomax = iglomax - (ijglomax -1) * klon
      ENDIF
C
C* Land
C
      iflag = 1
      zsolmin = rminim (pfild, kmask, ksize, isolmin, iflag)
      zsolmax = rmaxim (pfild, kmask, ksize, isolmax, iflag)
C* Indexes for minimum
      IF (mod(isolmin,klon) .EQ. 0) THEN
          ijsolmin = isolmin / klon
          iisolmin = klon
        ELSE 
          ztmp = float(isolmin/klon)
          ijsolmin = int(ztmp) + 1
          iisolmin = isolmin - (ijsolmin -1) * klon
      ENDIF
C* Indexes for maximum
      IF (mod(isolmax,klon) .EQ. 0) THEN
          ijsolmax = isolmax / klon
          iisolmax = klon
        ELSE 
          ztmp = float(isolmax/klon)
          ijsolmax = int(ztmp) + 1
          iisolmax = isolmax - (ijsolmax -1) * klon
      ENDIF
C
C* Sea
C
      iflag = 2
      zmermin = rminim (pfild, kmask, ksize, imermin, iflag)
      zmermax = rmaxim (pfild, kmask, ksize, imermax, iflag)
C* Indexes for minimum
      IF (mod(imermin,klon) .EQ. 0) THEN
          ijmermin = imermin / klon
          iimermin = klon
        ELSE 
          ztmp = float(imermin/klon)
          ijmermin = int(ztmp) + 1
          iimermin = imermin - (ijmermin -1) * klon
      ENDIF
C* Indexes for maximum
      IF (mod(imermax,klon) .EQ. 0) THEN
          ijmermax = imermax / klon
          iimermax = klon
        ELSE 
          ztmp = float(imermax/klon)
          ijmermax = int(ztmp) + 1
          iimermax = imermax - (ijmermax -1) * klon
      ENDIF
C
C* Print results
C
      WRITE(UNIT = nulou,FMT = 2040) cdname
      WRITE(UNIT = nulou,FMT = 2050)
      WRITE(UNIT = nulou,FMT = 2060) zmean, 
     $    zglomax, iiglomax, ijglomax,
     $    zglomin, iiglomin, ijglomin,
     $    imer, zmer, 
     $    zmermax, iimermax, ijmermax,
     $    zmermin, iimermin, ijmermin,
     $    iterre, zterre, 
     $    zsolmax, iisolmax, ijsolmax,
     $    zsolmin, iisolmin, ijsolmin
C
C* Formats
C
 2010 FORMAT(' WARNING: total number of points .LE. 0 ')
 2020 FORMAT(' WARNING: number of sea points .LE. 0 ')
 2030 FORMAT(' WARNING: number of land points .LE. 0 ',
     $       /,' This must be a sea-world run ')
 2040 FORMAT(/,15X,'  Field checks: ',A8)
 2050 FORMAT(15X,'  ************  ',/)
 2060 FORMAT(/,10X,'  Global average     = ',E15.7,
     $       /,10X,'  Global maximum     = ',E15.7,
     $    '  Pt i-j = ',I3,2X,I3,
     $       /,10X,'  Global minimum     = ',E15.7,
     $    '  Pt i-j = ',I3,2X,I3,
     $       /,10X,'  Ocean grid points  = ',I6,
     $       /,10X,'  Ocean  average     = ',E15.7,
     $       /,10X,'  Ocean  maximum     = ',E15.7,
     $    '  Pt i-j = ',I3,2X,I3,
     $       /,10X,'  Ocean  minimum     = ',E15.7,
     $    '  Pt i-j = ',I3,2X,I3,
     $       /,10X,'  Land grid points   = ',I6,
     $       /,10X,'  Land  average      = ',E15.7,
     $       /,10X,'  Land  maximum      = ',E15.7,
     $    '  Pt i-j = ',I3,2X,I3,
     $       /,10X,'  Land  minimum      = ',E15.7,
     $    '  Pt i-j = '
     $    ,I3,2X,I3,/)
C
C
C*    3. Calculate integral of current field if needed
C        ------------------------------------------------------
C
      IF (kflag .EQ. 1) THEN
          zglo = 0.0
          zsea = 0.0
          zland = 0.0
C
C* The integral is performed on all and unmasked points
C
          DO 310 ji = 1, ksize
            zsea = zsea + pfild(ji) * psurf(ji) * float(1 - kmask(ji))
            zland = zland + pfild(ji) * psurf(ji) * float(kmask(ji))
            zglo = zglo + pfild(ji) * psurf(ji)
 310      CONTINUE 
          WRITE(UNIT = nulou,FMT = 3010)  zglo, zsea, zland       
      ENDIF
C
C* Formats
C 
 3010 FORMAT(/,10X,'  Earth integral       = ',E15.7,
     $       /,10X,'  Ocean integral       = ',E15.7,
     $       /,10X,'  Land  integral       = ',E15.7)
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          ---------- End of routine chkfld --------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


