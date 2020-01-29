      SUBROUTINE coasts (plon, plat, kmsk, kmskval, klon, klat, kmesh)
C
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *coasts* - locate coastal points where SST must be extrapolated
C                when using surfmesh interpolation.
C
C     Purpose:
C     -------
C     This routine is designed to detect coastal points where there is
C     mismatch between the atmosphere and ocean land sea masks and
C     where this mismatch could result in the atmosphere (undesirably)
C     'seeing' climatological SST's directly adjacent to ocean model SST's.
C     Where this situation arises, suitable neighbours from where
C     ocean model SST's may be extrapolated are located.
C     
C     NB: This is only for use with 'SURFMESH' interpolation of SST's.
C
C**   Interface:
C     ---------
C       *CALL*  *coasts*(plon, plat, kmsk, kmskval, klon, klat, kmesh)*
C
C     Input:
C     -----
C                plon    : target grid longitude array (real 2D) 
C                plat    : target grid latitude array (real 2D) 
C                klon    : number of longitude for target grid (integer)
C                klat    : number of latitude for target grid (integer)
C                kmsk    : target grid sea-land mask (integer 2D)
C                kmskval : sea-land mask value (integer)
C                kmesh   : overlap array (integer 2D)
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C         indx(4) - stores indices for neighbours
C
C     Externals:
C     ---------
C     None
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       1.1       R. Sutton      24/11/95  Original
C       2.0       L. Terray      26/12/95  Modified: to suit OASIS 2.0
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'coast.h'
      INCLUDE 'smooth.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C
      REAL plon(klon,klat), plat(klon,klat)
      INTEGER kmsk(klon,klat), kmesh(klon,klat)
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER indx(4)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE coasts  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *)
     $     ' locate any coastal points where SST must be extrapolated'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      ncoast = 0
C
C
C*    2. Locate coastal points and neighbours from where to do extrapolation 
C        -------------------------------------------------------------------
C
C* Loop between southern and northern boundaries   
C (Range of latitude is set by parameters defined in blkdata.)
C
      DO 210 jj = nsltb+1, nnltb-1
        DO 220 ji = 1, klon 
C
C* Find all grid squares which are ocean on the atmosphere grid but
C  have no ocean underlying them on the ocean grid
C
          IF (kmsk(ji,jj) .NE.  kmskval .AND. 
     $        kmesh(ji,jj) .EQ. 0) THEN
              is = jj-1
              in = jj+1
              iw = ji-1
              ie = ji+1
C
C* For an atmospheric domain that is periodic in longitude
C
              IF (iw .EQ. 0) iw = klon
              IF (ie .GT. klon) ie = ie - klon
C
C* For a domain that is not periodic in longitude
C          if (iw .eq. 0) iw=1
C          if (ie .gt. klon) ie=klon
C
C* Search for neighbours from which ocean model SST may be extrapolated.
C     
              inbor = 0
              IF (kmsk(iw,jj) .EQ. 0 .AND. kmesh(iw,jj) .NE. 0) THEN
                  inbor = inbor + 1
                  indx(inbor) = iw + (jj-1) * klon
              ENDIF
              IF (kmsk(ie,jj) .EQ. 0 .AND. kmesh(ie,jj) .NE. 0) THEN
                  inbor = inbor + 1
                  indx(inbor)= ie + (jj-1) * klon
              ENDIF
              IF (kmsk(ji,is) .EQ. 0 .AND. kmesh(ji,is) .NE. 0) THEN
                  inbor = inbor + 1
                  indx(inbor) = ji + (is-1) * klon
              ENDIF
              IF (kmsk(ji,in) .EQ. 0 .AND. kmesh(ji,in) .NE. 0) THEN
                  inbor = inbor + 1
                  indx(inbor) = ji + (in-1) * klon
              ENDIF
C
C* Store location of coastal point, number of suitable
C  neighbours and their locations.
C  Note that if there are no neighbours the point must
C  be an inland sea on the atmosphere grid for which 
C  we do not want to extrapolate the ocean SST's 
C
              IF (inbor .GT. 0) THEN
                  ncoast = ncoast + 1
                  npcoast(ncoast,1) = ji + (jj-1) * klon
                  npcoast(ncoast,2) = inbor
                  DO 230 jkbor = 1, inbor
                    npcoast(ncoast,2+jkbor) = indx(jkbor)
 230              CONTINUE
              ENDIF
          ENDIF
 220    CONTINUE
 210  CONTINUE
C
C
C*    3. Writing results
C        ---------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $    'Number of coastal ocean squares on the atmosphere grid '//
     $    'for which there'
          WRITE (UNIT = nulou,FMT = *) 
     $    'are no underlying ocean squares on the ocean grid = ',ncoast
          WRITE (UNIT = nulou,FMT = *) ' ' 
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    'Coordinates  nbor  Coordinates of neighbours '
          do 310 jn = 1, ncoast
            ii = icoor(npcoast(jn,1),klon)
            ij = jcoor(npcoast(jn,1),klon)
            WRITE (UNIT = nulou,FMT = 3100) 
     $       plon(ii,ij),plat(ii,ij),npcoast(jn,2),
     $       (plon(icoor(npcoast(jn,jkbor),klon),
     $            jcoor(npcoast(jn,jkbor),klon)),
     $        plat(icoor(npcoast(jn,jkbor),klon),   
     $            jcoor(npcoast(jn,jkbor),klon)), 
     $       jkbor=3,2+npcoast(jn,2))
 310      CONTINUE
      ENDIF
C
C* Formats
C
 3100 FORMAT (1X,'(',F5.1,',',F5.1,')  ',I1,
     $       4X,4('(',F5.1,',',F5.1,') '))
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           --------- End of routine coasts ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END


